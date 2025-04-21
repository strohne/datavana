#
# Functions for low level API access to Epigraf
#


#' Save API connection settings to environment variables.
#'
#' @param apiserver URL of the Epigraf server (including https-protocol)
#' @param apitoken Access token. If NULL, you will be asked to enter the token.
#' @param verbose Show debug messages and the built URLs
#' @export
api_setup <- function(apiserver, apitoken=NULL, verbose=F) {
  if (missing(apitoken)) {
    apitoken <- readline(prompt="Please, enter your access token:")
  }
  settings <- as.list(environment())
  settings <- setNames(settings, paste0("epi_",names(settings)))
  do.call(Sys.setenv, settings)
}

#' Set silent mode
#'
#' In silent mode, all user prompts are automatically confirmed.
#' Be careful, this will skip the prompt to confirm operations
#' on the live server.
#'
#' @param silent Boolen
#' @export
api_silent <- function(silent = F) {
  Sys.setenv("epi_silent" = silent)
}

#' Build base URL
#'
#' @param endpoint The endpoint, e.g. articles/import
#' @param query Query parameters for the endpoint
#' @param database The database name
#' @param extension Extension added to the URL path, defaults to json.
#' @export
api_buildurl <- function(endpoint, query=NA, database=NA, extension="json") {

  # Get server and token from the global settings
  server <- Sys.getenv("epi_apiserver")
  token <- Sys.getenv("epi_apitoken")
  verbose <- Sys.getenv("epi_verbose") == "TRUE"

  url <- httr::parse_url(server)
  url$query$token <- token

  # Add query parameters
  if (!all(is.na(query)))  {
    url$query = merge_lists(list(url$query,as.list(query)))
  }

  if (!str_starts(endpoint,"/")) {
    endpoint <-  paste0("/", endpoint)
  }


  if (is.na(extension)) {
    extension = ""
  }
  else if (!is.na(extension) && !str_starts(extension,"\\.")) {
    extension <-  paste0(".", extension)
  }


  if (!is.na(database)) {
    url$path <- paste0("epi/",database,endpoint,extension)
  } else {
    url$path <- paste0(endpoint, extension)
  }

  url <- httr::build_url(url)
  if (verbose == "TRUE") {
    print(url)
  }

  return(url)

}

#' Create and execute a job
#'
#' @param endpoint The endpoint supporting job creation
#' @param params Query parameters
#' @param database The selected database
#' @param payload The data posted to the job endpoint
#' @return void
#' @export
api_job_create <- function(endpoint, params, database, payload=NULL) {
  verbose <- Sys.getenv("epi_verbose") == "TRUE"
  server <- Sys.getenv("epi_apiserver")

  print (paste0("Creating job on server ", server))
  if (!isLocalServer(server)) {
    confirmAction()
  }

  # 1. Create job
  url = api_buildurl(endpoint, params, database)

  if (verbose)  {
    resp <- POST(url, body=payload, encode="json", set_cookies(XDEBUG_SESSION="XDEBUG_ECLIPSE"))
  } else {
    resp <- POST(url, body=payload, encode="json")
  }


  body <- content(resp)
  job_id <- purrr::pluck(body,"job_id",.default = NA)

  error <- F
  message <- NA

  # Request error
  if (resp$status_code != 200)
  {
    error <- T
    message <- purrr::pluck(body,"error","message",.default = NA)
  }

  # Job error
  else if (purrr::pluck(body,"success",.default = TRUE) != TRUE)
  {
    error <- T
    message <- purrr::pluck(body,"message",.default = NA)
  }

  # No job ID
  else if (is.na(job_id))
  {
    error <- T
    message <- "No job ID found."
  }


  if (error) {
    stop(paste0("Could not create job: ", message))
  }

  if (!is.na(message)){
    print(message)
  }

  # 2. Execute job
  api_job_execute(job_id)
}

#' Execute a job
#'
#' @param job_id The job ID
#' @return Whether the job was finished without error.
#' @export
api_job_execute <- function(job_id) {
  verbose <- Sys.getenv("epi_verbose") == "TRUE"
  print(paste0("Starting job ", job_id, "."))

  url = api_buildurl(paste0("jobs/execute/", job_id))

  polling <- T
  while (polling) {

    if (verbose) {
      resp <- POST(url, set_cookies(XDEBUG_SESSION="XDEBUG_ECLIPSE"))
    } else {
      resp <- POST(url)
    }

    body <- content(resp)

    # Request error
    if (resp$status_code != 200)
    {
      polling <- F
      error <- T
      message <- purrr::pluck(body,"error","message",.default = NA)
    }

    # Job error
    else if (purrr::pluck(body,"job","error",.default = FALSE) != FALSE)
    {
      polling <- F
      error <- T
      message <- purrr::pluck(body,"job","error",.default = NA)
    }

    # Continue
    else if (!is.na(purrr::pluck(body,"job","nexturl",.default = NA)))
    {
      polling <- T
      error <- F
      message <- purrr::pluck(body,"job","message",.default = NA)

      delay <- purrr::pluck(body,"job","delay",.default = 0)
      if (delay > 0) {
        Sys.sleep(1)
      }

      #url <- purrr::pluck(body,"job","nexturl",.default = NA)
      #api_buildurl(url, NA, "epi_all")

      progressCurrent <- purrr::pluck(body,"job","progress",.default = NA)
      progressMax <- purrr::pluck(body,"job","progressmax",.default = -1)
      if (progressMax == -1) {
        print(paste0("Progress ", progressCurrent))
      } else {
        print(paste0("Progress ", progressCurrent, " / ", progressMax))
      }
    }

    # Finished
    else
    {
      polling <- F
      error <- F
      message <- purrr::pluck(body,"message",.default = NA)
    }

    # Output
    if (error) {
      stop(paste0("Could not execute job: ", message))
    }

    if (!is.na(message)) {
      print(message)
    }

  }

  return (invisible((polling == F) & (error == F)))
}


#' Download tables
#'
#' Fetches tables such as articles, projects or properties
#'
#' TODO: silent problem message (false positive, results from the last column being empty from case to case)
#' TODO: add progress bar
#'
#' @param endpoint The endpoint path (e.g. "articles/index" or "articles/view/1")
#' @param params A named list of query params
#' @param db The database name
#' @param maxpages Maximum number of pages to request.
#'                 Set to 1 for non-paginated tables.
#' @param silent Whether to output status messages
#' @export
api_table <- function(endpoint, params=c(), db, maxpages=1, silent=FALSE) {

  verbose <- Sys.getenv("epi_verbose") == "TRUE"


  data = data.frame()
  page = 1

  fetchmore <- TRUE
  while (fetchmore) {
    params["page"] <- page
    url = api_buildurl(endpoint, params, db, "csv")

    if (!silent) {
      if (maxpages == 1) {
        message(paste0("Fetching data from ", endpoint,"."))
      } else {
        message(paste0("Fetching page ", page ," from ", endpoint,"."))
      }
    }
    message <- NA

    rows <- tryCatch(
      {

        if (verbose)  {
          resp <- GET(url, set_cookies(XDEBUG_SESSION="XDEBUG_ECLIPSE"))
        } else {
          resp <- GET(url)
        }


        if (resp$status_code == 200) {
          body <- content(resp,as="text")
          rows <- suppressWarnings(read_delim(I(body), delim=";", col_types = cols(.default = col_character())))
        }


        else if (resp$status_code == 404) {
          message <- "No more data found."
          rows <- data.frame()
        }

        else {
          rows <- data.frame()
          message <- paste0("Error ",resp$status_code,": ", content(resp))
        }

        rows
      },
      error=function(msg) {
        message <- msg
        data.frame()
      }
    )

    if (!is.na(message)) {
      print(message)
    }

    if (nrow(rows) > 0) {
      data <- bind_rows_char(list(data, rows))
      fetchmore <- (page < maxpages)
      page <- page + 1
    } else {
      fetchmore = F
    }
  }

  if (!silent) {
    print (paste0("Fetched ", nrow(data) ," records from ", endpoint,"."))
  }

  if (nrow(data) > 0) {
    data <- suppressMessages(type_convert(data))
  }

  .to_epitable(data, c("endpoint"=endpoint, "params" = params, "db"=db))
}



#' Patch data
#'
#' Update records in the database using the API.
#' Existing records will be updated, missing records will be created.
#' The function supports uploading all data related to articles:
#' articles, sections, items, links, footnotes, properties, projects, users, types.
#' The IRI path in the ID column of the dataframe must contain the specific table name.
#'
#' @param data A dataframe with the column `id`.
#'             Additional columns such as norm_data will be written to the record.
#'             The id must either be a a valid IRI path (e.g. properties/objecttypes/xxx)
#'             or an id prefixed by the table name (e.g. properties-12).
#'             Patching properties with prefixed ids requires a `type` column
#'             that contains the property type.
#'             Column names with table names as prefixes will be extracted, if wide is set to TRUE (default).
#' @param database The database name
#' @param table Optional: Check that the data only contains rows for a specific table
#' @param type Optional: Check that the data only contains rows with a specific type
#' @param wide Convert wide format to long format.
#'             If TRUE, column names prefixed with "properties", "items", "sections", "articles"
#'             and "projects" followed by a dot (e.g. "properties.id",
#'            "properties.lemma") will be extracted and patched as additional records.
#' @export
api_patch <- function(data, database, table=NA, type=NA, wide=T) {

  if (wide) {
    data <- epi_wide_to_long(data)
  }

  stopifnot(epi_is_iripath(data$id, table, type) | epi_is_id(data$id, table))

  # IRI path
  data <- data %>%
    dplyr::select(id, everything()) %>%

    # Remove complete empty columns
    dplyr::select(where(~!all(is.na(.x)))) %>%

    # Remove rows where all values are NA
    dplyr::filter(if_any(everything(), ~ !is.na(.)))


  if ((nrow(data) == 0) || (ncol(data) == 0)) {
    stop("Data is empty or contains NA values.")
  }

  if ((ncol(data) == 1) && (colnames(data) == "id")) {
    stop("Skipped, the data only contains the ID column.")
  }

  print(paste0("Uploading ",nrow(data)," rows."))

  api_job_create("articles/import", NA, database,list(data=data))
}

#' Add the epi_tbl class and make it remember its source
#'
#' @param data A tibble
#' @param source A named vector of source parameters, containing endpount, parameters and database name
.to_epitable <- function(data, source=NULL) {
  if (!is.null(source)) {
    attr(data, "source") <- source
  }

  # Reorder columns
  id_cols <-  intersect(c("database", "table", "row", "type", "norm_iri"), names(data))
  belongsto_id_cols <- grep("id$", names(data), value = TRUE)
  belongsto_name_cols <- intersect(c("project","article","section","item","property","footnote"), names(data))
  state_cols <- grep("^(created)|(modified)", names(data), value = TRUE)
  content_cols <- setdiff(names(data), c(id_cols, belongsto_id_cols, belongsto_name_cols, state_cols))

  data <- dplyr::select(
    data,
    dplyr::all_of(id_cols),
    dplyr::all_of(content_cols),
    dplyr::all_of(belongsto_name_cols),
    dplyr::all_of(belongsto_id_cols),
    dplyr::all_of(state_cols)
  )


  class(data) <- c("epi_tbl", setdiff(class(data), "epi_tbl"))
  data
}
