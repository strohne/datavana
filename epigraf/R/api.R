#
# Functions for API access to Epigraf
#


#' Save API connection settings to environment variables.
#' @param apiserver URL of the Epigraf server (including https-protocol)
#' @param apitoken Access token
#' @param verbose Show debug messages and the built URLs
#' @export
api_setup <- function(apiserver, apitoken, verbose=F) {
  settings <- as.list(environment())
  settings <- setNames(settings, paste0("epi_",names(settings)))
  do.call(Sys.setenv, settings)
}

#' Build base URL
#' @param endpoint
#' @param query
#' @param database
#' @param format
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


#' Download tabular data
#'
#' @param table The table name (e.g. "items")
#' @param params A named list of query params
#' @param db The database name
#' @param maxpages Maximum number of pages to request.
#'                 Set to 1 for non-paginated tables.
#' @export
api_table <- function(table, params=c(), db, maxpages=1) {

  verbose <- Sys.getenv("epi_verbose") == "TRUE"


  data = data.frame()
  page = 1

  fetchmore <- TRUE
  while (fetchmore) {
    params["page"] <- page
    url = api_buildurl(table, params, db, "csv")
    ext <- ".csv"

    print (paste0("Fetching page ", page ," from ", table,"."))
    message <- NA

    rows <- tryCatch(
      {

        if (verbose)  {
          resp <- GET(url, set_cookies(XDEBUG_SESSION="XDEBUG_ECLIPSE"), accept(ext))
        } else {
          resp <- GET(url, accept(ext))
        }


        if (resp$status_code == 200) {
          body <- content(resp,as="text")
          rows <- read_delim(I(body), delim=";", col_types = cols(.default = col_character()))
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
      data <- bind_rows(data, rows)
      fetchmore <- (page < maxpages)
      page <- page + 1
    } else {
      fetchmore = F
    }
  }

  print (paste0("Fetched ", nrow(data) ," records from ", table,"."))

  data <- type_convert(data)
  data
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


#' Patch data
#'
#' Update records in the database using the API.
#' Existing records will be updated, missing records will be created.
#' The function supports uploading all data related to articles:
#' articles, sections, items, links, footnotes, properties, projects, users, types.
#' The IRI path in the ID column of the dataframe must contain the specific table name.
#'
#' @param data A dataframe with the column id (must be a a valid IRI path).
#'              Additional columns such as norm_data will be written to the record.
#' @param database The database name
#' @param table Check that the data only contains rows for a specific table
#' @param type Check that the data only contains rows with a specific type
#' @export
api_patch <- function(data, database, table=NA, type=NA) {

  stopifnot(epi_is_iripath(data$id, table, type))

  # IRI path
  data <- data %>%
    select(id, everything()) %>%
    na.omit()

  if ((nrow(data) == 0) || (ncol(data) == 0)) {
    stop("Data is empty or contains NA values.")
  }

  api_job_create("articles/import", NA, database,list(data=data))
}


#' Patch properties
#'
#' Update properties in the database using the API.
#' Existing properties will be updated, missing properties will be created.
#' Whether a property is considered as existing depends on the type and the IRI fragment.
#' You can use these fields to overwrite existing data.
#'
#' @param database The database name
#' @param propertytype The property type (character)
#' @param lemmata A vector of lemmata (character).
#' @param irifragments Optional. A vector of IRI fragments (character) of the same length as the lemmata vector.
#' @export
api_patch_properties <- function(database, propertytype, lemmata, irifragments=NA){
  properties <- epi_create_properties(propertytype, lemmata, NA, irifragments)
  api_job_create("articles/import", NA, database,list(data=properties))
}


#' Patch types
#'
#' Update the types config in the database using the API.
#' Existing types will be updated, missing types will be created.
#'
#' @param types A dataframe with the column id (must be a a valid IRI path).
#'              Additional columns such as norm_data will be written to the article
#' @param database The database name
#' @export
api_patch_types <- function(types, database) {

  stopifnot(epi_is_iripath(types$id, "types"))

  # Article IRI path
  types <- types %>%
    select(id, everything()) %>%
    na.omit()

  api_job_create("types/import", NA, database,list(data=types))
}


#' Patch articles
#'
#' Update articles in the database using the API.
#' Existing articles will be updated, missing articles will be created.
#'
#' @param sections A dataframe with the column id (must be a a valid IRI path).
#'                 Additional columns such as norm_data will be written to the article
#' @param database The database name
#' @export
api_patch_articles <- function(articles, database) {

  stopifnot(epi_is_iripath(articles$id, "articles"))

  # Article IRI path
  articles <- articles %>%
    select(id, everything()) %>%
    na.omit()

  api_job_create("articles/import", NA, database,list(data=articles))
}

#' Patch sections
#'
#' Update sections in the database using the API.
#' Existing sections will be updated, missing sections will be created.
#' Section IRI fragments will be derived from the article IRI fragments.
#'
#' @param database The database name
#' @param sections A dataframe with the columns
#'                 article (must be a a valid IRI path) and
#'                 section (either a valid IRI path or a section type)
#'                 Additional columns such as name will be written to the section.
#' @export
api_patch_sections <- function(sections, database) {

  stopifnot(epi_is_iripath(sections$article, "articles"))

  # Article IRI path
  sections <- sections %>%
    rename(id=section,articles_id=article)

  # Section IRI path
  if (!all(epi_is_iripath(sections$id, "sections"))) {
    sections <- sections %>%
      mutate(articles_iri = str_extract(articles_id,"[^/]+$")) %>%
      mutate(id = paste0("sections/", id, "/",articles_iri)) %>%
      select(-articles_iri)
  }
  stopifnot(epi_is_iripath(sections$id, "sections"))

  sections <- sections %>%
    select(id, articles_id, everything()) %>%
    na.omit()

  api_job_create("articles/import", NA, database,list(data=sections))
}


#' Patch items and create related properties, sections, articles and projects
#'
#' @param data A dataframe with the column id containing a valid item IRI path.
#'             Additional columns such as norm_data will be written to the record.
#'
#'             Column names prefixed with "properties", "sections", "articles"
#'             and "projects" followed by a dot (e.g. "properties.id",
#'             "properties.lemma") indicate which other records will be patched.
#' @param database The database name
#' @export
api_patch_items <- function(data, database) {

  # Extract properties
  rows <- data %>%
    select(starts_with("properties.")) %>%
    rename_all(~str_replace(.,"properties\\.","")) %>%
    distinct()

  if ((nrow(rows) > 0) && (ncol(rows) > 0)) {
    api_patch(rows, database, "properties")
  }

  # Extract projects
  rows <- data %>%
    select(starts_with("projects.")) %>%
    rename_all(~str_replace(.,"projects\\.","")) %>%
    distinct()

  if ((nrow(rows) > 0) && (ncol(rows) > 0)) {
    api_patch(rows, database, "projects")
  }

  # Extract articles
  rows <- data %>%
    select(starts_with("articles."),matches("^projects.id$")) %>%
    rename_all(~str_replace(.,"articles\\.","")) %>%
    rename_all(~str_replace(.,"\\.","_")) %>%
    distinct()

  if ((nrow(rows) > 0) && (ncol(rows) > 0)) {
    api_patch(rows, database, "articles")
  }

  # Extract sections
  rows <- data %>%
    select(starts_with("sections."),matches("^articles.id$")) %>%
    rename_all(~str_replace(.,"sections\\.","")) %>%
    rename_all(~str_replace(.,"\\.","_")) %>%
    distinct()

  if ((nrow(rows) > 0) && (ncol(rows) > 0)) {
    api_patch(rows, database, "sections")
  }

  # Extract items
  rows <- data %>%
    select(matches("^[_a-z]+$"),matches("^articles\\.id|sections\\.id|properties\\.id$")) %>%
    rename_all(~str_replace(.,"\\.","_"))

  if ((nrow(rows) > 0) && (ncol(rows) > 0)) {
    api_patch(rows, database, "items")
  }

}

