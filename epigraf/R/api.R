library(httr)

#
# Functions for API access to Epigraf
#


#' Save API connection settings to environment variables.
#' @param apiserver URL of the Epigraf server (including https-protocol)
#' @param  apitoken Access token
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

#' Merge list elements by their name
#'
#'@param l A list of lists to merge
#'@return A merged list
#'@export
merge_lists <- function(l) {
  keys <- unique(unlist(lapply(l, names)))
  l <- setNames(do.call(mapply, c(FUN=c, lapply(l, `[`, keys))), keys)
  as.list(l)
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

  data = data.frame()
  page = 1

  fetchmore <- TRUE
  while (fetchmore) {
    params["page"] <- page
    url = api_buildurl(table, params, db, "csv")

    print (paste0("Fetching page ", page ," from ", table,"."))

    rows <- tryCatch(
      {
        rows <- read_delim(url, delim=";", show_col_types=F)
      },
      error=function(msg) {
        print(msg)
        return(NA)
      }
    )

    if (!is.na(rows) && (nrow(rows) > 0)) {
      data <- bind_rows(data, rows)
      fetchmore <- (page < maxpages)
      page <- page + 1
    } else {
      fetchmore = F
    }
  }

  print (paste0("Fetched ", nrow(data) ," records from ", table,"."))

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
  print ("Creating job")
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
#' @return void
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
}
