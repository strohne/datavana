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

    print (paste0("Fetching page ", page ," from ", table,"."))
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
          rows <- read_delim(I(body), delim=";", show_col_types=F)
        }

        else if (resp$status_code == 404) {
          message <- "No more data found."
          rows <- data.frame()
        }

        else {
          rows <- data.frame()
          message <- content(resp)
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

#' Patch properties
#'
#' Update properties in the database using the API.
#' Existing properties will be updated, missing properties will be created.
#'
#' @param database The database name
#' @param propertytype The property type (character)
#' @param lemmata A vector of lemmata (character).
#' @param irifragments Optional. A vector of IRI fragments (character).
#' @export
api_patch_properties <- function(database, propertytype, lemmata, irifragments=NA){
  properties <- epi_create_properties(propertytype, lemmata, NA, irifragments)
  api_job_create("articles/import", NA, database,list(data=properties))
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



#' Patch items
#'
#' Existing items will be updated, missing items will be created.
#' The section IRI fragments and the item IRI iri fragments will be derived from the
#' article IRI fragments. The item IRI will be suffixed by the sortno value (default=1).
#'
#' @param items Dataframe with the columns
#'              article (must be a valid IRI path),
#'              section (either a valid IRI path or a section type),
#'              item (the item type).
#'              sortno (to address multiple items in a section, optional, defaults to 1)
#'              Additional columns such as properties_id or value will be
#'              written to the item.
#' @param database The database name
#' @export
api_patch_items <- function(items, database) {

  stopifnot(epi_is_iripath(items$article, "articles"))
  stopifnot(stringr::str_detect(items$item, "^[a-z0-9_-]+$"))

  items <- items %>%
    rename(
      articles_id=article,
      sections_id=section,
      itemtype=item
    )


  if (! "sortno" %in% colnames(items)) {
    items$sortno <- 1
  }

  # Section IRI path
  if (!all(epi_is_iripath(items$sections_id, "sections"))) {
    items <- items %>%
      mutate(articles_iri = str_extract(articles_id,"[^/]+$")) %>%
      mutate(sections_id = paste0("sections/", sections_id, "/",articles_iri)) %>%
      select(-articles_iri)
  }

  stopifnot(epi_is_iripath(items$sections_id, "sections"))

  items <- items %>%
    mutate(
      articles_iri = str_extract(articles_id,"[^/]+$"),
      norm_iri = paste0(articles_iri,"~",sortno),
      id = paste0("items/", itemtype, "/",norm_iri),
    ) %>%
    select(-itemtype,-norm_iri,-articles_iri) %>%
    select(id, sections_id, articles_id, everything()) %>%
    na.omit()

  stopifnot(epi_is_iripath(items$sections_id, "sections"))

  api_job_create("articles/import", NA, database,list(data=items))
}

