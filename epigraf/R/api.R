#
# Functions for API access to Epigraf
#


#' Save API connection settings to environment variables.
#' @param apiserver URL of the Epigraf server (including https-protocol)
#' @param  apitoken Access token
#' @export
api_setup <- function(apiserver, apitoken) {
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

  url <- parse_url(server)
  url$query$token <- token

  # Add query parameters
  if (!is.na(query))  {
    url$query = c(url$query,query)
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
    url$path <- endpoint
  }

  build_url(url)

}
