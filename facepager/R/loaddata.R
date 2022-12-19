#' Load a CSV file
#' @import tidyverse
#' @param filename name of the CSV file you want to load
#' @param na define missing values as string 'None'
#' @return a tibble containing the data of the csv file
#' @examples data <- fp_read_csv2(system.file("extdata", "example.csv", package = "facepager"))
#' @export
fp_read_csv2 <- function(filename) {
  read_csv2(filename, na=c("None",""))
}

#' Load all nodes from a Facepager database
#' @import RSQLite
#' @param filename Name of the Facepager database, e.g. "post.db"
#' @param fields A character vector of fields to load or "*" to load all fields.
#'               Possible candidates are:
#'               objectid, objecttype, querystatus, querytype, querytime, queryparamse
#'               id, parent_id, level, childcount, response.
#'               The response field contains the data in JSON format.
#' @param rename Rename the columns to match the names in Facepager's CSV files
#' @param shard When loading multiple databases, give the ids a prefix. This way, the same numerical IDs from different databases don't come into conflict.
#' @param .progress progress bar displays estimated time remaining#'
#' @return A data frame containing the data of the selected fields of the fp database.
#' @examples data_db <- fp_load_database(system.file("extdata", "example.db", package = "facepager"))
#' @export
fp_load_database <- function(filename, fields = '*', rename=T, shard=NA, .progress=NULL) {
  db.con = dbConnect(RSQLite::SQLite(), dbname=filename,flags=SQLITE_RO)

  fields <- paste0(fields, collapse=",")
  statement = paste0('select ', fields, ' from Nodes')
  db.nodes = dbGetQuery( db.con, statement)
  dbDisconnect(db.con)

  data <- as_tibble(db.nodes)


  if (rename) {
    data <- data %>%
      rename(
        object_id=objectid,
        object_type=objecttype,
        query_status=querystatus,
        query_type=querytype,
        query_time=querytime,
        query_params = queryparams
      )
  }

  if (!is.null(.progress))
    .progress$tick()$print()


  if (!is.na(shard)) {
    data <- data %>%
      mutate(
        id=paste0(shard,"_",id),
        parent_id=ifelse(is.na(parent_id),NA,paste0(shard,"_",parent_id))
        )
  }

  return (data)
}


#' Load multiple Facepager databases
#' @importFrom  purrr map_df
#' @param filenames A character vector containing the names of the databases
#' @param fields select your collected fields you want to load
#' (you can only load fields you have collected via facepager)
#' e.g. objectid, object_type, query_status, query_type, query_time etc.
#' @return A data frame containing the data of multiple fp databases
#' @examples
#' @export
fp_load_databases <- function(filenames, fields="*", rename=TRUE) {

  fields <- paste0(fields, collapse=",")
  .progress <- progress_estimated(length(filenames),min_time = 1)
  .progress$print()

  # Load data
  shard=ifelse(length(filenames) > 0,"file", NULL)
  data <- purrr::map_df(filenames,fp_load_database, fields, rename, shard, .progress=.progress, .id=shard)

  .progress$stop()$print()

  cat("Loaded ",length(filenames)," databases.\n")
  data %>%
    count(file) %>%
    print()

  return (data)
}


#' Convert JSON response data to columns of a data frame
#' @importFrom jsonlite flatten
#' @description As you can see in the loaded data,
#' most of your data is in JSON format in the response column.
#' Therefore you need to convert the JSON data in the response to columns of a data frame.
#' @param nodes the fields/columns originally collected by Facepager,
#' which are all together in the response column,
#' can be for example "created_at", "text"/ "message", "favorite_count"/ "like_count" etc.
#' @param plain Only keep atomic columns, remove data frames and lists
#' @param prefix name of the columns
#' @return A data frame containing the data of the Facepager database
#'  with all response data as own columns
#' @examples data_db <- fp_load_database(system.file("extdata", "example.db", package = "facepager"))
#' data_db <- fp_parse_response_data(data_db)
#' @export
fp_parse_response_data <- function (nodes, plain = F, prefix="response.") {
  responses = fp_from_ndjson(nodes$response)

  responses = jsonlite::flatten(responses, recursive = T)
  if (plain) {
    responses <- select_if(responses,~!is.data.frame(.))
    responses <- select_if(responses,~!is.list(.))
  }

  colnames(responses) = paste0(prefix,colnames(responses))
  bind_cols(select(nodes,-response),responses)
}

#' helper function for fp_parse_response_data()
#' @importFrom jsonlite stream_in
#' @rdname fp_parse_response_data
#' @export
fp_from_ndjson <- function(data) {
  data[data == "[]"] = "{}"
  data[is.na(data)] = "{}"

  jsonlite::stream_in(textConnection(data))
}


#' helper function for fp_parse_response_data()
#' @import tidyverse
#' @rdname fp_parse_response_data
#' @export
fp_get_response_value <- function(nodes, col, .split=TRUE,.progress=NULL) {

  if (.split) {

    nodes <- nodes %>%
      mutate(chunk = row_number() %/% 1000 ) %>%
      split(.$chunk)

    .progress <- progress_estimated(length(nodes),min_time = 1)
    .progress$print()

    nodes <- nodes %>%
      purrr::map(fp_get_response_value,col,.split=FALSE,.progress=.progress) %>%
      unlist()

    .progress$stop()$print()
    cat("\n")

  } else {
    col_js <- do.call(jstring,as.list(strsplit(col, ".",fixed=T)[[1]]))
    nodes <- nodes$response %>%
      spread_values(value=col_js) %>% extract2(2)

    if (!is.null(.progress))
      .progress$tick()$print()
  }

  return(nodes)
}
