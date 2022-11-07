
#' Load a complete Facepager database
#' @param filename Name of the Facepager database, e.g. "posts.db".
#' @param fields A character vector of fields to load or "*" to load all fields.
#'               Possible candidates are:
#'               objectid, objecttype, querystatus, querytype, querytime, queryparamse
#'               id, parent_id, level, childcount, response.
#' @param rename Rename the columns to match the names in Facepager's CSV files
#' @param shard When loading multiple databases, give the ids a prefix. This way, the same numerical IDs from different databases don't come into conflict.
#' @return A data frame containing the data of the database
#' @examples
#' @export
fp_load_database <- function(filename, fields="*", rename=TRUE, shard=NA) {

  # Load data
  data <- fp_load_nodes(filename, fields, rename)

  if (!is.na(shard)) {
    data <- data %>%
      mutate(
        id = paste0(shard, "_", id),
        parent_id = ifelse(is.na(parent_id), NA, paste0(shard,"_",parent_id))
      )
  }

  return (data)
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
#' @param .progress progress bar displays estimated time remaining
#' @param shard When loading multiple databases, give the ids a prefix. This way, the same numerical IDs from different databases don't come into conflict.
#' @return A data frame containing the data of the selected fields of the fp database.
#' @examples

#' @export
fp_load_nodes <- function(filename, fields = '*', rename=T, .progress=NULL, shard=NA) {
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
#' @import purrr
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
  data <- purrr::map_df(filenames,fp_load_nodes, fields, rename, .id=shard, .progress=.progress)

  if (length(filenames) > 0) {
    data <- data %>%
      mutate(
        id=paste0(file,"_",id),
        parent_id=ifelse(is.na(parent_id),NA,paste0(file,"_",parent_id))
      )
  }

  .progress$stop()$print()

  cat("Loaded ",length(filenames)," databases.\n")
  data %>%
    count(file) %>%
    print()

  return (data)
}
