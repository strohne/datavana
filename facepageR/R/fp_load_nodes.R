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
#' @return A data frame containing the data of the selected fields of the fp database.
#' @examples

#' @export
fp_load_nodes <- function(filename, fields = '*', rename=T, .progress=NULL) {
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

  return (data)
}
