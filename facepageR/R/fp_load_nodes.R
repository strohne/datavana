#' Load single data from a fp database
#' @import RSQLite
#' @param dbname name of the fp database you want to load data from (file extension db)
#' @param fields select your collected fields you want to load
#' (you can only load fields you have collected via facepager)
#' e.g. objectid, objecttype, querystatus, querytype, querytime etc.
#' @param .progress progress bar displays estimated time remaining
#' @return A data frame containing the data of the selected fields of the fp database.
#' @examples

#' @export
fp_load_nodes <- function(dbname, fields = '*',.progress=NULL) {
  db.con = dbConnect(RSQLite::SQLite(), dbname=dbname,flags=SQLITE_RO)

  statement = paste0('select ',fields,' from Nodes')
  db.nodes = dbGetQuery( db.con,statement )
  dbDisconnect(db.con)

  data <- as_tibble(db.nodes)

  if (!is.null(.progress))
    .progress$tick()$print()

  return (data)
}
