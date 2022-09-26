#' Function to load data from the database
#' @import RSQLite
#' @param
#' @keywords
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
