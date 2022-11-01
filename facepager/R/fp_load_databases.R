#' Load a fp database
#' @param filename name of the fp database you want to load (file extension db)
#' @param shard prefix ids to load multiple databases
#' @return a data frame containing the data of the fp database
#' @examples
#' @export
fp_load_database <- function(dbname,shard=NA) {

  # Load data
  fields <- "objectid,objecttype,id,parent_id,level,childcount,
             querystatus,querytype,querytime,response"
  data <- fp_load_nodes(dbname, fields)

  if (!is.na(shard)) {
    data <- data %>%
      mutate(id=paste0(shard,"_",id),
             parent_id=ifelse(is.na(parent_id),NA,paste0(shard,"_",parent_id)))
  }


  return (data)
}

#' Load data from a fp database
#' @import RSQLite
#' @param dbname name of the fp database you want to load data from (file extension db)
#' @param fields select your collected fields you want to load
#' (you can only load fields you have collected via facepager)
#' e.g. object_id, object_type, query_status, query_type, query_time etc.
#' @param .progress progress bar displays estimated time remaining
#' @param shard prefix ids to load multiple databases
#' @return A data frame containing the data of the selected fields of the fp database.
#' @examples

#' @export
fp_load_nodes <- function(dbname, fields = '*',.progress=NULL, shard=NA) {
  db.con = dbConnect(RSQLite::SQLite(), dbname=dbname,flags=SQLITE_RO)

  statement = paste0('select ',fields,' from Nodes')
  db.nodes = dbGetQuery( db.con,statement )
  dbDisconnect(db.con)

  data <- as_tibble(db.nodes)

  if (!is.null(.progress))
    .progress$tick()$print()

  if (!is.na(shard)) {
    data <- data %>%
      mutate(id=paste0(shard,"_",id),
             parent_id=ifelse(is.na(parent_id),NA,paste0(shard,"_",parent_id)))
  }

  return (data)
}


#' Load multiple fp databases
#' @import tidyverse
#' @param filenames names of the fp databases you want to load
#' @param fields select your collected fields you want to load
#' (you can only load fields you have collected via facepager)
#' e.g. objectid, object_type, query_status, query_type, query_time etc.
#' @return A data frame containing the data of multiple fp databases
#' @examples
#' @export
fp_load_databases <- function(filenames, fields=NULL) {

  # Load data
  if (is.null(fields)) {
    fields <- "objectid,objecttype,id,parent_id,level,childcount,
               querystatus,querytype,querytime,response"
  }

  .progress <- progress_estimated(length(filenames),min_time = 1)
  .progress$print()

  shard=ifelse(length(filenames) > 0,"file",NULL)
  data <- tidyverse::map_df(filenames,fp_load_nodes, fields,.id=shard,.progress=.progress)

  if (length(filenames) > 0) {
    data <- data %>%
      mutate(id=paste0(file,"_",id),
             parent_id=ifelse(is.na(parent_id),NA,paste0(file,"_",parent_id)))
  }


  .progress$stop()$print()

  cat("Loaded ",length(filenames)," databases.\n")
  data %>%
    count(file) %>%
    print()

  return (data)
}
