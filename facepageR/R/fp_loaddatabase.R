#' Load a complete fp database
#' @param filename name of the fp database you want to load (file extension db)
#' @param shard prefix ids to load multiple databases
#' @return a data frame containing the data of the fp database
#' @examples
#' @export
fp_loaddatabase <- function(dbname,shard=NA) {

  # Load data
  fields <- "objectid,object_type,id,parent_id,level,childcount,
             query_status,query_type,query_time,response"
  data <- fp_load_nodes(dbname, fields)

  if (!is.na(shard)) {
    data <- data %>%
      mutate(id=paste0(shard,"_",id),
             parent_id=ifelse(is.na(parent_id),NA,paste0(shard,"_",parent_id)))
  }


  return (data)
}
