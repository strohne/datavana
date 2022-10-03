#' Function to load a database
# shard: prefix ids to load multiple databases
#' @param
#' @keywords
#' @examples
#' @export
fp_loaddatabase <- function(filename,shard=NA) {

  # Load data
  fields <- "objectid,objecttype,id,parent_id,level,childcount,
             querystatus,querytype,querytime,response"
  data <- fp_load_nodes(filename, fields)

  if (!is.na(shard)) {
    data <- data %>%
      mutate(id=paste0(shard,"_",id),
             parent_id=ifelse(is.na(parent_id),NA,paste0(shard,"_",parent_id)))
  }


  return (data)
}
