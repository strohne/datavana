#' Load multiple fp databases
#' @import purrr
#' @param filenames names of the fp databases you want to load
#' @param fields select your collected fields you want to load
#' (you can only load fields you have collected via facepager)
#' e.g. objectid, objecttype, querystatus, querytype, querytime etc.
#' @return A data frame containing the data of multiple fp databases
#' @examples
#' @export
fp_loaddatabases <- function(filenames, fields=NULL) {

  # Load data
  if (is.null(fields)) {
    fields <- "objectid,objecttype,id,parent_id,level,childcount,
               querystatus,querytype,querytime,response"
  }

  .progress <- progress_estimated(length(filenames),min_time = 1)
  .progress$print()

  shard=ifelse(length(filenames) > 0,"file",NULL)
  data <- purrr::map_df(filenames,fp_load_nodes, fields,.id=shard,.progress=.progress)

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
