#' Load multiple Facepager databases
#' @import purrr
#' @param filenames A character vector containing the names of the databases
#' @param fields select your collected fields you want to load
#' (you can only load fields you have collected via facepager)
#' e.g. objectid, object_type, query_status, query_type, query_time etc.
#' @return A data frame containing the data of multiple fp databases
#' @examples
#' @export
fp_loaddatabases <- function(filenames, fields="*", rename=TRUE) {

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
