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
fp_loaddatabase <- function(filename, fields="*", rename=TRUE, shard=NA) {

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
