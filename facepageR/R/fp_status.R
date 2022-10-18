#' Get details of the query
#' @description These details include the numbers of
#' levels, querytypes, querystatus and objecttypes.
#' @param data the loaded data from Facepager
#' @return a tibble with 5 columns: level, querytype, querystatus, objecttype  and n
#' @examples
#' @export
fp_status <- function(data) {
  data %>%
    count(level,query_type,query_status,object_type)
}
