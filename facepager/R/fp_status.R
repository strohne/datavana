#' Get details of the query
#' @description These details include the numbers of
#' levels, query types, query status and object types.
#' @param data the loaded data from Facepager
#' @return a tibble with 5 columns: level, query_type, query_status, object_type  and n
#' @examples
#' @export
fp_status <- function(data) {
  data %>%
    count(level,query_type,query_status,object_type)
}
