#' Function to get number of levels, querytypes, querystatus and objecttypes
#' @param
#' @keywords
#' @examples
#' @export
fp_status <- function(data) {
  data %>%
    count(level,querytype,querystatus,objecttype)
}
