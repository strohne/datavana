#' Function to count the collected data per Object ID
#' @param
#' @keywords
#' @examples
#' @export
fp_children <- function(.data, col_target=NA,col_paging=NA,col_created=NA,timestamp=F,level=0) {

  parents <- fp_childcount(.data,level=!!level)

  parents <- parents %>%
    left_join(fp_coverage(.data, col_target=col_target,level=!!level),by="parent_objectid")

  parents <- parents %>%
    left_join(fp_paging(.data, col_paging=col_paging,level=!!level),by="parent_objectid")

  parents <- parents %>%
    left_join(fp_created(.data, col_created=col_created,timestamp,level=!!level),by="parent_objectid")

  invisible(parents)
}
