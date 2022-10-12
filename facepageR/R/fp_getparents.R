#' get the Object IDs  and number of parent nodes
#' @import dplyr
#' @param data the loaded data from Facepager
#' @param level=0 the node level
#' @return a tibble with 3 columns: parent_id, parent_object, parent_no
#' @examples
#' @export
fp_getparents <- function(.data, level=0) {


  # Object ID and number of parent nodes
  parents <- data %>%
    dplyr::filter(level == !!level,
                  objecttype %in% c("data","seed"),
                  querystatus %in% c("","fetched (200)")) %>%
    group_by(parent_id) %>%
    mutate(no = row_number()) %>%
    ungroup()



  if ("file" %in% colnames(data))  {
    parents <- parents %>%
      select(file,parent_id=id,parent_objectid = objectid,parent_no=no) %>%
      mutate(file=str_replace(file,"../databases/",""))
  }
  else {
    parents <- parents %>%
      select(parent_id=id,parent_objectid = objectid,parent_no=no)
  }


  return (parents)
}
