#' get the number of childnodes for each node shown in Facepager
#' @import ggplot2
#' @import tidyr
#' @import dplyr
#' @param data the loaded data from Facepager
#' @param .global=T ...
#' @param .maxlevel the node level
#' @return a tibble with 4 columns: parent_id, parent_no, parent_objectid, children
#' @return a boxplot
#' @examples
#' @export
fp_children_bynode <- function(data, .global=T, .maxlevel=1) {
  # Für jeden Knoten so wie in Facepager angezeigt die Zahl
  # der Unterknoten bestimmen.
  # parent_no gibt die Position des Datensatzes in Facepager an
  parents <- fp_getparents(data)

  children.bynode <- data %>%
    dplyr::filter(object_type == "data",level == .maxlevel) %>%
    count(parent_id) %>% rename(children = n) %>%
    right_join(parents,by=c("parent_id")) %>%
    replace_na(list(children = 0)) %>%
    select(parent_id,parent_no,parent_objectid,children)

  .pl <- ggplot(children.bynode,aes(y=children+1,x="")) +
    geom_boxplot() +
    scale_y_log10() +
    coord_flip()
  print(.pl)

  result <- list("children.bynode" = children.bynode)
  if (.global)
    list2env(result, envir = .GlobalEnv)

  return(result)
}
