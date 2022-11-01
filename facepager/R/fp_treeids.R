#' Create IDs to avoid collision between levels
#'
#' @param id Column holding the ID of the node
#' @param parent Column holding the parent ID of the node
#' @param type Column holding the type of the node. Every level needs a different type
#' @param levels The hierarchy of the types, e.g. c("forum","post","comment","reply")
#' @return
#' @examples
#' @export
fp_treeids <- function(.data,id,parent,type,levels) {
  id <- enquo(id)
  parent <- enquo(parent)
  type <- enquo(type)


  tree <- .data %>%
    select(plat_id=!!id,plat_parent=!!parent,msg_type=!!type)

  # Create IDs in a way multiple levels don't collide
  prefixes <- as.list(str_sub(levels,1,1))
  names(prefixes) <- levels

  tree <- tree %>%
    mutate(tree_id=recode(msg_type,!!!prefixes)) %>%
    mutate(tree_id=paste0(tree_id,"_",plat_id))

  # Create parent IDs in a way multiple levels don't collide
  prefixes <- as.list(str_sub(levels,1,1))
  prefixes[[length(prefixes)]] <- NULL
  names(prefixes) <- levels[2:length(levels)]

  tree <- tree %>%
    mutate(tree_parent=recode(msg_type,!!!prefixes)) %>%
    mutate(tree_parent=paste0(tree_parent,"_",plat_parent))

  tree <- select(tree,tree_id,tree_parent)

  # Return
  bind_cols(tree,.data)
}
