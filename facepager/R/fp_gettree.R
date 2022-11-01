#' Adds left and right values to the dataframe (modified preorder tree traversal)
#' See https://www.sitepoint.com/hierarchical-data-database-3/
#'
#' @param data dataframe with tree_level, tree_thread, tree_id, tree_parent, created
#' @return
#' @examples
#' @export
fp_gettree <- function(data) {
  # Progress
  .maxlevel = max(data$tree_level)
  .minlevel = min(data$tree_level)
  p <- progressr::progressor(steps = (.maxlevel-.minlevel+1) * 2)

  # Add descendents
  data$tree_descendants = 0
  for (.level in c(.maxlevel : .minlevel)) {
    p(message=paste0("Level ",.level))

    leafs <- data %>%
      filter(tree_level == .level)  %>%
      mutate(tree_descendants = tree_descendants + 1) %>%
      group_by(tree_thread,tree_id=tree_parent) %>%
      summarise(tree_leafs = sum(tree_descendants),.groups="keep") %>%
      ungroup(tree_thread,tree_id)

    data <-  data %>%
      left_join(leafs,by=c("tree_thread","tree_id")) %>%
      replace_na(list(tree_leafs=0)) %>%
      mutate(tree_descendants = tree_descendants + tree_leafs) %>%
      select(-tree_leafs)

  }


  # Add left
  data <- data %>%
    group_by(tree_thread,tree_parent) %>%
    arrange(created) %>%
    mutate(tree_order = row_number()) %>%
    mutate(tree_rgt = 1 + cumsum(tree_descendants)*2 + (2 * (tree_order-1)) + 1) %>%
    mutate(tree_lft = tree_rgt - 2*tree_descendants - 1) %>%
    ungroup(tree_thread,tree_parent) %>%
    select(-tree_descendants)


  # Bubble from parents to children
  .level <- .minlevel
  parents <- data %>%
    filter(tree_level == .minlevel)

  while(nrow(parents) > 0) {

    p(message=paste0("Level ",.level))
    .level <- .level + 1

    data <- data %>%
      left_join(select(parents,tree_thread,tree_id,tree_parent_lft=tree_lft),
                by=c("tree_thread","tree_parent"="tree_id"))%>%
      replace_na(list(tree_parent_lft = 0)) %>%
      mutate(tree_lft = tree_lft + tree_parent_lft) %>%
      mutate(tree_rgt = tree_rgt + tree_parent_lft) %>%
      select(-tree_parent_lft)

    parents <- data %>%
      semi_join(parents,by=c("tree_thread","tree_parent"="tree_id"))

  }

  return(data)
}
