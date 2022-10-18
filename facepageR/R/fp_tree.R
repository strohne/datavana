#' Creates IDs for threads, adds level and order data
#'
#' @import tidygraph
#' @param .data The dataframe containing hierarchical data
#' @param id The ID of the node
#' @param parent The ID of the parent node
#' @param no The number of the node inside of parents
#' @return
#' @examples
#' @export
fp_tree <- function(.data,id,parent,no) {
  id <- enquo(id)
  parent <- enquo(parent)
  no <- enquo(no)

  # Seperate roots and leafs
  tree <- .data %>%
    arrange(!!no) %>%
    select(!!id,!!parent) %>%
    distinct()

  by = set_names(quo_name(id), quo_name(parent))
  tree.roots <- tree %>%
    anti_join(tree,by=by)

  tree.leafs <- tree %>%
    anti_join(tree.roots,by=quo_name(id))

  rm(tree)

  # Create graph
  gr <- tbl_graph(nodes = bind_rows(tree.roots,tree.leafs),edges = tree.leafs)

  # Komponenten, Reihenfolge und Ebene feststellen
  # tree_comp: Nummer des Strangs
  # tree_order: Nummer der Mitteilung im Strang
  # tree_level: Ebene der Mitteilung
  gr <- gr %>%
    mutate(tree_thread = group_components(type = "weak")) %>%
    morph(to_components) %>%
    mutate(tree_order = dfs_rank(root=1,mode="in")) %>%
    mutate(tree_level = bfs_dist(root=1,mode="in"))   %>%
    unmorph()

  # Zurüch zu Knotenliste
  tree <- gr %>%
    as_tibble() %>%
    arrange(tree_thread,tree_order)

  # Join (1. right_index 2. left_index)
  .data %>%
    left_join(select(tree,-!!parent),by=quo_name(id))  %>%
    select(starts_with("tree_"),everything()) %>%
    arrange(tree_thread,tree_order)

}
