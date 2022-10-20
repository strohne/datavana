#' Add thread, level and order
#'
#' @import tidygraph
#' @param .data The dataframe containing hierarchical data
#' @param id The ID column of the node
#' @param parent The ID column of the parent node
#' @param sort Column for sorting the nodes inside each parent. Leave empty to use the order in the dataset.
#' @return Data frame with the additional columns tree_thread, tree_order and tree_level
#' @examples
#' @export
tree_add_level <- function(.data, id, parent, sort=NULL) {
  id <- enquo(id)
  parent <- enquo(parent)

  # See below
  #.numeric <- is.numeric(select(.data,!!id,!!parent))


  # Convert to character for tidygraph functions
  .data <- .data %>%
    mutate(!!id := as.character(!!id), !!parent := as.character(!!parent))

  # Sort and select tree data
  if (!missing(sort)) {
    sort <- enquo(sort)
    tree <- .data %>%
      arrange(!!sort)
  } else {
    tree <- .data
  }

  # Select and distinct
  tree <- tree %>%
    select(!!id,!!parent) %>%
    distinct()

  # Seperate roots and descendants
  # (to restructure tree in cases where parents are missing)
  by_roots = set_names(quo_name(id), quo_name(parent))

  tree_roots <- tree %>%
    anti_join(tree,by=by_roots)

  tree_descendants <- tree %>%
    semi_join(tree,by=by_roots)

  tree_nodes <- bind_rows(tree_roots,tree_descendants) %>%
    select(!!id)

  tree_edges <- tree_descendants %>%
    select(source=!!id, target=!!parent)

  rm(tree)
  rm(tree_roots)
  rm(tree_descendants)

  # Create graph
  gr <- tbl_graph(nodes = tree_nodes,edges = tree_edges)

  # Components, order and level
  # tree_comp: Thread number
  # tree_order: Number of node within thread
  # tree_level: Level of node
  gr <- gr %>%
    mutate(tree_comp = group_components(type = "weak")) %>%
    morph(to_components) %>%
    mutate(tree_order = dfs_rank(root=1,mode="in")) %>%
    mutate(tree_level = bfs_dist(root=1,mode="in"))   %>%
    unmorph()

  # Back to the node list and add root id per component
  tree <- gr %>%
    as_tibble() %>%

    group_by(tree_comp) %>%
    arrange(tree_order) %>%
    mutate(tree_thread = first(!!id)) %>%
    ungroup() %>%
    select(-tree_comp)

  # Join tree to original data
  .data <-   .data %>%
    left_join(tree, by=quo_name(id))  %>%
    arrange(tree_thread, tree_order) %>%
    mutate(!!parent := ifelse(tree_level==0,NA,!!parent)) %>%
    select(tree_id=!!id,tree_parent=!!parent,tree_thread,tree_order,tree_level, everything())

  # # Cast character back to numeric
  # if (.numeric) {
  #
  # }

  return (.data)

}

#' Row bind all ancestors of the selected nodes
#'
#' @param .data Data frame containing the selected nodes
#' @param .tree Data frame containing all nodes including all ancestors
#' @param id Column name of the id in .data and .tree
#' @param parent_id Column name of the parent id in .data and .tree
#' @return Data frame containing the nodes of .data and alle ancestors
#' @examples
#' @export
tree_add_ancestors <- function(.data, .tree, id, parent_id) {
  id <- enquo(id)
  parent_id <- enquo(parent_id)

  # Equavalent to c("id" = "parent_id"), note the changed field order
  joinby = set_names(quo_name(parent_id), quo_name(id))

  selected = tibble()

  while (nrow(.data) > 0) {
    print(paste0(nrow(.data), " nodes added" ))
    .data = bind_rows(selected, .data)
    .data = .tree %>%
      semi_join(.data,by=joinby)
  }

  return (selected)
}


#' Adds left and right values to the dataframe (modified preorder tree traversal)
#' See https://www.sitepoint.com/hierarchical-data-database-3/
#'
#' @param data Dataframe with the columns tree_id, tree_parent, tree_thread, tree_level, tree_order
#'            TODO: parameters?
#' @return
#' @examples
#' @export
tree_add_mptt <- function(.data) {
  # Progress
  .maxlevel = max(.data$tree_level)
  .minlevel = min(.data$tree_level)
  p <- progressr::progressor(steps = (.maxlevel-.minlevel+1) * 2)

  # Add descendants
  .data$tree_descendants = 0
  for (.level in c(.maxlevel : .minlevel)) {
    p(message=paste0("Level ",.level))

    descendants <- .data %>%
      filter(tree_level == .level)  %>%
      mutate(tree_descendants = tree_descendants + 1) %>%
      group_by(tree_thread, tree_id=tree_parent) %>%
      summarise(tree_tmp_descendants = sum(tree_descendants),.groups="keep") %>%
      ungroup(tree_thread, tree_id)

    .data <-  .data %>%
      left_join(descendants, by=c("tree_thread", "tree_id")) %>%
      replace_na(list(tree_tmp_descendants=0)) %>%
      mutate(tree_descendants = tree_descendants + tree_tmp_descendants) %>%
      select(-tree_tmp_descendants)

  }


  # Add left
  .data <- .data %>%
    group_by(tree_thread, tree_parent) %>%
    arrange(tree_order) %>%
    mutate(tree_no = row_number()) %>%
    mutate(tree_rgt = 1 + cumsum(tree_descendants)*2 + (2 * (tree_no-1)) + 1) %>%
    mutate(tree_lft = tree_rgt - 2*tree_descendants - 1) %>%
    ungroup(tree_thread,tree_parent) %>%
    select(-tree_descendants)


  # Bubble from parents to children
  .level <- .minlevel
  parents <- .data %>%
    filter(tree_level == .minlevel)

  while(nrow(parents) > 0) {

    p(message=paste0("Level ",.level))
    .level <- .level + 1

    .data <- .data %>%
      left_join(select(parents,tree_thread,tree_id,tree_parent_lft=tree_lft),
                by=c("tree_thread","tree_parent"="tree_id"))%>%
      replace_na(list(tree_parent_lft = 0)) %>%
      mutate(tree_lft = tree_lft + tree_parent_lft) %>%
      mutate(tree_rgt = tree_rgt + tree_parent_lft) %>%
      select(-tree_parent_lft)

    parents <- .data %>%
      semi_join(parents,by=c("tree_thread","tree_parent"="tree_id"))

  }

  .data <- .data %>%
    arrange(tree_thread, tree_order) %>%
    select(starts_with("tree_"), everything())

  return(.data)
}



#' Create tree IDs - disambiguate IDs of different node types to avoid collision between IDs
#'
#' TODO: test and refine
#'
#' @param id Column holding the ID of the node
#' @param parent Column holding the parent ID of the node
#' @param type Column holding the type of the node. Every level needs a different type, e.g. "page", "post", "comment" or "reply".
#' @param levels The hierarchy of the types, e.g. c("page","post","comment","reply")
#' @return
#' @examples
#' @export
tree_disambiguate_ids <- function(.data, id, parent, type, levels) {
  id <- enquo(id)
  parent <- enquo(parent)
  type <- enquo(type)

  tree <- .data %>%
    select(msg_type=!!type)

  # Create IDs in a way multiple levels don't collide
  prefixes <- as.list(str_sub(levels,1,1))
  names(prefixes) <- levels

  tree <- tree %>%
    mutate(tree_id=recode(msg_type,!!!prefixes)) %>%
    mutate(tree_id=paste0(tree_id,"_",!!id))

  # Create parent IDs in a way multiple levels don't collide
  prefixes <- as.list(str_sub(levels,1,1))
  prefixes[[length(prefixes)]] <- NULL
  names(prefixes) <- levels[2:length(levels)]

  tree <- tree %>%
    mutate(tree_parent=recode(msg_type,!!!prefixes)) %>%
    mutate(tree_parent=paste0(tree_parent,"_",!!parent))

  tree <- select(tree,tree_id,tree_parent)

  # Return
  bind_cols(tree,.data)
}



#' Transfer tree_thread (ID of root node) to all children of root nodes
#'
#' @param data All nodes (e.g. posts and comments)
#' @param col_id The column holding IDs of the nodes
#' @param col_parent The column holding IDs of the parent nodes
#' @param col_order The column used for ordering the nodes inside of a level
#' @return
#' @examples
#' @export
fp_getthreads <- function(data,col_id,col_parent,col_order=NULL) {
  # Quoting
  col_id <- enquo(col_id)
  col_parent <- enquo(col_parent)
  col_order <- enquo(col_order)
  if (quo_is_null(col_order)) {
    col_order <- col_id
  }

  # Prepare columns
  data <- mutate(data,.tree_id=!!col_id)
  data <- mutate(data,.tree_parent=!!col_parent)

  # Prepare roots
  roots <- data %>%
    anti_join(data,by=c(".tree_parent"=".tree_id")) %>%
    mutate(tree_thread=.tree_id,tree_level=0,tree_order=0)

  # First level
  .level <- 1
  children <- data %>%
    inner_join(select(roots,.tree_id,tree_thread),by=c(".tree_parent"=".tree_id")) %>%
    mutate(tree_level=.level) %>%

    group_by(.tree_parent) %>%
    arrange(!!col_order) %>%
    mutate(tree_order=row_number()) %>%
    ungroup()

  cat("Level ",.level,". ",sep="")
  cat(nrow(children)," nodes addes.\n\n",sep="")

  while (TRUE) {

    .level <- .level + 1
    cat("Level ",.level,". ",sep="")

    children.next <- data %>%
      anti_join(children,by=c(".tree_id")) %>%
      inner_join(select(children,tree_thread,.tree_id,.parent_order=tree_order),by=c(".tree_parent"=".tree_id")) %>%
      mutate(tree_level=.level) %>%

      group_by(.tree_parent) %>%
      arrange(!!col_order) %>%
      mutate(tree_order= row_number()) %>%
      ungroup() %>%
      select(-.parent_order)

    children <- dplyr::bind_rows(children,children.next)

    cat(nrow(children.next)," nodes addes.\n\n",sep="")

    if (!nrow(children.next))
      break
  }

  bind_rows(roots,children) %>%
    arrange(tree_thread,tree_order) %>%
    select(-.tree_id,-.tree_parent)

}
