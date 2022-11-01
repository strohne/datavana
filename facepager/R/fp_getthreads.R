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
