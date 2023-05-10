#' Compare factor value by group
#'
#' @param data A tibble
#' @param col_category The column holding factor values
#' @param col_group The column holding groups to compare
#' @param relative Show absolute (F) or relative values (T)
#' @export
plot_compare_factor <- function(data, col_category, col_group, relative=F) {
  col_category <- enquo(col_category)
  col_group <- enquo(col_group)


  data <- data %>%
    count(!!col_group,!!col_category) %>%
    group_by(!!col_category) %>%
    mutate(p = n / sum(n)) %>%
    ungroup()

  if (relative) {
    col_value <- quo(p)
  } else {
    col_value <- quo(n)
  }

  data %>%
    mutate(!!col_category := forcats::fct_rev(!!col_category)) %>%
    ggplot(aes(!!col_category,y=!!col_value,fill=!!col_group)) +
    geom_col() +
    geom_text(aes(label=n),position=position_stack(vjust=0.5)) +
    coord_flip()

}
