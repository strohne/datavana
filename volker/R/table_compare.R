#' Compare & Table factor value by group
#'
#' @param data A tibble
#' @param col_category The column holding factor values
#' @param col_group The column holding groups to compare
#' @export
table_compare_factor <- function(data, col_category, col_group) {

  col_category <- enquo(col_category)
  col_group <- enquo(col_group)

  data <- data %>%
    count(!!col_group,!!col_category) %>%
    group_by(!!col_category) %>%
    mutate(p = n / sum(n)) %>%
    ungroup()
}

#' Compare & Table items
#'
#' @param data A tibble containing item measures
#' @param cols_items Tidyselect item variables (e.g. starts_with...)
#' @param col_group Optional faceting variable
#' @export
table_compare_items <- function(data, cols_items, col_group) {

  col_group <- enquo(col_group)

  # Get code labels from the attributes
  codes <- tibble(
    item = colnames(data),
    label = sapply(data,attr,"comment"),
    value = lapply(data,attributes)
  ) %>%
    mutate(label=as.character(label)) %>%
    tidyr::unnest_longer(value) %>%
    filter(value_id != "comment", value_id != "class" ) %>%
    mutate(value = as.character(value))

  # Filter item labels
  items <- codes %>%
    #na.omit() %>%
    distinct(item,label) %>%
    mutate(no = row_number())

  # Calculate
  data_grouped <- data %>%

    tidyr::pivot_longer(tidyselect::all_of(cols_items), names_to="item",values_to="value_id") %>%

    count(!!col_group,item,value_id) %>%
    group_by(!!col_group,item) %>%
    mutate(p= n / sum(n)) %>%
    ungroup() %>%

    # Labeling
    mutate(value_id = as.character(value_id)) %>%
    left_join(items,by=c("item")) %>%
    left_join(select(codes,item,value_id,value),by=c("item","value_id")) %>%

    mutate(label = str_remove(label,"^.*: ")) %>%
    mutate(label = str_trunc(label,50) ) %>%

    mutate(item=str_remove(item,common_prefix(.$item))) %>%
    mutate(item=paste0(item," ",label)) %>%
    mutate(item=forcats::fct_reorder(item,no, .desc=T)) %>%

    mutate(value=paste0(value_id, " ", value))

  data_grouped
}


