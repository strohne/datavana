#' Plot items
#'
#' @import forcats
#' @importFrom stats na.omit
#' @importFrom dplyr filter
#' @param data A tibble containing only the selected items
#' @export
plot_compare_items <- function(data) {

  # Code labels
  codes <- tibble(
    item = colnames(data),
    label = sapply(data,attr,"comment"),
    value = lapply(data,attributes)
  ) %>%
    mutate(label=as.character(label)) %>%
    unnest_longer(value) %>%
    filter(value_id != "comment", value_id != "class" ) %>%
    mutate(value = as.character(value))

  # Item labels
  items <- codes %>%
    na.omit() %>%
    distinct(item,label)

  # Plot
  data %>%
    na.omit() %>%
    pivot_longer(everything(), names_to="item",values_to="value_id") %>%
    count(item,value_id) %>%

    mutate(value_id = as.character(value_id)) %>%

    left_join(items,by=c("item")) %>%
    left_join(select(codes,item,value_id,value),by=c("item","value_id")) %>%

    mutate(label = str_remove(label,"^.*: ")) %>%
    mutate(label = str_trunc(label,50) ) %>%
    mutate(item=paste0(item," ",label)) %>%
    mutate(item = forcats::fct_rev(item)) %>%

    mutate(value=paste0(value_id, " ", value)) %>%
    #mutate(value = forcats::fct_rev(value)) %>%
    #mutate(value=as.factor(value)) %>%

    group_by(item) %>%
    mutate(p= n / sum(n)) %>%
    ungroup() %>%


    ggplot(aes(item,y=p,fill=value)) +
    geom_col() +
    geom_text(aes(label=n),position=position_stack(vjust=0.5),size=3) +
    scale_fill_brewer(type="qual") +
    coord_flip()  +
    theme(legend.position="bottom")
}
