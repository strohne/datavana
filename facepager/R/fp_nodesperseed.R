#' get the number of childnodes for each seed for a certain period of time
#' @import dplyr
#' @param .data the loaded data from Facepager
#' @param col_created column with the date and time the data (post, tweet, etc.) was created at
#' @param daterange=NULL the period of time you want to know, e.g. ymd(c("2019-11-01","2020-02-01"),tz="Europe/Berlin")
#' @param prefix=NA when transformatting, the values are not prefixed with a character
#' @param col_seed=NULL calculation is for each seed
#' @return a tibble ..?
#' @examples
#' @export
fp_nodesperseed <- function(.data,col_created,daterange=NULL,prefix=NA,col_seed=NULL) {
  #For testing
  # .data <- posts
  # daterange = ymd(c("2019-11-01","2020-02-01"),tz="Europe/Berlin")
  # col_created = quo(created_time)

  col_created <- enquo(col_created)

  # Add seed
  # Remove duplicates
  col_seed <- enquo(col_seed)
  if (quo_is_null(col_seed)) {
    .data <- fp_addseed(.data)
    col_seed <- "seed"
    col_seed <- as.name(col_seed)
    col_seed <- enquo(col_seed)
  }


  # Filter
  if (!is.null(daterange)) {
    from <- daterange[1]
    to <- daterange[2]
    .data <- .data %>%
      dplyr::filter((!!col_created >= from) & (!!col_created < to))
  }


  # Aggregate
  seeds <- .data %>%
    dplyr::filter(!is.na(!!col_created)) %>%
    group_by(!!col_seed) %>%
    summarize(
      count = n(),
      from = min(!!col_created),
      to = max(!!col_created),
      days = as.numeric(to - from)+1
    ) %>%
    ungroup()

  # Per week (ersten und letzten Tag rausnehmen)
  perweek <- .data %>%
    group_by(!!col_seed) %>%
    dplyr::filter(!is.na(!!col_created)) %>%
    dplyr::filter(!!col_created >= (date(min(!!col_created))+1 )) %>%
    dplyr::filter(!!col_created < (date(max(!!col_created)))) %>%
    summarize(
      count = n(),
      from = min(date(!!col_created)),
      to = max(date(!!col_created)),
      days = as.numeric(to - from)+1,
      perweek = count / ifelse(days>=7,days,NA) * 7
    ) %>%
    ungroup()


  # Join
  idx_left <- c(quo_name(col_seed))
  idx_right <- c(quo_name(col_seed))
  by = set_names(idx_right,idx_left)
  seeds <- left_join(seeds,select(perweek,!!col_seed,perweek),by=by)

  # Rename
  if (!is.na(prefix))
    seeds <- rename_all(seeds ,~paste0(prefix,.))

  seeds
}


#' helper function for fp_addseed()
#' @import dplyr
#' @export
fp_addseed <- function(.data) {
  .data %>%
    dplyr::filter(object_type=="data",query_status=="fetched (200)") %>%
    inner_join(.data %>% filter(level == 0) %>% select(seed = object_id,id),by=c("parent_id"="id")) %>%
    distinct(seed,object_id,.keep_all = T)
}
