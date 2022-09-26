#' Function to
#' @param
#' @keywords
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
