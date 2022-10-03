#' Function to get requests per minute
#' @import ggplot2
#' @param
#' @keywords
#' @examples
#' @export
fp_timing <- function(data,f.objecttype="offcut",f.querystatus="fetched (200)") {


  timing <- data %>%
    dplyr::filter(querystatus == f.querystatus) %>%
    dplyr::filter(objecttype == f.objecttype) %>%
    mutate(querytime = ymd_hms(querytime)) %>%
    mutate(queryminute = floor_date(querytime,unit="minute")) %>%
    mutate(queryhour = floor_date(querytime,unit="hour")) %>%
    mutate(queryday = date(querytime))

  cat("Start: ",format(min(timing$querytime)),"\n")
  cat("End: ",format(max(timing$querytime)),"\n")
  cat("Duration: ",as.character(as.period(interval(min(timing$querytime),max(timing$querytime)))),"\n")
  cat("Active hours: ",n_distinct(timing$queryhour), " \n")
  cat("Active minutes: ",n_distinct(timing$queryminute), " \n")

  cat("Requests per day (",f.objecttype,"): \n")
  timing %>%
    count(queryday) %>%
    print(n=100)

  timeline <- seq(min(timing$queryminute), max(timing$queryminute), by = "min")
  timeline <- timing %>%
    count(timeslot=queryminute) %>%
    tidyr::complete(timeslot=timeline,fill=list(n=0))}
