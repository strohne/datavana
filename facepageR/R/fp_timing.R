#' Get requests per minute
#' @import ggplot2
#' @import lubridate
#' @import tidyr
#' @param data the loaded data from Facepager
#' @param f.objecttype object types to be included in the calculation of requests/minute.
#' Usually "data", because "seed" and "offcut" do not contain any data.
#' @param f.querystatus query status to be included in the calculation of requests/minute.
#' Only those nodes are included here that have been fetched in full like "fetched(200)".
#' @return a list with Start, End, Duration, Active hours, Active minutes and Requests per day
#'  a line plot with requests per minute as yvalue and the time es xvalue
#' @examples
#' @export
fp_timing <- function(data,f.objecttype="data",f.querystatus="fetched (200)") {


  timing <- data %>%
    dplyr::filter(querystatus == f.querystatus) %>%
    dplyr::filter(objecttype == f.objecttype) %>%
    mutate(querytime = lubridate::ymd_hms(querytime)) %>%
    mutate(queryminute = lubridate::floor_date(querytime,unit="minute")) %>%
    mutate(queryhour = lubridate::floor_date(querytime,unit="hour")) %>%
    mutate(queryday = lubridate::floor_date(querytime,unit="day"))

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
    tidyr::complete(timeslot=timeline,fill=list(n=0))

# Requests per minute
pl <- timeline %>%

  ggplot(aes(x=timeslot,y=n)) +
  geom_line(color="grey") +
  xlab("Time")  +
  ylab(paste0("Requests per minute (",f.objecttype,")") )

# Rolling average if more than 60 minutes
if (nrow(timeline) > 60) {

  timeline <- timeline %>%
    mutate(perhour = rollmean(n, 60, na.pad=TRUE,align = "right")) %>%
    na.omit()

  pl <- pl +
    geom_line(aes(x=timeslot,y=perhour),data=timeline,color="red")
}
print(pl)

return(pl)
}
