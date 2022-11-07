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
fp_timing <- function(data, f.objecttype="data", f.querystatus="fetched (200)") {


  timing <- data %>%
    dplyr::filter(query_status == f.querystatus) %>%
    dplyr::filter(object_type == f.objecttype) %>%
    mutate(query_time = lubridate::ymd_hms(query_time)) %>%
    mutate(query_minute = lubridate::floor_date(query_time,unit="minute")) %>%
    mutate(query_hour = lubridate::floor_date(query_time,unit="hour")) %>%
    mutate(query_day = lubridate::floor_date(query_time,unit="day"))

  cat("Start: ",format(min(timing$query_time)),"\n")
  cat("End: ",format(max(timing$query_time)),"\n")
  cat("Duration: ",
      as.character(
        lubridate::as.period(lubridate::interval(min(timing$query_time),max(timing$query_time)))
      ),
      "\n")
  cat("Active hours: ",n_distinct(timing$query_hour), " \n")
  cat("Active minutes: ",n_distinct(timing$query_minute), " \n")

  cat("Requests per day (",f.objecttype,"): \n")
  timing %>%
    count(query_day) %>%
    print(n=100)

  timeline <- seq(min(timing$query_minute), max(timing$query_minute), by = "min")
  timeline <- timing %>%
    count(timeslot=query_minute) %>%
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
