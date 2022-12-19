#' Get details of the query
#' @description These details include the numbers of
#' levels, query types, query status and object types.
#' @param data the loaded data from Facepager
#' @return a tibble with 5 columns: level, query_type, query_status, object_type  and n
#' @examples
#' @export
fp_status <- function(data) {
  data %>%
    count(level,query_type,query_status,object_type)
}


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


#' get the number of childnodes for each seed for a certain period of time
#' @import dplyr
#' @importFrom rlang quo_is_null
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


#' helper function for fp_nodesperseed()
#' @import dplyr
#' @export
fp_addseed <- function(.data) {
  .data %>%
    dplyr::filter(object_type=="data",query_status=="fetched (200)") %>%
    inner_join(.data %>% filter(level == 0) %>% select(seed = object_id,id),by=c("parent_id"="id")) %>%
    distinct(seed,object_id,.keep_all = T)
}


#' Evaluate duplicates on different node levels
#' @description With this function duplicates will be extracted on three different levels:
#' 1. at parent level,
#' 2. at children level with same parent/seed
#' 3. at children level independent of parent
#' @param data the loaded data from Facepager
#' @param level the node level
#' @return a list of duplicates at parent level,
#' duplicates within the seeds, duplicates at all childnodes
#' @examples
#' @export
fp_duplicates <- function(data, level=0) {

  # Duplikate auf Elternebene
  parents <- fp_getparents(data,!!level)
  dupl.parents <- parents %>%
    count(parent_objectid,sort=T)

  cat("Auswertung der Duplikate bei den Seeds:\n")
  cat("- ",sum(dupl.parents$n)," Elternknoten insgesamt\n")
  cat("- ",nrow(dupl.parents)," eindeutige Elternknoten\n")

  dupl.parents <- dupl.parents %>%
    dplyr::filter(n > 1)

  cat("- ",nrow(dupl.parents)," mehrfache Elternknoten\n")

  if (nrow(dupl.parents) > 0)
    print(dupl.parents)


  # Bei gleichen Eltern, auf zweiter Ebene
  dupl.children <- data %>%
    dplyr::filter(object_type %in% c("data","unpacked"),query_status=="fetched (200)",level==!!level+1) %>%
    count(objectid,parent_id, sort=T) %>%  #level,querytype
    inner_join(parents,by=c("parent_id"))

  dupl.children.missing <- parents %>%
    anti_join(dupl.children,by=c("parent_id"))

  cat("Auswertung der Duplikate innerhalb der Seeds:\n")
  cat("- ",nrow(dupl.children.missing)," Seeds ohne Kindknoten\n")
  cat("- ",sum(dupl.children$n)," Kindknoten insgesamt\n")
  cat("- ",nrow(dupl.children)," eindeutige Kindknoten\n")

  dupl.children <- dupl.children %>%
    dplyr::filter(n > 1)

  cat("- ",nrow(dupl.children)," mehrfache Kindknoten\n")
  if (nrow(dupl.children) > 0)
    print(select(dupl.children,objectid,n,file,parent_objectid))


  # Unabhängig von Eltern, auf gleicher Ebene
  dupl.all <- data %>%
    dplyr::filter(object_type %in% c("data","unpacked"),query_status=="fetched (200)",level==!!level+1) %>%
    count(objectid,sort=T)   #level,querytype,


  cat("Auswertung der Duplikate bei allen Kindknoten:\n")
  cat("- ",sum(dupl.all$n)," Kindknoten insgesamt\n")
  cat("- ",nrow(dupl.all)," eindeutige Kindknoten\n")


  dupl.all <- dupl.all %>%
    dplyr::filter(n > 1)

  cat("- ",nrow(dupl.all)," mehrfache Kindknoten\n")

  if (nrow(dupl.all) > 0)
    print(dupl.all)

  result <- list("dupl.parents"=dupl.parents,
                 "dupl.children"=dupl.children,
                 "dupl.all"=dupl.all)


  invisible(result)
}
