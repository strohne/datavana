#' get the collected data per Object ID
#' @import dplyr
#' @param .data the loaded data from Facepager
#' @param col_target=NA coverage per object ID
#' @param col_paging=NA offcuts and last cursor per object ID
#' @param col_created=NA first and last date per object ID
#' @param timestamp on Twitter, the timestamp must be parsed (timestamp == "twitter"),
#' on Instagram, the timestamp must first be converted to a date (timestamp == TRUE)
#' @param level=0 the node level
#' @return a tibble with ?
#' @examples
#' @export
fp_children <- function(.data, col_target=NA,col_paging=NA,col_created=NA,timestamp=F,level=0) {

  parents <- fp_childcount(.data,level=!!level)

  parents <- parents %>%
    left_join(fp_coverage(.data, col_target=col_target,level=!!level),by="parent_objectid")

  parents <- parents %>%
    left_join(fp_paging(.data, col_paging=col_paging,level=!!level),by="parent_objectid")

  parents <- parents %>%
    left_join(fp_created(.data, col_created=col_created,timestamp,level=!!level),by="parent_objectid")

  invisible(parents)
}

#' helper function for fp_children()
#' @export
fp_coverage <- function(.data, col_target, .parents=NA, level=0) {

  if (is.na(.parents)) {
    .parents <- fp_childcount(.data,.silent = T,level=!!level)
  }

  if (is.na(col_target))
    return (invisible(select(.parents,parent_objectid)))


  # Mit Zielvorgabe vergleichen
  target <- .data %>%
    filter(level==!!level) %>%
    select(parent_objectid=objectid,response)  %>%
    mutate(children.target=fp_get_response_value(.,col_target)) %>%
    mutate(children.target= as.numeric(children.target)) %>%
    distinct(parent_objectid,children.target)

  # Ist und Soll-Datensätze verbinden
  .parents <- .parents  %>%
    left_join(target,by="parent_objectid") %>%
    mutate(children.missing = children.target - children) %>%
    mutate(children.rate = children.missing / children.target) %>%
    arrange(parent_no)


  # Auszählen
  cat("Missing children (absolute number)\n")
  print(summary(.parents$children.missing))
  cat("\n")

  cat("Missing children (percent)\n")
  print(summary(.parents$children.rate))
  cat("\n")


  cat("Children (head)\n")
  head(arrange(.parents,children.rate,children.missing,children.target)) %>% show()
  cat("\n")

  cat("Children (tail)\n")
  tail(arrange(.parents,children.rate,children.missing,children.target)) %>% show()
  cat("\n")

  cat("Collected ",sum(.parents$children)," of ",
      sum(.parents$children.target), "children. ",
      sum(.parents$children.missing)," are missing.\n\n")

  .parents <- .parents %>%
    select(parent_objectid,children.target,children.missing,children.rate)

  invisible(.parents)
}


#' helper function for fp_children()
#' @export
fp_paging <- function(.data, col_paging,.parents=NA,level=0) {

  # Den Wert für die Fortsetzung der Paginierung auslesen
  if (is.na(.parents)) {
    .parents <- fp_getparents(.data,level=!!level)
  }

  if (is.na(col_paging))
    return (invisible(distinct(select(.parents,parent_objectid))))

  offcut <- .data %>%
    filter(objecttype == "offcut",querystatus=="fetched (200)") %>%
    inner_join(.parents,by=c("parent_id")) %>%
    group_by(parent_objectid)  %>%
    mutate(offcuts = n()) %>%
    filter(row_number()==n()) %>%
    ungroup()

  offcut <- offcut %>%
    mutate(cursor=fp_get_response_value(.,col_paging)) %>%
    select(parent_objectid,offcuts,cursor)

  # Fehlende ergänzen
  .parents <- .parents %>%
    distinct(parent_objectid) %>%
    left_join(offcut,by="parent_objectid") %>%
    replace_na(list("offcuts"=0))

  cat("Paginierung:\n")
  print(count(.parents,cursor,sort=T))
  cat("\n")


  invisible(.parents)
}


#' helper function for fp_children()
#' @export
fp_created <- function(.data, col_created,timestamp=FALSE,.parents=NA,level=0) {
  # Erstes und letztes Post auslesen
  if (is.na(.parents)) {
    .parents <- fp_getparents(.data,level=!!level)
  }

  if (is.na(col_created))
    return (invisible(distinct(select(.parents,parent_objectid))))

  posts <- .data %>%
    filter(objecttype %in% c("data","unpacked"),querystatus=="fetched (200)") %>%
    inner_join(.parents,by=c("parent_id"))

  posts <- posts %>%
    mutate(created=fp_get_response_value(.,col_created)) %>%
    select(parent_objectid,created)

  # Bei Twitter muss die Zeitangabe geparsed werden
  # Bei Instagram muss der Timestamp erst in ein Datum konvertiert werden
  if(timestamp == "twitter") {
    posts <-  posts %>%
      mutate(created = parse_date_time(created, orders="a b d H:M:S z Y",locale="us"))
  }
  else if(timestamp == TRUE) {
    posts <- posts %>%
      mutate(created = as.POSIXct(as.numeric(created),origin="1970-01-01"))
  }


  # Erster und letzer Posts und Anzahl der Posts je Elternknoten berechnen
  posts <- posts %>%
    group_by(parent_objectid) %>%
    summarize(from = min(created,na.rm=T),
              to=max(created,na.rm = T)) %>%
    ungroup()

  # Fehlende ergänzen
  .parents <- .parents %>%
    distinct(parent_objectid)%>%
    left_join(posts,by="parent_objectid")

  invisible(.parents)
}
