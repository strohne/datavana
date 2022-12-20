#' Get the Object IDs  and number of parent nodes
#' @import dplyr
#' @description `fp_getparents()` can be used to get details about the parents.
#' @param data the loaded data from Facepager
#' @param level=0 the node level
#' @return a tibble with 3 columns: parent_id, parent_object, parent_no
#' @examples
#' data <- fp_read_csv2(system.file("extdata", "example.csv", package = "facepager"))
#' fp_getparents(data)
#' @export
fp_getparents <- function(.data, level=0) {


  # Object ID and number of parent nodes
  parents <- data %>%
    dplyr::filter(level == !!level,
                  object_type %in% c("data","seed"),
                  query_status %in% c("","fetched (200)")) %>%
    group_by(parent_id) %>%
    mutate(no = row_number()) %>%
    ungroup()



  if ("file" %in% colnames(data))  {
    parents <- parents %>%
      select(file,parent_id=id,parent_objectid = objectid,parent_no=no) %>%
      mutate(file=str_replace(file,"../databases/",""))
  }
  else {
    parents <- parents %>%
      select(parent_id=id,parent_objectid = objectid,parent_no=no)
  }


  return (parents)
}


#' get tags (urls, mentions, hashtags)
#' @import tokenizers
#' @param data the loaded data from Facepager
#' @param col_text the column from which the tags should be extracted
#' @param prefix=NA when transformatting, the values are not prefixed with a character
#' @return
#' @examples
#' @export
fp_extracttags <- function(data, col_text, prefix=NA) {
  col_text <- enquo(col_text)

  text <- data %>%
    select(!!col_text)

  # Length
  text <- text %>%
    rowwise() %>%
    mutate(
      msg_length_chars = str_length(!!col_text),
      msg_length_words = length(tokenize_words(!!col_text)[[1]][!is.na(!!col_text)])
    )


  # URLs (extract and remove from text)
  text <- text %>%
    mutate(urls=str_extract_all(!!col_text,"http\\S+")) %>%
    mutate(!!col_text := str_remove_all(!!col_text,"http\\S+")) %>%

    rowwise() %>%
    mutate(
      count_urls=length(unlist(urls)),
      urls=paste0(unlist(urls),collapse=";")
    ) %>%
    ungroup()


  # Hashtags
  text <- text %>%
    mutate(tags=str_extract_all(!!col_text,"(?<![\\S])#\\S+")) %>%
    rowwise() %>%
    mutate(
      count_tags=length(unlist(tags)),
      tags=paste0(unlist(tags),collapse=";")
    ) %>%
    ungroup()


  # Mentions
  text <- text %>%
    mutate(mentions=str_extract_all(!!col_text,"(?<![\\S])@\\S+")) %>%
    rowwise() %>%
    mutate(
      count_mentions=length(unlist(mentions)),
      mentions=paste0(unlist(mentions),collapse=";")
    ) %>%
    ungroup()


  text <- select(text,tags,urls,mentions,
                 count_tags,count_urls,count_mentions,
                 msg_length_chars,msg_length_words)

  if (!is.na(prefix))
    text <- rename_all(text,~paste0(prefix,.))

  data <- data[!(colnames(data) %in% colnames(text))]
  data <- bind_cols(data,text)

  return(data)

}


#' Get the collected data per Object ID
#'
#' @import dplyr
#' @param .data the loaded data from Facepager
#' @param col_target=NA coverage per object ID
#' @param col_paging=NA offcuts and last cursor per object ID
#' @param col_created=NA first and last date per object ID
#' @param timestamp on Twitter, the timestamp must be parsed (timestamp == "twitter"),
#'                  on Instagram, the timestamp must first be converted to a date (timestamp == TRUE)
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

#' Helper function for fp_children()
#' @rdname fp_children
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


#' Helper function for fp_children()
#' @rdname fp_children
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
#' @rdname fp_children
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


#' Get the number of childnodes for each node shown in Facepager
#'
#' @import ggplot2
#' @import tidyr
#' @import dplyr
#' @param data the loaded data from Facepager
#' @param .global=T ...
#' @param .maxlevel the node level
#' @return a tibble with 4 columns: parent_id, parent_no, parent_objectid, children
#' @return a boxplot
#' @examples
#' @export
fp_children_bynode <- function(data, .global=T, .maxlevel=1) {
  # Für jeden Knoten so wie in Facepager angezeigt die Zahl
  # der Unterknoten bestimmen.
  # parent_no gibt die Position des Datensatzes in Facepager an
  parents <- fp_getparents(data)

  children.bynode <- data %>%
    dplyr::filter(object_type == "data",level == .maxlevel) %>%
    count(parent_id) %>% rename(children = n) %>%
    right_join(parents,by=c("parent_id")) %>%
    replace_na(list(children = 0)) %>%
    select(parent_id,parent_no,parent_objectid,children)

  .pl <- ggplot(children.bynode,aes(y=children+1,x="")) +
    geom_boxplot() +
    scale_y_log10() +
    coord_flip()
  print(.pl)

  result <- list("children.bynode" = children.bynode)
  if (.global)
    list2env(result, envir = .GlobalEnv)

  return(result)
}


#' Count the number of different children per Object ID (without duplicates).
#'
#' @import stringr
#' @import dplyr
#' @param .data the loaded data from Facepager
#' @param .parents=NA ensures the usage of the parent nodes
#' @param .silent=FALSE standard that error messages are displayed
#' @param level=0 the node level
#' @return a tibble with 4 columns: parent_objectid, children, parent_no, file
#' @examples
#' @export
fp_childcount <- function(.data, .parents=NA, .silent=FALSE, level=0) {
  # .data <- data
  # .parents <- NA
  # level=0

  if (is.na(.parents)) {
    .parents <- fp_getparents(.data,level=!!level)
  }

  if (! ("file" %in% colnames(.parents))) {
    .parents <- mutate(.parents,file = "")
  }

  if (("file" %in% colnames(.data))) {
    .data$file <- NULL
  }

  .parents <- .data %>%
    dplyr::filter(object_type %in% c("data","unpacked"),query_status=="fetched (200)") %>%
    right_join(.parents,by=c("parent_id")) %>%
    group_by(parent_objectid)  %>%
    summarize(
      children = n_distinct(na.omit(objectid)),
      parent_no = paste0(unique(str_pad(parent_no,5,"left","0")),collapse=","),
      file = paste0(unique(file) ,collapse=",")
    ) %>%
    ungroup()
}

