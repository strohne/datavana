#
# This script provides functions to analyze the database files of Facepager
#




#
# Packages ---
#

library(tidyverse)

library(RSQLite)
library(jsonlite)
library(tidyjson)
library(magrittr)
library(lubridate)
library(tidygraph)
library(zoo)
library(rlang)
library(fuzzyjoin)

library(tidytext)
library(tokenizers)
library(SnowballC)

theme_set(theme_bw())

# Setup parallel processing
library(foreach)
library(progressr)
library(doFuture)

registerDoFuture()
handlers(list(handler_progress(format="[:bar] :percent ELA: :elapsed ETA: :eta")))

#
# Functions ----
#


fb_read_csv2 <- function(filename) {
  read_csv2(filename,na="None")
}

# Function to read JSON from text
fp_from_ndjson <- function(data) {
  data[data == "[]"] = "{}"
  data[is.na(data)] = "{}"
  
  jsonlite::stream_in(textConnection(data))
}

# Function to convert JSON in the response to columns of a data frame
fp_parse_response_data <- function (nodes, plain = F, prefix="response.") {
  responses = fp_from_ndjson(nodes$response)
  
  responses = jsonlite::flatten(responses,recursive = T)
  # if (plain) {
  #   responses <- select_if(responses,~!is.data.frame(.))
  #   responses <- select_if(responses,~!is.list(.))
  # }
  
  colnames(responses) = paste0(prefix,colnames(responses))
  nodes = bind_cols(select(nodes,-response),responses)
  rm(responses)  
  nodes
}

fp_get_response_value <- function(nodes, col, .split=TRUE,.progress=NULL) {
  
  if (.split) {
    
    nodes <- nodes %>%
      mutate(chunk = row_number() %/% 1000 ) %>% 
      split(.$chunk)
    
    .progress <- progress_estimated(length(nodes),min_time = 1)
    .progress$print()
    
    nodes <- nodes %>%
      purrr::map(fp_get_response_value,col,.split=FALSE,.progress=.progress) %>% 
      unlist()
    
    .progress$stop()$print()
    cat("\n")
    
  } else {
    col_js <- do.call(jstring,as.list(strsplit(col, ".",fixed=T)[[1]]))
    nodes <- nodes$response %>% 
      spread_values(value=col_js) %>% extract2(2)
    
    if (!is.null(.progress))
      .progress$tick()$print()
  }  
  
  return(nodes)
}



# Function to load data from the database
fp_load_nodes <- function(dbname, fields = '*',.progress=NULL) {
  db.con = dbConnect(RSQLite::SQLite(), dbname=dbname,flags=SQLITE_RO)
  
  statement = paste0('select ',fields,' from Nodes')
  db.nodes = dbGetQuery( db.con,statement )
  dbDisconnect(db.con)
  
  data <- as_tibble(db.nodes)
  
  if (!is.null(.progress))
    .progress$tick()$print()  
  
  return (data)
}

#
# Load data ----
#


# shard: prefix ids to load multiple databases
fp_loaddatabase <- function(filename,shard=NA) {
  
  # Load data
  fields <- "objectid,objecttype,id,parent_id,level,childcount,
             querystatus,querytype,querytime,response"
  data <- fp_load_nodes(filename, fields)
  
  if (!is.na(shard)) {
    data <- data %>% 
      mutate(id=paste0(shard,"_",id),
             parent_id=ifelse(is.na(parent_id),NA,paste0(shard,"_",parent_id)))
  }  
  
  
  return (data)
}

fp_loaddatabases <- function(filenames, fields=NULL) {
  
  # Load data
  if (is.null(fields)) {
    fields <- "objectid,objecttype,id,parent_id,level,childcount,
               querystatus,querytype,querytime,response"
  }  
  
  .progress <- progress_estimated(length(filenames),min_time = 1)
  .progress$print()
  
  shard=ifelse(length(filenames) > 0,"file",NULL)
  data <- map_df(filenames,fp_load_nodes, fields,.id=shard,.progress=.progress)
  
  if (length(filenames) > 0) {
    data <- data %>% 
      mutate(id=paste0(file,"_",id),
             parent_id=ifelse(is.na(parent_id),NA,paste0(file,"_",parent_id)))
  }  
  
  
  .progress$stop()$print()
  
  cat("Loaded ",length(filenames)," databases.\n")
  data %>% 
    count(file) %>% 
    print()
  
  return (data)
}

fp_getparents <- function(data, level=0) {
  
  
  # Object ID and number of parent nodes
  parents <- data %>% 
    filter(level == !!level,
           objecttype %in% c("data","seed"),
           querystatus %in% c("","fetched (200)")) %>% 
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

#
# Status  ----
#

fp_status <- function(data) {
  data %>% 
    count(level,querytype,querystatus,objecttype)
}

#
# Timing - requests per minute ----
#

fp_timing <- function(data,f.objecttype="offcut",f.querystatus="fetched (200)") {
  
  
  timing <- data %>% 
    filter(querystatus == f.querystatus) %>% 
    filter(objecttype == f.objecttype) %>% 
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

#
# Duplicates ----
#

fp_duplicates <- function(data, level=0) {
  
  # Duplikate auf Elternebene
  parents <- fp_getparents(data,!!level)
  dupl.parents <- parents %>% 
    count(parent_objectid,sort=T) 
  
  cat("Auswertung der Duplikate bei den Seeds:\n")
  cat("- ",sum(dupl.parents$n)," Elternknoten insgesamt\n")
  cat("- ",nrow(dupl.parents)," eindeutige Elternknoten\n")
  
  dupl.parents <- dupl.parents %>% 
    filter(n > 1)
  
  cat("- ",nrow(dupl.parents)," mehrfache Elternknoten\n")
  
  if (nrow(dupl.parents) > 0)
    print(dupl.parents)
  
  
  # Bei gleichen Eltern, auf zweiter Ebene
  dupl.children <- data %>% 
    filter(objecttype %in% c("data","unpacked"),querystatus=="fetched (200)",level==!!level+1) %>% 
    count(objectid,parent_id, sort=T) %>%  #level,querytype
    inner_join(parents,by=c("parent_id"))
  
  dupl.children.missing <- parents %>% 
    anti_join(dupl.children,by=c("parent_id"))
  
  cat("Auswertung der Duplikate innerhalb der Seeds:\n")
  cat("- ",nrow(dupl.children.missing)," Seeds ohne Kindknoten\n")
  cat("- ",sum(dupl.children$n)," Kindknoten insgesamt\n")
  cat("- ",nrow(dupl.children)," eindeutige Kindknoten\n")
  
  dupl.children <- dupl.children %>% 
    filter(n > 1)
  
  cat("- ",nrow(dupl.children)," mehrfache Kindknoten\n")
  if (nrow(dupl.children) > 0)
    print(select(dupl.children,objectid,n,file,parent_objectid))
  
  
  # Unabhängig von Eltern, auf gleicher Ebene
  dupl.all <- data %>% 
    filter(objecttype %in% c("data","unpacked"),querystatus=="fetched (200)",level==!!level+1) %>% 
    count(objectid,sort=T)   #level,querytype,
  
  
  cat("Auswertung der Duplikate bei allen Kindknoten:\n")
  cat("- ",sum(dupl.all$n)," Kindknoten insgesamt\n")
  cat("- ",nrow(dupl.all)," eindeutige Kindknoten\n")
  
  
  dupl.all <- dupl.all %>% 
    filter(n > 1)
  
  cat("- ",nrow(dupl.all)," mehrfache Kindknoten\n")
  
  if (nrow(dupl.all) > 0)
    print(dupl.all)
  
  result <- list("dupl.parents"=dupl.parents,
                 "dupl.children"=dupl.children,
                 "dupl.all"=dupl.all)
  
  
  invisible(result)
}

#
# Kinder  ----
#

fp_children_bynode <- function(data, .global=T, .maxlevel=1) {
  # Für jeden Knoten so wie in FAcepager angezeigt die Zahl 
  # der Unterknoten bestimmen. 
  # parent_no gibt die Position des DAtensatzes in Facepager an
  parents <- fp_getparents(data)
  
  children.bynode <- data %>% 
    filter(objecttype == "data",level == .maxlevel) %>% 
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


#' Die Anzahl unterschiedlicher Kinder je Object ID auszählen (ohne Duplikate)
fp_childcount <- function(.data, .parents=NA, .silent=FALSE,level=0) {
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
    filter(objecttype %in% c("data","unpacked"),querystatus=="fetched (200)") %>% 
    right_join(.parents,by=c("parent_id")) %>% 
    group_by(parent_objectid)  %>% 
    summarize(
      children = n_distinct(na.omit(objectid)),
      parent_no = paste0(unique(str_pad(parent_no,5,"left","0")),collapse=","),
      file = paste0(unique(file) ,collapse=",") 
    ) %>% 
    ungroup()
  
  
  # Auszählen
  if (!.silent) {
    cat("Children per parent (absolute number)\n")
    print(summary(.parents$children))
    cat("\n")  
  }
  
  invisible(.parents)
}

#' Anzahl der offcuts und letzter cursor je object ID
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

#' Erstes und letztes Datum je Object ID
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

#
# Erhobene Daten je Object ID auszählen ----
#

# Die Zielvorgabe bestimmen (setzt voraus, dass eine Spalte mit der Zielvorgabe vorhanden ist.
# col_target bei Instagram comments: response.comments
# col_target bei Instagram likes: response.likes_count
# col_target bei Instagram follower: response.count_followers
# col_target bei Instagram followees: response.count_followees

# Den Schlüssel für die Paginierung  anpassen.
# col_paging bei Instagram posts: response.data.user.edge_owner_to_timeline_media.page_info.end_cursor
# col_paging bei Instagram comments: response.data.shortcode_media.edge_media_to_parent_comment.page_info.end_cursor
# col_paging bei Instagram likes: response.data.shortcode_media.edge_liked_by.page_info.end_cursor

# Den Schlüssel für das Erstelldatum anpassen
# col_created bei Instagram posts "response.node.taken_at_timestamp"
# col_created bei Instagram comments "response.node.created_at"
# timestamp = T

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


#
# Dateien abgleichen (zwischen Facepager und Ordner) ----
#

fp_comparefiles <- function(data, folder="../profiledata/") {
  
  files.facepager <- data %>% 
    filter(querystatus=="fetched (200)",objecttype=="data") %>%
    fp_parse_response_data() %>% 
    select(objectid,filename=response.filename)
  
  files.downloaded <- tibble(filename=list.files(folder))
  
  cat("Missing in the folder...\n")
  missing.infolder <- files.facepager %>% 
    anti_join(files.downloaded) 
  print(missing.infolder)
  
  cat("Missing in the database...\n")
  missing.infacepager <-  files.downloaded %>% 
    anti_join(files.facepager) 
  print(missing.infacepager)
  
  result <- list("infolder"=missing.infolder,
                 "infacepager"=missing.infacepager) 
  
  
  invisible(result)
}



#
# Extract Hashtags, mentions and URLs ----
#

# Siehe auch https://stackoverflow.com/a/2528131 
# Twitters Verfahren: https://github.com/twitter/twitter-text
# Interessant ist auch: https://gist.github.com/CateGitau/05e6ff80b2a3aaa58236067811cee44e#file-twitter-text-analysis-r-L51

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



#' Create IDs to avoid collision between levels
#'
#' @param id Column holding the ID of the node
#' @param parent Column holding the parent ID of the node
#' @param type Column holding the type of the node. Every level needs a different type
#' @param levels The hierarchy of the types, e.g. c("forum","post","comment","reply")
fp_treeids <- function(.data,id,parent,type,levels) {
  id <- enquo(id)
  parent <- enquo(parent)
  type <- enquo(type)
  
  
  tree <- .data %>% 
    select(plat_id=!!id,plat_parent=!!parent,msg_type=!!type)
  
  # Create IDs in a way multiple levels don't collide
  prefixes <- as.list(str_sub(levels,1,1))
  names(prefixes) <- levels
  
  tree <- tree %>% 
    mutate(tree_id=recode(msg_type,!!!prefixes)) %>% 
    mutate(tree_id=paste0(tree_id,"_",plat_id))
  
  # Create parent IDs in a way multiple levels don't collide
  prefixes <- as.list(str_sub(levels,1,1))
  prefixes[[length(prefixes)]] <- NULL
  names(prefixes) <- levels[2:length(levels)]
  
  tree <- tree %>% 
    mutate(tree_parent=recode(msg_type,!!!prefixes)) %>% 
    mutate(tree_parent=paste0(tree_parent,"_",plat_parent))
  
  tree <- select(tree,tree_id,tree_parent)
  
  # Return
  bind_cols(tree,.data)
}


#' Adds left and right values to the dataframe (modified preorder tree traversal)
#' See https://www.sitepoint.com/hierarchical-data-database-3/

#' @param data dataframe with tree_level, tree_thread, tree_id, tree_parent, created

fp_gettree <- function(data) {
  # Progress
  .maxlevel = max(data$tree_level)
  .minlevel = min(data$tree_level)
  p <- progressr::progressor(steps = (.maxlevel-.minlevel+1) * 2)
  
  # Add descendents
  data$tree_descendants = 0
  for (.level in c(.maxlevel : .minlevel)) {
    p(message=paste0("Level ",.level))
    
    leafs <- data %>%
      filter(tree_level == .level)  %>% 
      mutate(tree_descendants = tree_descendants + 1) %>% 
      group_by(tree_thread,tree_id=tree_parent) %>%
      summarise(tree_leafs = sum(tree_descendants),.groups="keep") %>%
      ungroup(tree_thread,tree_id) 
    
    data <-  data %>%
      left_join(leafs,by=c("tree_thread","tree_id")) %>%
      replace_na(list(tree_leafs=0)) %>% 
      mutate(tree_descendants = tree_descendants + tree_leafs) %>% 
      select(-tree_leafs)
    
  }
  
  
  # Add left
  data <- data %>%
    group_by(tree_thread,tree_parent) %>% 
    arrange(created) %>% 
    mutate(tree_order = row_number()) %>% 
    mutate(tree_rgt = 1 + cumsum(tree_descendants)*2 + (2 * (tree_order-1)) + 1) %>% 
    mutate(tree_lft = tree_rgt - 2*tree_descendants - 1) %>% 
    ungroup(tree_thread,tree_parent) %>% 
    select(-tree_descendants)
  
  
  # Bubble from parents to children  
  .level <- .minlevel
  parents <- data %>%
    filter(tree_level == .minlevel) 
  
  while(nrow(parents) > 0) {
    
    p(message=paste0("Level ",.level))
    .level <- .level + 1
    
    data <- data %>%
      left_join(select(parents,tree_thread,tree_id,tree_parent_lft=tree_lft),
                by=c("tree_thread","tree_parent"="tree_id"))%>% 
      replace_na(list(tree_parent_lft = 0)) %>% 
      mutate(tree_lft = tree_lft + tree_parent_lft) %>% 
      mutate(tree_rgt = tree_rgt + tree_parent_lft) %>% 
      select(-tree_parent_lft)
    
    parents <- data %>%
      semi_join(parents,by=c("tree_thread","tree_parent"="tree_id")) 
    
  }
  
  return(data)
}


#' Adds left and right values to the dataframe (modified preorder tree traversal)
#' See https://www.sitepoint.com/hierarchical-data-database-3/

#' @param col_id Column holding the ID of the node
#' @param col_parent Column holding the parent ID of the node
#' @param col_order Column holding the order of nodes inside of a parent (e.g. a date). Default is col_id

rebuild_tree <- function(data, col_id,col_parent,col_order=NULL) {
  
  # Quoting
  col_id <- enquo(col_id)
  col_parent <- enquo(col_parent)
  col_order <- enquo(col_order)
  if (quo_is_null(col_order)) { col_order <- col_id }
  
  # Prepare temporary columns
  data <- mutate(data,.tree_id=!!col_id)
  data <- mutate(data,.tree_parent=!!col_parent)
  data <- mutate(data,.tree_order=!!col_order)
  
  # Get roots
  roots <- data %>% 
    anti_join(data,by=c(".tree_parent"=".tree_id")) %>%
    semi_join(data,by=c(".tree_id"=".tree_parent")) %>% 
    extract2(".tree_id")
  
  # Progress
  p <- progressr::progressor(steps = length(roots))
  
  for (rootid in roots) {
    p(message=rootid)
    data <- rebuild_tree_mptt(data,rootid)$data
  }
  
  # Remove temporary columns
  data <- data %>% 
    select(-.tree_id,-.tree_parent,-.tree_order)
  
  return(data)
  
}  


#' Recursively called from rebuild_tree
#' See https://www.sitepoint.com/hierarchical-data-database-3/

#' @param data A dataframe with columns .tree_id,.tree_parent and .tree_order
#' @param parentvalue The .tree_id value of the root node
#' @param left The left value of the root node
rebuild_tree_mptt <- function(data, parentvalue, left=1) {
  
  #the right value of this node is the left value + 1
  right = left+1
  
  
  #get all children of this node
  children <-  data[data$.tree_parent==parentvalue,] 
  
  
  if (nrow(children)  > 0) {
    children[order(children$.tree_order),]
    
    for (no in c(1:nrow(children))) {
      
      child <- children[no,]
      
      # recursive execution of this function for each
      # child of this node
      # right is the current right value, which is
      # incremented by the rebuild_tree function
      subtree <-  rebuild_tree_mptt(data,child$.tree_id, right)
      
      
      right <- subtree$right
      data <- subtree$data
    }
  }  
  
  #we've got the left value, and now that we've processed
  #the children of this node we also know the right value
  data[data$.tree_id==parentvalue,'tree_lft'] <- left
  data[data$.tree_id==parentvalue,'tree_rgt'] <- right
  
  
  #return the right value of this node + 1
  return (list(data=data, right=right+1))
}




#' Transfer tree_thread (ID of root node) to all children of root nodes 
#' 
#' @param data All nodes (e.g. posts and comments)
#' @param col_id The column holding IDs of the nodes
#' @param col_parent The column holding IDs of the parent nodes
#' @param col_order The column used for ordering the nodes inside of a level
#' 
fp_getthreads <- function(data,col_id,col_parent,col_order=NULL) {
  # Quoting
  col_id <- enquo(col_id)
  col_parent <- enquo(col_parent)
  col_order <- enquo(col_order)
  if (quo_is_null(col_order)) {
    col_order <- col_id
  }
  
  # Prepare columns
  data <- mutate(data,.tree_id=!!col_id)
  data <- mutate(data,.tree_parent=!!col_parent)
  
  # Prepare roots
  roots <- data %>% 
    anti_join(data,by=c(".tree_parent"=".tree_id")) %>% 
    mutate(tree_thread=.tree_id,tree_level=0,tree_order=0)
  
  # First level  
  .level <- 1
  children <- data %>% 
    inner_join(select(roots,.tree_id,tree_thread),by=c(".tree_parent"=".tree_id")) %>% 
    mutate(tree_level=.level) %>% 
    
    group_by(.tree_parent) %>% 
    arrange(!!col_order) %>% 
    mutate(tree_order=row_number()) %>% 
    ungroup()
  
  cat("Level ",.level,". ",sep="")
  cat(nrow(children)," nodes addes.\n\n",sep="")
  
  while (TRUE) {
    
    .level <- .level + 1
    cat("Level ",.level,". ",sep="")
    
    children.next <- data %>% 
      anti_join(children,by=c(".tree_id")) %>% 
      inner_join(select(children,tree_thread,.tree_id,.parent_order=tree_order),by=c(".tree_parent"=".tree_id")) %>% 
      mutate(tree_level=.level) %>% 
      
      group_by(.tree_parent) %>% 
      arrange(!!col_order) %>% 
      mutate(tree_order= row_number()) %>% 
      ungroup() %>% 
      select(-.parent_order)
    
    children <- dplyr::bind_rows(children,children.next)
    
    cat(nrow(children.next)," nodes addes.\n\n",sep="")
    
    if (!nrow(children.next))
      break
  }
  
  bind_rows(roots,children) %>% 
    arrange(tree_thread,tree_order) %>% 
    select(-.tree_id,-.tree_parent)
  
}



#' Creates IDs for threads, adds level and order data
#' 
#' @param .data The dataframe containing hierarchical data
#' @param id The ID of the node
#' @param parent The ID of the parent node
#' @param id The number of the node inside of parents


fp_tree <- function(.data,id,parent,no) {
  id <- enquo(id)
  parent <- enquo(parent)
  no <- enquo(no)
  
  # Seperate roots and leafs
  tree <- .data %>% 
    arrange(!!no) %>% 
    select(!!id,!!parent) %>% 
    distinct()
  
  by = set_names(quo_name(id), quo_name(parent))
  tree.roots <- tree %>% 
    anti_join(tree,by=by)
  
  tree.leafs <- tree %>% 
    anti_join(tree.roots,by=quo_name(id))
  
  rm(tree)
  
  # Create graph
  gr <- tbl_graph(nodes = bind_rows(tree.roots,tree.leafs),edges = tree.leafs) 
  
  # Komponenten, Reihenfolge und Ebene feststellen
  # tree_comp: Nummer des Strangs
  # tree_order: Nummer der Mitteilung im Strang
  # tree_level: Ebene der Mitteilung
  gr <- gr %>% 
    mutate(tree_thread = group_components(type = "weak")) %>% 
    morph(to_components) %>% 
    mutate(tree_order = dfs_rank(root=1,mode="in")) %>% 
    mutate(tree_level = bfs_dist(root=1,mode="in"))   %>% 
    unmorph() 
  
  # Zurüch zu Knotenliste
  tree <- gr %>% 
    as_tibble() %>% 
    arrange(tree_thread,tree_order)
  
  # Join (1. right_index 2. left_index)
  .data %>% 
    left_join(select(tree,-!!parent),by=quo_name(id))  %>% 
    select(starts_with("tree_"),everything()) %>% 
    arrange(tree_thread,tree_order)
  
}

#' #' Extracts the first mention in the text and joins the
#' #' id of the mentioned node by author name
#' fp_mentionids <- function(.data,id,parent,no,author,text) {
#'   
#'   id <- enquo(id)
#'   parent <- enquo(parent)
#'   no <- enquo(no)
#'   author <- enquo(author)
#'   text <- enquo(text)
#'   
#'   mentions <- .data %>% 
#'     mutate(mention_name = str_extract(!!text,"(?<=@)[^ \\(\\)]+")) %>% 
#'     select(!!id,!!parent,!!no,mention_name) 
#'   
#'   #c("tree_parent","mention_name"="author_name")
#'   idx_left <- c(quo_name(parent),"mention_name")
#'   idx_right <- c(quo_name(parent),quo_name(author))
#'   by = set_names(idx_right,idx_left)
#'   
#'   mentions <- mentions %>% 
#'     inner_join(
#'       select(.data,mention_id=!!id,!!parent,mention_order=!!no,!!author),
#'       by=by
#'     ) 
#'   
#'   mentions <- mentions %>% 
#'     group_by(!!id) %>% 
#'     filter(mention_order < !!no) %>% 
#'     filter(row_number()==n()) %>% 
#'     ungroup() %>% 
#'     select(!!id,mention_id)
#'   
#'   
#'   left_join(.data,mentions,by=quo_name(id))
#' }


#' Extracts IDs of mentioned comments by comparing all 
#' author names with the comment text 
#' in all subthreads having the same tree_parent.
fp_mentionids <- function(.data,id,parent,no,author,text) {
  
  id <- enquo(id)
  parent <- enquo(parent)
  no <- enquo(no)
  author <- enquo(author)
  text <- enquo(text)

  
  
  match_identical <- function(v1, v2) {
    v1==v2  
  }
  
  match_higher <- function(v1, v2) {
    v1 > v2
  }
  
  match_regex <- function(v1, v2) {
    stringr::str_detect(v1, stringr::regex(v2, ignore_case = F))
  }
  
  # Select relevant sub threads
  print("-split data into shards")
  shards <- .data %>% 
    filter(!is.na(!!parent)) %>% 
    group_by(!!parent) %>% 
    filter(n() > 1) %>%
    filter(any(str_detect(!!text,"^@"))) %>% 
    ungroup() %>% 
    select(!!id,!!parent,!!no,!!author,!!text) %>% 
    group_split(!!parent)
  
  shards <- sample(shards)
  #summary(unlist(lapply(shards,nrow)))
  
  print(paste0("-start processing ",length(shards)," shards. The workers need time for their warm-up. Progress bar will start as soon as all workers work. "))
  p <- progressor(steps=length(shards))

  # Parallel processing  
  mentions <- foreach(shard=shards,.combine=rbind,.packages=c('dplyr','fuzzyjoin')) %dopar% {
    p()
    
    shard_mentions <- shard %>% 
      mutate(mention_regex=paste0("^\\Q@",!!author," \\E")) %>% 
      select(mention_parent=!!parent,mention_id=!!id,mention_regex,mention_no=!!no)
    
    idx_left <- c(quo_name(no),quo_name(text))
    idx_right <- c("mention_no","mention_regex")
    by = set_names(idx_right,idx_left)
    
    shard_mentions <- fuzzy_join(
      shard,shard_mentions,
      by = by, #c(!!no="mention_no",!!text="mention_regex")
      match_fun = c(match_higher,match_regex),
      mode = "inner"
    )
    
    shard_mentions <- shard_mentions %>% 
      group_by(!!parent,!!id) %>% 
      slice_max(order_by = mention_no,n=1) %>% 
      ungroup() %>% 
      
      distinct(!!id,mention_id) 
    
    return(shard_mentions)
  }
  
  print("-join mention ids")
  
  mentions <- mentions %>% 
    distinct(!!id,mention_id)
  
  .data <- .data %>% 
    left_join(mentions,by=quo_name(id))
  
  return (.data)
  
}


fp_addseed <- function(.data) {
  .data %>% 
    filter(object_type=="data",query_status=="fetched (200)") %>% 
    inner_join(.data %>% filter(level == 0) %>% select(seed = object_id,id),by=c("parent_id"="id")) %>% 
    distinct(seed,object_id,.keep_all = T)
}

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
      filter((!!col_created >= from) & (!!col_created < to)) 
  }    
  
  
  # Aggregate
  seeds <- .data %>%
    filter(!is.na(!!col_created)) %>% 
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
    filter(!is.na(!!col_created)) %>% 
    filter(!!col_created >= (date(min(!!col_created))+1 )) %>%
    filter(!!col_created < (date(max(!!col_created)))) %>% 
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

#
# Funktionen für TEXT
#

count_words <- function(text) {
  tokenize_words(text)  %>% 
    lapply(na.omit) %>% 
    lapply(length) %>% 
    unlist()
}
