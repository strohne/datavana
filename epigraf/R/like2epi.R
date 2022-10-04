#
# Aufbereitung der Posts & Kommentare fÃ¼r den Epi-Import
# v0.1
#

# Packages ----

#' @import tidyverse
#' @import jsonlite

#
# Hilfsfunktionen ----
#

# rm(list=ls())
# source("../../0_helper/likehelper.R")



#
# Epi functions ----
#


# Create a clean IRI
#'
#' @param table The table name
#' @param type If NA, the type will be omitted.
#' @param fragment The IRI fragment that will be cleaned
#' @export
epi_create_iri <- function(table, type, fragment) {
  paste0(
    table, "/",
    ifelse(is.na(type),"",paste0(type, "/")),
    str_to_lower(str_remove_all(fragment,"[^a-zA-Z0-9_-]"))
  )
}


# Anonymisieren
#' @export
#' @param msg Dataframe with the columns:
#'            platform, author_id, ...
#' @return Dataframe with the columns platform, author_id, author_name, ...
sm_pseudonyms <- function(msg) {

  letters.consonant <- c("b","c","d","f","g","h","j","k","l","m","n","p","q","r","s","t","v","w","x","z")
  letters.vocal <- c("a","e","i","o","u")

  authors <- msg %>%
    filter(!is.na(author_id)) %>%
    distinct(platform,author_id)

  set.seed(42)
  authors <- authors %>%
    slice_sample(prop=1.0) %>%
    mutate(
      .author_id = row_number(),
      .author_name = paste0(
        sample(str_to_upper(letters.consonant),size = nrow(.),replace=T),
        sample(letters.vocal,size = nrow(.),replace=T),

        sample(letters.consonant,size = nrow(.),replace=T),
        sample(letters.vocal,size = nrow(.),replace=T),

        sample(letters.consonant,size = nrow(.),replace=T),
        sample(letters.vocal,size = nrow(.),replace=T)
      )
    )


  msg %>%
    left_join(authors,by=c("platform","author_id")) %>%
    mutate(author_id=.author_id) %>%
    mutate(author_name=.author_name) %>%
    select(-.author_id,-.author_name)
}


# Create project rows
#'
# Creates a project row for each sample
#' @export
#' @param msg Dataframe with the columns:
#'            sample_name, ...
sm_create_projects <- function(msg) {

  msg %>%
    distinct(sample_name) %>%
    mutate(
      table="projects",
      id = epi_create_iri("projects",NA, sample_name),
      iri=id
    ) %>%
    select(table,id,iri,name=sample_name)

}


# Create property items from a column with semicolon separated values
#' @export
#' @param msg Dataframe
sm_unnest_property_items <- function(msg, col_id, sortno, first=F) {

  col_id <- enquo(col_id)

  msg %>%
    filter(!is.na(!!col_id)) %>%
    separate_rows(!!col_id, sep=";") %>%
    filter(!!col_id != "") %>%
    sm_create_property_items(!!col_id, sortno, first)

}

# Create properties from a column with semicolon separated values
#' @export
#' @param msg Dataframe
sm_unnest_property_properties <- function(msg, col_id) {

  col_id <- enquo(col_id)

  msg %>%
    filter(!is.na(!!col_id)) %>%
    separate_rows(!!col_id, sep=";") %>%
    filter(!!col_id != "") %>%
    sm_create_properties(!!col_id,!!col_id)

}

# Creates an article row from the first message in each thread
#' @export
#' @param msg Dataframe with the following columns:
#'            platform, tree_thread, tree_pos,
#'            caption, text,
#'            sample_name, sample_no
sm_create_articles <- function(msg) {

  firstmsg <- msg %>%
    group_by(platform, tree_thread) %>%
    arrange(tree_pos) %>%
    slice_head(n=1) %>%
    ungroup()

  pad_size = str_length(as.character(max(msg$sample_no, na.rm=T)))

  numbers =  msg %>%
    filter(!is.na(sample_no)) %>%
    distinct(platform, tree_thread, sample_no) %>%

    group_by(platform, tree_thread) %>%
    arrange(sample_no) %>%
    summarize(number = paste0(
      str_pad(sample_no, pad_size,"left",pad="0"),
      collapse = " "
    )) %>%
    ungroup()

  firstmsg %>%
    mutate(
      table="articles",
      type="sm-thread",
      doc_id= paste0(platform,"-",tree_thread),

      id = epi_create_iri("articles","sm-thread",doc_id),
      iri = id,

      projects_id= epi_create_iri("projects",NA, sample_name),
      name = ifelse(is.na(caption),str_trunc(text, 50, "right"),caption),
    ) %>%

    left_join(numbers,by=c("platform","tree_thread")) %>%
    select(table,id,type,projects_id,name,number,iri)
}


# Creates a section for each message in a thread
#' @export
#' @param msg Dataframe with the following columns:
#'            platform, tree_thread, tree_pos,
#'            caption, text,
#'            sample_name, sample_no
sm_create_sections <- function(msg) {

  msg <- msg %>%

    mutate(
      doc_id= paste0(platform,"-",tree_id),
      type= paste0("sm-", msg_type),
      iri = epi_create_iri("sections", type, doc_id)
    ) %>%
    distinct(iri, .keep_all=T)

  msg %>%
    group_by(platform, tree_thread) %>%
    arrange(tree_pos) %>%
    ungroup() %>%

    mutate(
      table="sections",

      id = iri,
      sortno = tree_pos,

      articles_id= epi_create_iri("articles","sm-thread",paste0(platform,"-",tree_thread)),

      name=paste0(
        "#", tree_pos, " ",
        str_to_title(msg_type),
        " from ", author_name
      )

    ) %>%
    left_join(
      select(msg,platform,tree_id,parent_id=iri),
      by=c("platform","tree_parent"="tree_id")
    ) %>%
    select(table, id,parent_id,sortno, type,articles_id,name, alias=sample_no, iri)
}


# Create sections_id, articles_id, type and sortno fields
#' @export
#' @param msg
sm_create_items <- function(msg, itemtype, sortno) {

  msg %>%

    mutate(
      table="items",
      sections_id = epi_create_iri(
        "sections",
        paste0("sm-", msg_type),
        paste0(platform,"-",tree_id)
      ),
      articles_id= epi_create_iri(
        "articles",
        "sm-thread",
        paste0(platform,"-",tree_thread)
      ),
      type=itemtype,
      sortno=sortno
    )
}

# Create items with the message content
#' @export
#' @param msg Dataframe
sm_create_text_items <- function(msg, sortno) {

  msg %>%

    sm_create_items("sm-text",sortno) %>%
    select(
      table,articles_id, sections_id, sortno, type,
      value=caption,
      content=text,
      date_value = created,
      source_from = link
    )
}


# Create items from caption and text for the full text search
#' @export
#' @param msg Dataframe
sm_create_search_items <- function(msg, sortno) {

  msg %>%
    sm_create_items("sm-search",sortno) %>%
    unite(content, caption, text, sep =" ", na.rm = T) %>%
    select(table, articles_id, sections_id, sortno, type, content)
}



# Create items containing metrics
#' @export
#' @param msg Dataframe
sm_create_metrics_items <- function(msg, sortno) {

  msg %>%
    sm_create_items("sm-metrics",sortno) %>%
    select(table,sortno, articles_id, sections_id, type,starts_with("count_")) %>%

    pivot_longer(starts_with("count_")) %>%
    filter(!is.na(value)) %>%
    mutate(name = str_remove(name,"^count_")) %>%


    group_by(table, sortno, articles_id, sections_id, type) %>%
    summarise(metrics = map2(list(value), list(name), ~set_names(.x, .y))) %>%
    ungroup() %>%

    mutate(metrics = map(metrics,~as.list(.))) %>%
    mutate(metrics = map_chr(metrics, toJSON, auto_unbox=T, na="null")) %>%

    mutate(sortno=sortno) %>%

    select(table, articles_id, sections_id, sortno, type, content=metrics)
}

#' Create items linked to a property
#' @export
#' @param msg Dataframe
sm_create_property_items <- function(msg, col, sortno, first=T) {

  col <- enquo(col)

  if (first) {
    msg <- msg %>%
      group_by(platform, tree_thread) %>%
      arrange(tree_pos) %>%
      slice_head(n=1) %>%
      ungroup()

  }

  msg %>%
    sm_create_items(paste0("sm-",as_label(col)), sortno) %>%
    filter(!is.na(!!col)) %>%
    mutate(properties_id = epi_create_iri(
      "properties",
      paste0("sm-",as_label(col)),
      !!col)
    ) %>%
    select(table,articles_id, sections_id, sortno, type, properties_id)
}



#' Create properties from a column
#' @export
#' @param msg Dataframe
sm_create_properties <- function(msg, col_id, col_lemma) {

  col_id <- enquo(col_id)
  col_lemma <- enquo(col_lemma)

  msg %>%
    filter(!is.na(!!col_id)) %>%
    distinct(!!col_id, .keep_all = T) %>%

    mutate(
      table="properties",
      type= paste0("sm-",as_label(col_id)),
      id = epi_create_iri(
        "properties",
        type,
        !!col_id
      ),
      iri=id,
      lemma = !!col_lemma,
      name = !!col_lemma
    ) %>%
    arrange(name) %>%
    select(table, id, type, lemma, name)
}

# Fix xml attributes and entities
#' @export
#' @param data
#' @param column
#' @return
sm_cleanhtml <- function(data, column) {
  column <- enquo(column)

  data %>%
    mutate(!!column := sapply(!!column,unescape_html))
  # mutate(!!column := str_replace_all(
  #   !!column,
  #   c(
  #     "<br>"="<br></br>",
  #     "&emsp;"="&#8195;",
  #     "&ensp;"="&#8194;",
  #     "&nbsp;" = "&#160;",
  #     "&shy;" = "&#173;",
  #     "&amp;" = "&"
  #   )
  # ))

}

# Remove HTML entities
#' @export
unescape_html <- function(str){
  if (is.na(str)) {
    return (str)
  } else {
    return (xml2::xml_text(xml2::read_html(paste0("<x>", str, "</x>"))))
  }
}



#' Convert a social media dataset from its canonical form
#' to the form expected by Epigraf
#' 
#' @param data Datafram containing a social media dataset in its canonical form
#' @return Dataframe containing project, articles, sections, items and properties rows
#' @export
sm_canonical2epi <- function(data) {
  
  # Create structure
  epi_projects <- sm_create_projects(threads)
  epi_articles <- sm_create_articles(threads)
  epi_sections <- sm_create_sections(threads)
  
  # Create content
  epi_items  <- bind_rows(
    sm_create_text_items(threads, 1),
    sm_create_search_items(threads, 2),
    sm_create_metrics_items(threads, 3),
    
    sm_create_property_items(threads, platform, 4, T),
    sm_create_property_items(threads, seed_domain, 5, T),
    sm_create_property_items(threads, author_id, 6, F),
    sm_unnest_property_items(threads, tags, 7)
  )
  
  # Create properties
  epi_props  <- bind_rows(
    sm_create_properties(threads, platform, platform),
    sm_create_properties(threads, seed_domain, seed_domain),
    sm_create_properties(threads, author_id,author_name),
    sm_unnest_property_properties(threads, tags)
  )
  
  
  # Bind and return
  epi <- bind_rows(
    epi_projects,
    epi_articles,
    epi_sections,
    epi_items,
    epi_props
  )
  
  return(epi)
}
