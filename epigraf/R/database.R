#' Save database connection settings to environment variables.
#' Environment variables are prefixed with "epi_" and used in db_connect()
#' to establish the connection.
#'
#' @param host host
#' @param port port
#' @param username username
#' @param password password
#' @param database database
#' @export
db_setup <- function(host="localhost", port=3306, username="root", password="root", database="") {
  settings <- as.list(environment())
  settings <- setNames(settings, paste0("epi_",names(settings)))
  do.call(Sys.setenv, settings)
}

#' Get a connection to a database
#'
#' Before you can use this function, call db_setup once
#' to set the connection parameters.
#' All parameters are stored in the environment.
#'
#' @param db Name of the database as string.
#'           Leave empty to use the database
#'           name from the environment settings.
#' @export
db_connect <- function(db=NULL) {
  if (missing(db)) {
    db <- Sys.getenv("epi_dbname")
  }

  con <- dbConnect(
    RMySQL::MySQL(),
    host = Sys.getenv("epi_host"),
    port = as.numeric(Sys.getenv("epi_port")),
    username = Sys.getenv("epi_username"),
    password = Sys.getenv("epi_password"),
    dbname = db
  )

  return(con)
}

#' Get the database name from a connection object
#'
#' @param con A connection object
#' @export
db_name <- function(con) {
  na.omit(str_extract(capture.output(summary(con)),"(?<=Dbname: )[^ ]+"))
}


#' Get list of all databases
#'
#' @param epi Only keep databases with the epi-prefix.
#' @export
db_databases <- function(epi = FALSE) {
  con <- db_connect()
  dbs = dbGetQuery(con,"SHOW DATABASES;")
  dbDisconnect(con)
  rm(con)

  if (epi) {
    dbs <- filter(dbs, str_starts(Database,"epi_"))
  }

  return(dbs)
}



#
#' Construct filter conditions for the db_table() function
#'
#' @param table Table name
#' @param field Field name
#' @param value A single value, a list of characters or a list of integers
#' @export
db_condition <- function(table, field, value) {

  # preprocess value(s) for sql
  # check if value is list
  if (is.list(value)){

    # if items in list are numeric --> collapse without ''
    if (all(is.numeric(value))) {
      collapsed_values = paste("(", paste(prop_ids, collapse = ","), ")",sep = "")
    }

    # if items in list are characters --> collapse  ''
    else if (all(is.character(value))) {
      collapsed_values = paste("('", paste(prop_ids, collapse = "','"), "')",sep = "")
    }

    # create statement of type "col in list"
    statement = paste0(table, ".", field, " in ", collapsed_values)

  } else{

    # create statement of type "col = value"
    statement = paste0(table, ".", field, " = ", value)

  }

  return (statement)
}


#
#' Get data from a database table
#'
#' @param table Table name
#' @param db A connection object (object) or the database name (character)
#' @param deleted Deleted records are skipped by default. Set to TRUE, to get all records.
#' @param cond A character or a character vector of conditions, e.g.
#'                   "id = 10"
#' @export
db_table <- function(table, db, deleted=FALSE, cond=c()){
  # Check if db is character --> open db connection
  if (is.character(db)){
    con <- db_connect(db)
  }  else {
    con <- db
  }

  # Construct SQL
  sql <- paste0("SELECT * FROM ", table)

  # Add deleted = 0 to the conditions vector
  if (deleted == FALSE) {
    conditions = c("deleted = 0", conditions)
  }


  # Add all conditions to the query
  if (length(conditions) > 0) {
    conditions <- paste0("(", conditions, ")")
    conditions <- paste0(conditions, collapse = " AND ")
    sql <- paste0(sql, " WHERE ", conditions)
  }


  # get table
  table <- as_tibble(
    dbGetQuery(con, sql)
  )

  table <- mutate_if(
    table,
    is.character,
    .funs = function(x){return(`Encoding<-`(x, "UTF-8"))}
  )

  if (is.character(db)){
    dbDisconnect(con)
  }

  return(table)
}

#
#' Get codings from database
#'
#' @param db A connection object (object) or the database name (character)
#' @return A dataframe containing links and items.
#' @details Queries the database to join the items, properties, types, articles and links tables.
#' @export
db_get_codings <- function(db){

  # Check if db is character --> open db connection
  if (is.character(db)) {
    con <- db_connect(db)
    databasename <- db
  }  else {
    con <- db
    databasename <- db_name(con)
  }

  # Tables
  items <- db_table("items", con)
  properties <- db_table("properties", con)
  types <- db_table("types", con)
  articles <- db_table("articles", con)
  links <- db_table("links", con)

  # Close Database connection
  if (is.character(db)) {
    dbDisconnect(con)
  }

  # Join data
  items <- items %>%
    # Articles
    left_join(
      select(articles, id, articletype),
      by=c("articles_id"="id")
    ) %>%
    # Properties
    left_join(
      select(properties, id, propertytype, norm_data, lemma, name, unit),
      by=c("properties_id"="id")) %>%
    # Types
    left_join(
      select(types, scope, name, category, caption) %>%
        filter(scope=="properties"),
      by=c("propertytype"="name")
    )

  # Prepare data
  # 1. Items
  items <- items %>%
    filter(
      deleted == 0,
      !is.na(properties_id),
      articletype == "object"
    )

  # Links
  # - keep only relevant links (from articles to properties)
  links <- links %>%
    filter(
      deleted == 0,
      root_tab == "articles",
      to_tab == "properties",
    ) %>%
    semi_join(
      filter(articles, articletype == "object"),
      by=c("root_id"="id")
    )

  codings <- bind_rows(
    select(items, articles_id, properties_id),
    select(links, articles_id=root_id, properties_id=to_id)
  )

  codings <- codings %>%
    count(articles_id, properties_id)

  codings$db <- databasename

  return(codings)

}

#
#' Get codes from database
#'
#' @param db A connection object (object) or the database name (character)
#' @return A data frame containing the properties.
#' @details Queries the database to get the properties.
#'          The property types are added as root nodes
#'          and if the lemma is empty, it is replaced by the name
#' @export
db_get_codes <- function(db){

  # Check if db is character --> open db connection
  if (is.character(db)) {
    con <- db_connect(db)
    databasename <- db
  }  else {
    con <- db
    databasename <- db_name(con)
  }

  # Tables
  properties <- db_table("properties", con)

  # Close Database connection
  if (is.character(db)) {
    dbDisconnect(con)
  }

  # 2. Codes

  # Select properties
  codes  <- properties %>%
    filter(is.na(related_id)) %>%
    select(id, parent_id, propertytype, lemma, name, norm_data, norm_iri, level, lft, rght)

  # Create the tree

  # - Create root level from propertytypes
  #   (create negative ids that don't come in conflict with existing properties)
  propertytypes <- codes %>%
    distinct(propertytype) %>%
    mutate(level = -1) %>%
    mutate(id= - row_number()) %>%
    mutate(lemma=propertytype) %>%
    select(id,lemma,level,propertytype)

  # - Set parent id of first level to propertytype
  codes <- codes %>%
    left_join(select(
      propertytypes,
      propertytype_id=id,
      propertytype=lemma
    ),
    by=c("propertytype")
    ) %>%
    #mutate(parent_id = ifelse(level==0,propertytype_id,parent_id)) %>%
    mutate(parent_id = ifelse(is.na(parent_id), propertytype_id,parent_id)) %>%

    select(-propertytype_id)

  # - insert root level into the tree
  codes <- bind_rows(propertytypes,codes)
  rm(propertytypes)

  # -fix lft / rght for root items
  codes <- codes %>%
    group_by(propertytype) %>%
    mutate(lft = ifelse(level==-1,min(lft,na.rm = T)-1,lft)) %>%
    mutate(rght = ifelse(level==-1,max(rght,na.rm=T)+1,rght)) %>%
    ungroup()

  # create labels
  codes <- codes %>%
    mutate(lemma=ifelse(is.na(lemma) | lemma=="", name, lemma))

  codes$db <- databasename

  return(codes)
}
