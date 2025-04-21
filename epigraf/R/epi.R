#
# Functions for Epigraf data handling
#


#' Create a clean IRI
#'
#' @param table The table name
#' @param type If NA, the type will be omitted.
#' @param fragment The IRI fragment that will be cleaned
#' @param split If TRUE and the fragment already contains a type, the fragment's type is used
#' @export
epi_create_iri <- function(table, type, fragment, split=F) {

  # Get type from fragment
  # TODO: vectorize
  # if (split) {
  #   fragment <- strsplit(fragment, "/", fixed=T)[[1]]
  #   if (length(fragment) > 1) {
  #      type <- fragment[1]
  #      fragment <- fragment[2]
  #   }
  # }

  table <- paste0(table, "/")
  type <- ifelse(is.na(type),"", paste0(type, "/"))
  fragment <- epi_clean_irifragment(fragment)

  paste0(table, type, fragment)
}


#' Create a clean IRI fragment
#'
#' Replaces all non alphanumeric characters by hyphens
#' and converts the input to lowercase
#'
#' @param iri The dirty IRI fragment that will be cleaned
#' @export
epi_clean_irifragment <- function(fragment) {
  fragment %>%
    str_to_lower() %>%
    str_replace_all(
      c("ä"="ae","ö"="oe","ü"="ue","ß"="ss")
    ) %>%
    str_replace_all("[^a-z0-9_~-]","-") %>%
    str_replace_all("-+","-") %>%
    str_remove("^-") %>%
    str_remove("-$")
}

#' Check whether the provided vector contains a valid IRI path
#'
#' @param iripath The vector that will be proofed
#' @param table Check whether the path contains the table. Leave empty to allow all tables.
#' @param type Check whether the path contains the type.  Leave empty to allow all types.
#' @export
epi_is_iripath <- function(iripath, table=NA, type=NA) {

  if (is.na(table)) {
    table <- "(projects|articles|sections|items|properties|links|footnotes|types|users)"
  }

  if (is.na(type)) {
    type <- "([a-z0-9_-]+)"
  }

  fragment <- "([a-z0-9_~-]+)"

  stringr::str_detect(iripath,paste0("^",table,"/",type,"/",fragment,"$"))

}


#' Check whether the provided vector contains valid IDs prefixed
#' with table names. Example: articles-123
#'
#' @param ids The vector that will be proofed
#' @param table Check whether the path contains the table. Leave empty to allow all tables.
#' @export
epi_is_id <- function(ids, table=NA) {

  if (is.na(table)) {
    table <- "(projects|articles|sections|items|properties|links|footnotes|types|users)"
  }

  fragment <- "([0-9]+)"

  stringr::str_detect(ids,paste0("^",table,"-",fragment,"$"))
}


#' Check whether the provided vector contains valid IDs prefixed
#' with table names and temporary prefixes. Example: articles-tmp123
#'
#' @param ids The vector that will be proofed
#' @param table Check whether the path contains the table. Leave empty to allow all tables.
#' @param prefix Check whether the ID contains the prefix, e.g. "tmp". Leave empty to allow all prefixes.
#' @export
epi_is_prefixid <- function(ids, table=NA, prefix=NA) {

  if (is.na(table)) {
    table <- "(projects|articles|sections|items|properties|links|footnotes|types|users)"
  }

  if (is.na(prefix)) {
    prefix <- "[a-z]+"
  }

  fragment <- "([0-9]+)"

  stringr::str_detect(ids,paste0("^",table,"-",prefix,fragment,"$"))
}


#' Check whether the provided vector contains a valid IRI fragment
#'
#' @param iripath The vector that will be proofed
#' @export
epi_is_irifragment <- function(irifragment) {
  stringr::str_detect(irifragment,"^[a-z0-9_~-]+$")
}

#' Get the IRI fragment of an IRI path
#'
#' @param id An IRI path
#' @param prefix A prefix added to the IRI fragment, if the ID is not NULL.
#' @return The IRI fragment
epi_iri_parent <- function(id = NULL, prefix = "~") {
  if (missing(id)) {
    parent <- ""
  } else {
    id <- strsplit(id, "/", fixed=T)
    parent <- sapply(id, function(x) utils::tail(x, 1))
    parent <- paste0(parent, prefix)
  }
  parent
}



#' Select nested data from prefixed columns
#'
#' @param data A data frame
#' @param cols_prefix All columns with the prefix will be selected,
#'                    the prefix will be removed from the column name.
#' @param cols_keep Convert the provided column names to underscored columns
#' @return A dataframe containing all columns with the prefix without the prefix
#'
#' @export
epi_extract_wide <- function(data, cols_prefix, cols_keep=c()) {

  if (length(cols_keep) > 0) {
    regex_keep <- c(paste0(cols_keep,"\\.id"),paste0(cols_keep,"_id"))
    regex_keep <- paste0(regex_keep,collapse = "|")
    regex_keep <- paste0("^", regex_keep, "$")
  } else {
    regex_keep <- "^$"
  }


  data <- data %>%
    select(starts_with(paste0(cols_prefix, ".")), matches(regex_keep)) %>%
    rename_all(~str_replace(.,paste0(cols_prefix, "\\."),"")) %>%
    rename_all(~str_replace(.,"\\.","_")) %>%
    distinct() %>%
    dplyr::select(where(~!all(is.na(.x)))) %>%
    dplyr::filter(if_any(everything(), ~ !is.na(.)))

  # Remove data that only contains ID columns
  if (length(setdiff(colnames(data), c("id", paste0(cols_keep,"_id")))) == 0) {
    data <- tibble()
  }

  data

}

#' Convert wide to long format
#'
#' @param data A dataframe with the column id containing a valid IRI path.
#'             Additional columns may contain nested data in the following form:
#'
#'             Column names prefixed with "properties", "items", "sections",
#'              "articles" and "projects" followed by a dot (e.g. "properties.id",
#'             "properties.lemma") will be extracted and stacked to the dataframe.
#'
#' @return A dataframe with all input rows and the nested records stacked.
#' @export
epi_wide_to_long <- function(data) {

  rows = tibble()

  # Extract nested rows
  rows <- bind_rows(rows,epi_extract_wide(data, "properties"))
  rows <- bind_rows(rows,epi_extract_wide(data, "projects"))
  rows <- bind_rows(rows,epi_extract_wide(data, "articles", c("projects")))
  rows <- bind_rows(rows,epi_extract_wide(data, "sections", c("articles")))
  rows <- bind_rows(rows,epi_extract_wide(data, "items", c("articles","sections")))

  # All other rows
  extracted <- data %>%
    select(matches("^[_a-z]+$"),matches("^projects\\.id|articles\\.id|sections\\.id|items\\.id|properties\\.id$")) %>%
    rename_all(~str_replace(.,"\\.","_"))

  if ((nrow(extracted) > 0) && (ncol(extracted) > 0)) {
    rows <- bind_rows(rows, extracted)
  }

  stopifnot(epi_is_iripath(rows$id) | epi_is_id(rows$id))

  # Create table columns
  if ((nrow(rows) > 0) && (ncol(rows) > 0)) {
    rows <- rows %>%
      dplyr::filter(if_any(everything(), ~ !is.na(.))) %>%
      mutate(table=str_extract(id,"^[^/]+")) %>%
      select(table, id, everything())
  }

  rows
}

