
# "epi" functions

#' Convert text to epigraf article
#' @param text Dataframe with the columns id, project, caption and content
#' @return Dataframe with article, section and item
#' @export
#' @examples
#' epi_text2article(
#' tibble(id=1,project="Import",
#' caption="My first text",
#' content="Words are letters with glue"))

epi_text2article  <- function(text)
{ if (!requireNamespace("tidyverse", quietly = TRUE)) {
  stop(
    "Package \"tidyverse\" must be installed to use this function.",
    call. = FALSE
  )
}
  projects <- tibble(
    table="projects",
    type="default",
    id= paste0("projects-int",unique(text$project)),
    name=unique(text$project)
  )


  articles <- tibble(
    table="articles",
    type="default",
    id= paste0("articles-int",text$id),
    projects_id=paste0("projects-int",text$project),
    name=text$caption,
  )

  sections <- tibble(
    table="sections",
    type="default",
    id= paste0("sections-int",text$id),
    articles_id= paste0("articles-int",text$id),
    name="Text"
  )

  items <- tibble(
    table="items",
    type="default",
    id= paste0("items-int",text$id),
    sections_id= paste0("sections-int",text$id),
    articles_id= paste0("articles-int",text$id),
    content=text$content
  )

  bind_rows(
    projects,
    articles,
    sections,
    items
  )

}

#' Create a clean IRI
#'
#' @param table The table name
#' @param type If NA, the type will be omitted.
#' @param fragment The IRI fragment that will be cleaned
#' @export

epi_create_iri <- function(table, type, fragment) {
  paste0(
    table, "/",
    ifelse(is.na(type),"",paste0(type, "/")),
    str_remove_all(str_to_lower(fragment),"[^a-z0-9_~-]")
  )
}

#' Check whether the provided vector contains a valid IRI path
#'
#' @param iripath The vector that will be proofed
#' @param table Check whether the path contains the table. Leave empty to allow all tables.
#' @param type Check whether the path contains the type.  Leave empty to allow all types.
#' @export
epi_is_iripath <- function(iripath, table=NA, type=NA) {

  if (is.na(table)) {
    table <- "(projects|articles|sections|items|properties)"
  }

  if (is.na(type)) {
    type <- "([a-z0-9_-]+)"
  }

  fragment <- "([a-zA-Z0-9_~-]+)"

  stringr::str_detect(iripath,paste0("^",table,"/",type,"/",fragment,"$"))

}


#' Check whether the provided vector contains a valid IRI fragment
#'
#' @param iripath The vector that will be proofed
#' @export
epi_is_irifragment <- function(irifragment) {
  stringr::str_detect(irifragment,"^[a-z0-9_~-]+$")

}

# Export to Epi

#' Function to create properties
#'
#' Creates properties table.
#'
#' @param propertytype The property type (character)
#' @param lemmata A vector of lemmata (character).
#' @param names Optional. A vector of  names (character).
#' @param irifragments Optional. A vector of IRI fragments (character).
#' @export
epi_create_properties <- function(propertytype, lemmata, names=NA, irifragments=NA){

  # create properties table
  properties <- tibble(
    lemma = lemmata,
    name = names,
    irifragment = irifragments
  )

  iri_path = paste0("properties/", propertytype, "/")

  # create ids from irifragment or row-number
  properties <- properties %>%
    mutate(
      id = case_when(
        !is.na(irifragment) ~ paste0(iri_path, irifragment),
        TRUE ~ paste0(iri_path,  row_number()
        )
      )
    )

  # add name if given, else create name from lemma
  properties <- properties %>%
    mutate(name = ifelse(!is.na(name), name, lemma))

  # select relevant columns
  properties <- select(properties, id, lemma, name)

  return(properties)

}



#' Create sections
#'
#' Creates a section for each row in the input data frame
#'
#' @param data A data frame containing the columns articletype and norm_iri
#' @param sectiontype A string with the sectiontype
#' @param name The section name. Leave empty to use the name defined in the Epigraf domain model.
#' @export
epi_create_sections <- function(data, sectiontype, name=NA){
  tibble(
    articles_id = paste0("articles/", data$articletype, "/", data$norm_iri),
    id = paste0("sections/", sectiontype, "/", data$norm_iri),
    name = name
  )
}


#' Create one empty item for each section
#'
#' @param sections A data frame containing the sections
#' @param itemtype A string with the name for the itemtype
#' @export
epi_create_empty_items <- function(sections, itemtype) {


  sections %>%
    select(
      sections_id = id,
      articles_id
    ) %>%
    mutate(
      id = paste0(
        "items/",
        itemtype,"/",
        str_extract(sections_id,"[^/]+$")
      )
    ) %>%
    select(id, sections_id, articles_id)

}

#' Create filled items and properties from values
#'
#' @param data A data frame containing the columns articletype and norm_iri
#' @param col_articletype The column in data specifying the articletype
#' @param col_value The column in data specifying the value
#' @param col_prop The column in data containing the propertytype
#' @param sectiontype A string with the name of the section
#' @param itemtype A string with the name of the itemtype
#' @export
epi_create_property_items <- function(data, col_articletype, col_value, col_prop, sectiontype, itemtype ){

  col_articletype <- enquo(col_articletype)
  col_value <- enquo(col_value)
  col_prop <- enquo(col_prop)

  props <- create_properties("coding-sample",distinct(data, !!col_prop) %>%  pull() )

  items <- data %>%

    mutate(
      id = paste0("items/",itemtype,"/",norm_iri),
      sections_id = paste0("sections/", sectiontype, "/", norm_iri),
      articles_id = paste0("articles/", !!col_articletype, "/", norm_iri)
    ) %>%

    mutate(
      value = !!col_value,
      properties_lemma = !!col_prop

    ) %>%
    left_join(select(props,properties_id=id,properties_lemma=lemma),by="properties_lemma") %>%
    select(id, sections_id, articles_id, properties_id, value)

  bind_rows(props, items)
}

