
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
    str_to_lower(str_remove_all(fragment,"[^a-zA-Z0-9_-]"))
  )
}
