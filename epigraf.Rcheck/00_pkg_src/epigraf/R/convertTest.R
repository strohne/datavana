
#' Convert text to epigraf article
#' @param text Dataframe with the columns id, project, caption and content
#' @return Dataframe with article, section and item
#' @export
#' @examples
#' textToArticle(
#' tibble(id=1,project="Import",
#' caption="Mein erster Text",
#' content="Worte sind Buchstaben mit Kleber"))


textToArticle <- function(text)
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

# Example
textToArticle(
  tibble(
    id=1,
    project="Import",
    caption="Mein erster Text",
    content="Worte sind Buchstaben mit Kleber"
  )
)

