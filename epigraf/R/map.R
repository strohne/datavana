#' Map a data frame to the Relational Article Model
#'
#' @param df A data frame with the source data.
#' @param item A named list. Names are RAM columns, values source columns.
#' @param section A named list. Names are RAM columns, values source columns.
#' @param article A named list. Names are RAM columns, values source columns.
#' @param project A named list. Names are RAM columns, values source columns.
#' @param vals A named list with fixed values for the rows.
#'             Names on the first level correspond to the table (item, section, article, project).
#'             Names on the second level correspond the the field names.
#' @param compile Whether to return compiled rows or store them in the attributes of the input data frame.
#' @return A data frame. When compile is TRUE, ready to be imported into Epigraf.
#'         Otherwise with rows in the attributes of the data frame.
#' @examples
#' library(eprgraf)
#'
#' df <- tribble(
#'   ~case, ~title, ~text,
#'   1, "Case 01", "Happy New Year!",
#'   2, "Case 02", "Happy Easter!",
#'   3, "Case 03", "Happy Birthday!"
#' )
#'
#' df |>
#'  df_to_ram(
#'    item = c("content" = "text"),
#'    article = c("id" = "case", "signature" = "case", "name" = "title"),
#'    compile = T
#'  )
#'
#' @export
df_to_ram <- function(df, item = c(), section = c(), article = c(), project = c(), vals = list(), compile = FALSE) {

  df <- map_projects_to_ram(df, project)
  df <- map_articles_to_ram(df, article)
  df <- map_sections_to_ram(df, section)
  df <- map_items_to_ram(df, item)

  if (compile ) {
    return(ram_compile(df))
  }

  return(df)
}

#' Create RAM rows for project data
#'
#' @keywords internal
#'
#' @param df The source data frame
#' @param mapping The mapping of source columns to RAM columns
#' @return RAM rows
map_projects_to_ram <- function(df, mapping) {
  # Project rows
  mapping <- merge_vectors(mapping, c("id" = "project.id", "type" = "project.type"))

  rows <- df
  rows <- default_values(rows, mapping[["id"]], "default")
  rows <- default_values(rows, mapping[["type"]], "default")

  rows <- as.data.frame(rows[, mapping, drop = FALSE])
  names(rows) <- names(mapping)
  rows$id <- epi_create_iri("projects", rows$type, rows$id)
  rows$type <- NULL
  cols_projects <- colnames(rows)

  df$.project <- rows$id
  rows$.project <- df$.project

  rows <- dplyr::distinct(rows)
  rows$`_fields` <- paste0(cols_projects, collapse=",")

  df <- ram_add(df, rows)
  df
}

#' Create RAM rows for article data
#'
#' @keywords internal
#'
#' @param df The source data frame
#' @param mapping The mapping of source columns to RAM columns
#' @return RAM rows
map_articles_to_ram <- function(df, mapping) {


  if (!(".project" %in% colnames(df))) {
    .craft_stop("Please, map a project first")
  }

  # Article rows
  mapping <- merge_vectors(mapping, c("id" = "article.id", "type" = "article.type", "projects_id" = "article.project"))
  rows <- df
  rows <- default_values(rows, mapping[["id"]], "default")
  rows <- default_values(rows, mapping[["type"]], "default")

  rows[[mapping[["projects_id"]]]] <- df$.project

  rows <- as.data.frame(rows[, mapping, drop = FALSE])
  names(rows) <- names(mapping)

  rows$id <- epi_create_iri("articles", rows$type, rows$id)
  rows$type <- NULL
  cols_articles <- colnames(rows)

  df$.article <- rows$id
  rows$.project <- df$.project
  rows$.article <- df$.article

  rows <- dplyr::distinct(rows)
  rows$`_fields` <- paste0(cols_articles, collapse=",")

  df <- ram_add(df, rows)
  df
}

#' Create RAM rows for section data
#'
#' @keywords internal
#'
#' @param df The source data frame
#' @param mapping The mapping of source columns to RAM columns
#' @return RAM rows
map_sections_to_ram <- function(df, mapping) {

  if (!(".project" %in% colnames(df))) {
    .craft_stop("Please, map a project first")
  }

  if (!(".article" %in% colnames(df))) {
    .craft_stop("Please, map an article first")
  }

  # Section rows
  mapping <- merge_vectors(mapping, c("id" = "section.id", "type" = "section.type", "articles_id" = "section.article", "projects_id" = "section.project"))
  rows <- df
  rows <- default_values(rows, mapping[["id"]], "default")
  rows <- default_values(rows, mapping[["type"]], "default")
  rows[[mapping[["projects_id"]]]] <- df$.project
  rows[[mapping[["articles_id"]]]] <- df$.article

  rows <- as.data.frame(rows[, mapping, drop = FALSE])
  names(rows) <- names(mapping)
  rows$id <- epi_create_iri("sections", rows$type, paste0(epi_iri_parent(rows$articles_id),rows$id))
  rows$type <- NULL

  cols_sections <- colnames(rows)

  df$.section <- rows$id
  rows$.project <- df$.project
  rows$.article <- df$.article
  rows$.section <- df$.section

  rows <- dplyr::distinct(rows)
  rows$`_fields` <- paste0(cols_sections, collapse=",")

  df <- ram_add(df, rows)
  df
}

#' Create RAM rows for item data
#'
#' @keywords internal
#'
#' @param df The source data frame
#' @param mapping The mapping of source columns to RAM columns
#' @return RAM rows
map_items_to_ram <- function(df, mapping) {

  if (!(".project" %in% colnames(df))) {
    .craft_stop("Please, map a project first")
  }

  if (!(".article" %in% colnames(df))) {
    .craft_stop("Please, map an article first")
  }

  if (!(".section" %in% colnames(df))) {
    .craft_stop("Please, map a section first")
  }


  # Item rows
  mapping <- merge_vectors(mapping, c("id" = "item.id", "type" = "item.type", "sections_id" = "item.section", "articles_id" = "item.article","projects_id" = "item.project"))
  rows <- df
  rows <- default_values(rows, mapping[["id"]], "default")
  rows <- default_values(rows, mapping[["type"]], "default")
  rows[[mapping[["projects_id"]]]] <- df$.project
  rows[[mapping[["articles_id"]]]] <- df$.article
  rows[[mapping[["sections_id"]]]] <- df$.section

  rows <- as.data.frame(rows[, mapping, drop = FALSE])
  names(rows) <- names(mapping)
  rows$id <- epi_create_iri("items", rows$type, paste0(epi_iri_parent(rows$sections_id),rows$id))
  rows$type <- NULL
  cols_items <- colnames(rows)

  df$.item <- rows$id
  rows$.project <- df$.project
  rows$.article <- df$.article
  rows$.section <- df$.section
  rows$.item <- df$.item

  rows <- dplyr::distinct(rows)
  rows$`_fields` <- paste0(cols_items, collapse=",")

  df <- ram_add(df, rows)
  df
}
