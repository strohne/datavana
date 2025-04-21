#' Add a row to the rows in the epi attribute
#'
#' @param ds A tibble
#' @param row A crafted row
#' @param skip Whether to update the record or only use it as reference
#' @return Epigraf tibble
ram_add <- function(ds, rows, skip=FALSE) {

  epi <- attr(ds, "epi")
  if (is.null(epi)) {
    epi <- list()
  }

  if (is.null(epi$rows)) {
    epi$rows <- tibble::tibble()
  }

  rows <- dplyr::mutate(rows, dplyr::across(tidyselect::everything(), as.character))

  if (skip) {
    rows$`_action` <- "skip"
  }

  epi$rows <- bind_rows(epi$rows, rows)
  attr(ds, "epi") <- epi

  # Set class
  class(ds) <- c("epi_tbl", setdiff(class(ds),"epi_tbl"))
  ds
}

#' Compile a crafted Epigraf table ready to patch into the database
#'
#' @export
ram_compile <- function(ds) {
  rows <- attr(ds, "epi")$rows
  if (is.null(rows)) {
    rows <- tibble::tibble()
  }

  rows <- dplyr::arrange(rows, dplyr::desc(
    dplyr::across(tidyselect::any_of(c(".project",".article",".section",".item")))
  )
  )

  rows <- dplyr::select(
    rows,
    tidyselect::any_of(c(
      "table","id",
      "signature", "name", "alias", "sortno",
      "content","property",
      "sections_id","articles_id","projects_id"
    )),
    tidyselect::everything(),
    -tidyselect::starts_with(".")
  )

  tibble::as_tibble(apply(rows, 2, rev))
}
