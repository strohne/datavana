#' Check whether a column exist and stop if not
#'
#' @keywords internal
#'
#' @param data A data frame
#' @param col A column name
#' @param msg A custom error message if the check fails
#' @return boolean Whether the column exists
check_has_column <- function(data, col, msg=NULL) {
  colname <- as.character(rlang::get_expr(rlang::enquo(col)))

  check <- colname != ""
  if (!check ) {
    msg <- dplyr::coalesce(msg, paste0("Did you miss to say which column to use?"))
    stop(msg, call. = F)
  }


  check <- colname %in% colnames(data)
  if (!check ) {
    msg <- dplyr::coalesce(msg, paste0("The column ", colname, " does not exist, check your parameters."))
    stop(msg, call. = F)
  }

  check
}

#' Check whether a value is a valid table prefixed ID, and stop if not
#'
#' @keywords internal
#'
#' @param value A character value
#' @param msg A custom error message if the check fails
#' @return boolean Whether the value is a valid table prefixed ID
check_is_id   <- function(value, msg=NULL) {
  check <- epi_is_id(value)
  if (!check ) {
    msg <- dplyr::coalesce(msg, paste0("The value ", value, " is not a valid Epigraf ID."))
    stop(msg, call. = F)
  }

  check
}


#' Check whether a value is a valid database name, and stop if not
#'
#' @keywords internal
#'
#' @param value A character value
#' @param msg A custom error message if the check fails
#' @return boolean Whether the value is a valid database name
check_is_db   <- function(value, msg=NULL) {
  check <- is.character(value)
  if (!check ) {
    msg <- dplyr::coalesce(msg, paste0("The value ", value, " is not a valid Epigraf database name."))
    stop(msg, call. = F)
  }

  check
}
