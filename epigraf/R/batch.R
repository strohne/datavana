#
# Functions for batch operations
#

#' Transfer datasets between different databases
#'
#' @param table Table name, e.g. "types"
#' @param db_source Source database name
#' @param db_target Target database name
#' @param db_params A parameter list for selecting the appropriate rows.
#'                  For example: params <- list("scopes" = "properties")
#' @export
api_transfer <- function(table, db_source, db_target, db_params = list()) {
  db_params["source"] = db_source
  api_job_create(paste0(table, "/transfer"), db_params, db_target)
}
