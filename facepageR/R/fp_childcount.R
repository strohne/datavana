#' Count the number of different children per Object ID (without duplicates).
#' @import stringr
#' @param .data the loaded data from Facepager
#' @param .parents=NA ensures the usage of the parent nodes
#' @param .silent=FALSE standard that error messages are displayed
#' @param level=0 the node level
#' @return a tibble with 4 columns: parent_objectid, children, parent_no, file
#' @examples
#' @export
fp_childcount <- function(.data, .parents=NA, .silent=FALSE, level=0) {
  # .data <- data
  # .parents <- NA
  # level=0

  if (is.na(.parents)) {
    .parents <- fp_getparents(.data,level=!!level)
  }

  if (! ("file" %in% colnames(.parents))) {
    .parents <- mutate(.parents,file = "")
  }

  if (("file" %in% colnames(.data))) {
    .data$file <- NULL
  }

  .parents <- .data %>%
    dplyr::filter(objecttype %in% c("data","unpacked"),querystatus=="fetched (200)") %>%
    right_join(.parents,by=c("parent_id")) %>%
    group_by(parent_objectid)  %>%
    summarize(
      children = n_distinct(na.omit(objectid)),
      parent_no = paste0(unique(str_pad(parent_no,5,"left","0")),collapse=","),
      file = paste0(unique(file) ,collapse=",")
    ) %>%
    ungroup()
  }

