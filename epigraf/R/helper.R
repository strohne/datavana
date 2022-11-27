#' Remove HTML entities
#' @export
#'
unescape_html <- function(str){
  if (is.na(str)) {
    return (str)
  } else {
    return (xml2::xml_text(xml2::read_html(paste0("<x>", str, "</x>"))))
  }
}

#' Remove empty columns
#' @export
#'
drop_empty_columns <- function(df) {
  select_if(df, ~{any(!is.na(.))})
}


#' Parse JSON columns
#'
#' @importFrom jsonlite stream_in
#' @export
parse_json <- function(data) {
  data[data == "[]"] = "{}"
  data[is.na(data)] = "{}"

  jsonlite::stream_in(textConnection(data))
}

#' Merge list elements by their name
#'
#'@param l A list of lists to merge
#'@return A merged list
#'@export
merge_lists <- function(l) {
  keys <- unique(unlist(lapply(l, names)))
  l <- setNames(do.call(mapply, c(FUN=c, lapply(l, `[`, keys))), keys)
  as.list(l)
}
