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
