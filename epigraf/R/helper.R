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
