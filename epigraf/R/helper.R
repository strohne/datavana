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

#' Convert a number to letters, e.g. 3 becomes c
#'
#' @param number The number to convert
#' @param base The number of letters to use
#' @export
num2abc <- function(number, base = 26) {
  n <- ceiling(log((1/(1 - base) - 1 - number) * (1 - base), base = base)) - 1
  digits <- encode(number - sum(base^seq(0, n-1)), rep(base, n))
  paste(letters[digits + 1], collapse = "")
}


#' Converts "b" using the "base"
#'
#' APL-decode / APL-base "_|_", pw10/02
#' @param b The number to convert
#' @param base The base
#' @export
decode <- function(b, base) {
  b <- as.integer(b)
  if( length(base) == 1 ) base<-rep(base,length(b))
  base<-c(base,1)
  number<-as.vector( cumprod(rev(base)[ 1:length(b) ] ) %*% rev(b) )
  number
}

#' Converts numbers using the radix vector
#'
#' APL-encode / APL-representation "T", pw 10/02
#'
#' @param number The number to convert
#' @param base The base
#' @export
encode <- function(number, base) {
  "base"
  n.base <- length(base); result <- matrix(0, length(base),
                                           length(number))
  for(i in n.base:1){
    result[i,] <- if(base[i]>0) number %% base[i] else number
    number     <- ifelse(rep(base[i]>0,length(number)),
                         floor(number/base[i]), 0)
  }
  return( if(length(number)==1) result[,1] else result )
}
