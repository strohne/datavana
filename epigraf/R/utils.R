#'Remove NA entries from a list
#'
#'@param y A list
#'@return A cleaned list
#'@export
na.omit.list <- function(y) {
  return(y[!vapply(y, function(x) all(is.na(x)), logical(1))])
}

#' Ask user to confirm script execution
#'
confirmAction <- function() {
  silent <- Sys.getenv("epi_silent")
  if (silent == "TRUE") {
    return (TRUE)
  }

  user_input <- readline("Are you sure you want to proceed? (y/n)  ")
  if(user_input != 'y')
    stop('Canceled')
}

#' Check whether the URL is on a local server
#'
isLocalServer <- function(server) {
  return (
    startsWith(server,"https://127.0.0.1") ||
      startsWith(server,"http://127.0.0.1") ||
      startsWith(server,"https://localhost") ||
      startsWith(server,"http://localhost")
  )
}

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
#' @param data A character vector that may contain NAs
#' @return A parsed vector
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

#' Create distinct pseudonyms
#'
#' @param n Number of pseudonyms. Leave empty when used inside mutate to create value for each row
#' @return Vector of pseudonyms
#' @export
pseudonyms <- function(n) {

  if (missing(n)) {
    n = dplyr::n()
  }

  letters.consonant <- c("b","c","d","f","g","h","j","k","l","m","n","p","q","r","s","t","v","w","x","z")
  letters.vocal <- c("a","e","i","o","u")

  candidates <- c()
  todo <- n
  iterations <- 0

  while (todo > 0) {

    if (iterations > 100) {
      stop("Could not create sufficient values in 100 iterations.")
    }

    candidates <- unique(
      c(
        candidates,
        paste0(
          sample(str_to_upper(letters.consonant), size=todo, replace=T),
          sample(letters.vocal, size=todo, replace=T),

          sample(letters.consonant, size=todo, replace=T),
          sample(letters.vocal, size=todo, replace=T),

          sample(letters.consonant, size=todo, replace=T),
          sample(letters.vocal, size=todo, replace=T)
        )
      )
    )

    todo <- n - length(candidates)
    iterations <- iterations + 1
  }

  candidates
}


#' Bind rows of dataframes even if column types differ
#'
#' Convert columns that differ in type to character,
#' then bind rows.
#'
#' @param dataframes A list of dataframes
#' @param A list of dataframes with common column types
bind_rows_char <- function(dataframes){

  cols_names <- unique(unlist(lapply(dataframes, colnames)))
  cols_classes <- lapply(cols_names, function(colname)
    unique(sapply(dataframes, function(df) ifelse(
      colname %in% colnames(df),class(df[[colname]]),
      "NULL"
    ))
  ))
  cols_tocharacter <- lapply(cols_classes, \(col) (length(col[col != "NULL"]) > 1))
  names(cols_tocharacter) <- cols_names


  if (length(cols_tocharacter) > 0) {
    dataframes <- lapply(dataframes, \(df) {
      for (colname in colnames(df)) {
        if (cols_tocharacter[[colname]]) {
          df[[colname]] <- as.character(df[[colname]])
        }
      }
      df

    })
  }

  return(bind_rows(dataframes))
}
