na.omit.list <- function(y) { return(y[!vapply(y, function(x) all(is.na(x)), logical(1))]) }
