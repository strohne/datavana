#' Load a CSV file
#' @import tidyverse
#' @param filename name of the CSV file you want to load
#' @param na="None" define missing values as string 'None'
#' @return a tibble containing the data of the csv file
#' @examples
#' @export
fp_read_csv2 <- function(filename) {
  read_csv2(filename, na=c("None",""))
}
