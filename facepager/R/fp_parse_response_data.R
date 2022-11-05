#' convert response data to columns of a data frame
#' @import jsonlite
#' @description As you can see in the loaded data,
#' most of your data is in JSON format in the response column.
#' Therefore you need to convert the JSON data in the response to columns of a data frame.
#' @param nodes the fields/columns originally collected by Facepager,
#' which are all together in the response column,
#' can be for example "created_at", "text"/ "message", "favorite_count"/ "like_count" etc.
#' @param plain ...
#' @param prefix name of the columns
#' @return a data frame containing the data of the fp database
#'  with all response data as own columns
#' @examples
#' @export
fp_parse_response_data <- function (nodes, plain = F, prefix="response.") {
  responses = fp_from_ndjson(nodes$response)

  responses = jsonlite::flatten(responses,recursive = T)
  # if (plain) {
  #   responses <- select_if(responses,~!is.data.frame(.))
  #   responses <- select_if(responses,~!is.list(.))
  # }

  colnames(responses) = paste0(prefix,colnames(responses))
  nodes = bind_cols(select(nodes,-response),responses)
  rm(responses)
  nodes
  }

#' helper function for fp_parse_response_data()
#' @import jsonlite
#' @rdname fp_parse_response_data
#' @export
fp_from_ndjson <- function(data) {
  data[data == "[]"] = "{}"
  data[is.na(data)] = "{}"

  jsonlite::stream_in(textConnection(data))}


#' helper function for fp_parse_response_data()
#' @import tidyverse
#' @export
fp_get_response_value <- function(nodes, col, .split=TRUE,.progress=NULL) {

  if (.split) {

    nodes <- nodes %>%
      mutate(chunk = row_number() %/% 1000 ) %>%
      split(.$chunk)

    .progress <- progress_estimated(length(nodes),min_time = 1)
    .progress$print()

    nodes <- nodes %>%
      purrr::map(fp_get_response_value,col,.split=FALSE,.progress=.progress) %>%
      unlist()

    .progress$stop()$print()
    cat("\n")

  } else {
    col_js <- do.call(jstring,as.list(strsplit(col, ".",fixed=T)[[1]]))
    nodes <- nodes$response %>%
      spread_values(value=col_js) %>% extract2(2)

    if (!is.null(.progress))
      .progress$tick()$print()
  }

  return(nodes)
}
