#' Convert JSON response data to columns of a data frame
#' @importFrom jsonlite flatten
#' @description As you can see in the loaded data,
#' most of your data is in JSON format in the response column.
#' Therefore you need to convert the JSON data in the response to columns of a data frame.
#' @param nodes the fields/columns originally collected by Facepager,
#' which are all together in the response column,
#' can be for example "created_at", "text"/ "message", "favorite_count"/ "like_count" etc.
#' @param plain Only keep atomic columns, remove data frames and lists
#' @param prefix name of the columns
#' @return A data frame containing the data of the Facepager database
#'  with all response data as own columns
#' @examples
#' @export
fp_parse_response_data <- function (nodes, plain = F, prefix="response.") {
  responses = fp_from_ndjson(nodes$response)

  responses = jsonlite::flatten(responses, recursive = T)
  if (plain) {
    responses <- select_if(responses,~!is.data.frame(.))
    responses <- select_if(responses,~!is.list(.))
  }

  colnames(responses) = paste0(prefix,colnames(responses))
  bind_cols(select(nodes,-response),responses)
  }

#' helper function for fp_parse_response_data()
#' @importFrom jsonlite stream_in
#' @rdname fp_parse_response_data
#' @export
fp_from_ndjson <- function(data) {
  data[data == "[]"] = "{}"
  data[is.na(data)] = "{}"

  jsonlite::stream_in(textConnection(data))
}


#' helper function for fp_parse_response_data()
#' @import tidyverse
#' @rdname fp_parse_response_data
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
