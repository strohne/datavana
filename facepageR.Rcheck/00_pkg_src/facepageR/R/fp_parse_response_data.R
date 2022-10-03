#' Function to convert JSON in the response to columns of a data frame
#' @param
#' @keywords
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
