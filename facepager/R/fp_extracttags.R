#' get tags (urls, mentions, hashtags)
#' @import tokenizers
#' @param data the loaded data from Facepager
#' @param col_text the column from which the tags should be extracted
#' @param prefix=NA when transformatting, the values are not prefixed with a character
#' @return
#' @examples
#' @export
fp_extracttags <- function(data, col_text, prefix=NA) {
  col_text <- enquo(col_text)

  text <- data %>%
    select(!!col_text)

  # Length
  text <- text %>%
    rowwise() %>%
    mutate(
      msg_length_chars = str_length(!!col_text),
      msg_length_words = length(tokenize_words(!!col_text)[[1]][!is.na(!!col_text)])
    )


  # URLs (extract and remove from text)
  text <- text %>%
    mutate(urls=str_extract_all(!!col_text,"http\\S+")) %>%
    mutate(!!col_text := str_remove_all(!!col_text,"http\\S+")) %>%

    rowwise() %>%
    mutate(
      count_urls=length(unlist(urls)),
      urls=paste0(unlist(urls),collapse=";")
    ) %>%
    ungroup()


  # Hashtags
  text <- text %>%
    mutate(tags=str_extract_all(!!col_text,"(?<![\\S])#\\S+")) %>%
    rowwise() %>%
    mutate(
      count_tags=length(unlist(tags)),
      tags=paste0(unlist(tags),collapse=";")
    ) %>%
    ungroup()


  # Mentions
  text <- text %>%
    mutate(mentions=str_extract_all(!!col_text,"(?<![\\S])@\\S+")) %>%
    rowwise() %>%
    mutate(
      count_mentions=length(unlist(mentions)),
      mentions=paste0(unlist(mentions),collapse=";")
    ) %>%
    ungroup()


  text <- select(text,tags,urls,mentions,
                 count_tags,count_urls,count_mentions,
                 msg_length_chars,msg_length_words)

  if (!is.na(prefix))
    text <- rename_all(text,~paste0(prefix,.))

  data <- data[!(colnames(data) %in% colnames(text))]
  data <- bind_cols(data,text)

  return(data)

}
