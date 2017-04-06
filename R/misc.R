#' Predict Next Word
#'
#' @param input_text a single element \code{\link{character}} vector with the text that you would
#' like to perform nextword prediction on.
#'
#' @return a \code{\link{character}} vector of suggested words in order
#'
#' Uses ngram dictionary to compare the end of the provided string and provide suggestions.
#' @export
nextword = function(input_text) {
  text_tokens =
    quanteda::tokens(
      input_text,
      what="fastestword",
      removeNumbers = TRUE, removePunct = TRUE,
      removeSymbols = TRUE
    )[[1]]

  text_tokens =
    text_tokens %>%
    utils::tail(2) %>%
    tolower(.)

  word1 = text_tokens[1]
  word2 = text_tokens[2]

  candidates = dplyr::filter(
    ngram_dictionary,
    (key1 == word1 & key2 == word2) |
      (key1 == word2 & is.na(key2)) |
      (is.na(key1) & is.na(key2))
    # TODO also return results if words dont match any keys, or the matches are poor
  ) %>% utils::head(5)

  return(candidates$suggest)
}
