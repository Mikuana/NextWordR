#' Predict Next Word
#'
#' @param input_text a single element \code{\link{character}} vector with the text that you would
#' like to perform nextword prediction on.
#'
#' @return a \code{\link{character}} vector of suggested words in order
#'
#' Uses ngram dictionary to compare the end of the provided string and provide suggestions.
#' @export
next_word = function(input_text) {
  required_keys = 5
  tokens = quanteda::tokenize(
    input_text,
    concatenator = " ",
    what='fastestword',
    remove_numbers = TRUE,
    remove_punct = TRUE,
    remove_symbols = TRUE
  )

  provided_keys = tail(tokens[[1]], required_keys)
  working_keys = provided_keys
  sugg = as.character(NULL)

  while(length(sugg) < 3) {
    key_length = length(working_keys)
    fill_length = required_keys - key_length
    filled_keys = c(working_keys, rep('', times = fill_length))

    # lookup suggestions based upon working keys
    sugg = c(na.omit(as.character(grams[as.list(filled_keys), suggest])), sugg)

    # drop duplicate suggestions
    sugg = unique(sugg, fromLast=TRUE)

    # drop one key in preparation for extra loop through backoff if we don't have 3 suggestions
    working_keys = tail(working_keys, key_length - 1)
  }
  return(tail(sugg, 3))
}
