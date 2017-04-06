#' NextWordR: An R package for interactive word prediction
#'
#' @docType package
#' @name NextWordR
NULL

#' @importFrom magrittr %>%
NULL


globalVariables(c(
    '.', # for magrittr chaining in functions
    'ngram_dictionary',  # data set used natively by functions
    'key1',  #field in ngram_dictionary data set
    'key2',  #field in ngram_dictionary data set
    'raw_data_md5'  #data set that gets called internally
))
