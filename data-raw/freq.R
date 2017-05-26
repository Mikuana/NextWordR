library(quanteda)
library(data.table)
library(magrittr)
library(tidyr)

freqR = function(ngram_size = 6, start=1, fp=NULL) {
  if(is.null(fp)) { fp = file.path('data-raw', 'corp.train.rds') }
  # loop through each ngram length
  for(ngram in start:ngram_size) {
    corp = readRDS(fp)  # read in corpus
    tokens = tokenize(
      corp,
      ngrams = ngram,
      concatenator = " ",
      what='fastestword',
      remove_numbers = TRUE,
      remove_punct = TRUE,
      remove_symbols = TRUE
    )
    rm(corp)            # remove corpus
    gc()                # recover memory

    docfeatmat =  dfm(  # build document feature matrix
      tokens,
      tolower=TRUE
      # stem = TRUE
    )
    rm(tokens)         # remove tokens
    gc()               # recover memory

    obj_name = paste0('gramfreq', ngram)
    assign(
      obj_name,
      docfreq(docfeatmat) %>%
        data.table(
          gram = names(.),
          freq = .,
          stringsAsFactors=FALSE
        ) %>%
        .[order(freq)]
    )
    if(ngram > 1) {
      assign(obj_name, separate(
        get(obj_name), gram, c(paste0('key', 1:(ngram-1)), 'suggest'), sep = " ", remove=TRUE
      ))
    }
    if(ngram == 1) {
      setnames(get(obj_name),"gram","suggest")
    }

    saveRDS(                                        # save gram frequency table to disk
      get(obj_name),
      file.path('data-raw', paste0(paste0('gramfreq', ngram), '.rds'))
    )
    rm(obj_name, docfeatmat)                        # remove object
    gc()                                            # recover memory
  }
}

freqR()
