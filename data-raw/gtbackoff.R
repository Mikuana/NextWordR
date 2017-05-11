library(quanteda)
library(data.table)
library(magrittr)
library(tidyr)
library(foreach)
library(edgeR)

seqs = function(length, chunksize) {
  # This function breaks a vector of index numbers into chunks
  # ensuring that the last sequence is only as long as the provided
  # length. This is for staging row processing in chunks without
  # causes reference errors at the end.
  seed = seq(from=1, to=length, by=chunksize)
  most = lapply(head(seed, -1), function(x) {
    x:(x+chunksize-1)
  })
  last = tail(seed,1):length
  c(most, list(last))
}

gtbackoff = function(ngram_size = 6, start=1) {
  gc() # recover memory. We need a lot
  # loop through each ngram size
  for(ngram in start:ngram_size) {

    # file path for gramfreq table
    fp = file.path('data-raw', paste0('gramfreq', ngram,'.rds'))
    # read in gram frequency table
    gramfreq = readRDS(fp)

    if(ngram == 1) {
      sugg = gramfreq
      sugg[, gtprop := goodTuringProportions(freq)]
      sugg[, ngramL := ngram ]
      sugg = sugg[order(gtprop), tail(.SD, 5)]
      fp = file.path('data-raw', 'gram1.rds')
      saveRDS(sugg, fp)

      next
    }

    # get key columns by name
    keycolumns = grep('^key\\d', names(gramfreq), value=TRUE)
    # set keys on gramfreq table
    setkeyv(gramfreq, keycolumns)
    # create keygroup table for smoothing iterations
    keygroups = gramfreq[ , .(COUNT = .N), by=keycolumns]

    # file path for lower ngram 
    fp = file.path('data-raw', paste0('gram', ngram-1, '.rds'))
    lowgram = readRDS(fp)

    # file path for ngram table
    fp = file.path('data-raw', paste0('gram', ngram,'.rds'))
    # remove file if it already exists
    if(file.exists(fp)) { file.remove(fp)}
    # prepare for multi-threaded processing
    doParallel::registerDoParallel(cores=4)
    for(ixs in seqs(nrow(keygroups), 1000)[1]) {
      grams =
        foreach(x = ixs) %dopar% {
          kg = keygroups[x]
          # subset keygroup records from full table
          sugg = gramfreq[.(kg[,keycolumns,with=FALSE])]
          # calculate Good-Turing proportions for seen records
          sugg[,gtprop := goodTuringProportions(freq)]
          # calculate prevalence of unseen grams
          P0 = goodTuring(sugg$freq)$P0

          # add ngram length to suggestions
          sugg[, ngramL := ngram ]

          # keep only a specified number of suggestions
          sugg[order(gtprop), tail(.SD, 5)]
        } %>%
        rbindlist(use.names=FALSE, fill = FALSE)

      if(file.exists(fp)) {
        grams = rbindlist(list(readRDS(fp), grams), use.names=FALSE, fill = FALSE)
      }
      saveRDS(grams, fp, compress=FALSE)
      gc()
    }

    grams = readRDS(fp)
    setkeyv(grams, c(keycolumns, 'suggest', 'gtprop'))
    saveRDS(grams, fp)
  }
}

gtbackoff(6)
