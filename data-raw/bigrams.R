library(quanteda)
library(magrittr)
library(edgeR)
library(data.table)

# Load document feature matrices and convert to table
dfm1 = readRDS(file.path('dfm1.rds'))
dt1 = docfreq(dfm1) %>%
  data.table(
    suggest = names(.),
    freq = .,
    stringsAsFactors=FALSE
  )
dt1 = dt1[,gtprop := goodTuringProportions(freq)][order(gtprop)]

dfm2 = readRDS(file.path('dfm2.rds'))
dt2 = docfreq(dfm2) %>%
  data.table(
    gram = names(.),
    freq = .,
    stringsAsFactors=FALSE
  ) %>%
  .[order(freq)]

# Drop dfms and reclaim some memory
rm(dfm1, dfm2)
gc()

# Spread gram
dt2 = dt2 %>%
  tidyr::separate(gram, c("key1", "suggest"), sep = " ", remove=TRUE) %>%
  setkey(key1)

# Set parameters for bigram table construction
keep = 15   # number of suggestions to keep. Applied at each level
keygroups = dt2[,unique(key1)]

bigrams =
  lapply(keygroups, function(x) {
    # subset keygroup records
    sugg = dt2[x]
    # calculate proportions for seen records
    sugg[,gtprop := goodTuringProportions(freq)]
    # calculate prevalence of unseen grams
    P0 = goodTuring(sugg$freq)$P0
    # calculate proportion of dt1 that's already covered in dt2
    lower_CoverP = dt1[suggest %in% sugg$suggest, gtprop] %>% sum
    # remove suggestions already in higher ngram, keep select amount
    sugg_lower = dt1[!suggest %in% sugg$suggest, tail(.SD, keep)]
    # rescale prop after removing covered ngrams proportion from 1
    sugg_lower[, gtprop := gtprop / (1 - lower_CoverP)]
    # rescale prop as share of P0
    sugg_lower[, gtprop := gtprop * P0]
    # add key value and ngram length to dt1
    sugg_lower[, key1 := x ]
    sugg_lower[, ngramL := 1L ]
    # add ngram length to dt2
    sugg[, ngramL := 2L ]
    # bind lower level ngram with higher
    combined = rbindlist(list(sugg, sugg_lower), use.names=TRUE, fill = FALSE)
    # keep only the top select amount
    combined[order(gtprop), tail(.SD, keep)]
  }) %>%
  rbindlist(use.names=FALSE, fill = FALSE) %>%
  setkey(key1, gtprop)

saveRDS(bigrams, 'bigrams.rds')
