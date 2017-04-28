library(quanteda)
library(magrittr)
library(edgeR)
library(data.table)

dfm3 = readRDS(file.path('dfm3.rds'))
dt3 = docfreq(dfm3) %>%
  data.table(
    gram = names(.),
    freq = .,
    stringsAsFactors=FALSE
  ) %>%
  .[order(freq)]

# Drop dfms and reclaim some memory
rm(dfm3)
gc()

# Spread gram
dt3 = dt3 %>%
  tidyr::separate(gram, c("key1", "key2", "suggest"), sep = " ", remove=TRUE) %>%
  setkey(key1, key2)

# load bigrams
bigrams = readRDS('bigrams.rds')

# Set parameters for bigram table construction
keep = 15   # number of suggestions to keep. Applied at each level
keygroups = dt3[,.(key1, key2)][,unique(.SD)]
keygroups = tail(keygroups, 10)

trigrams =
  apply(keygroups, 1, function(x) {
    # subset keygroup records
    sugg = dt3[.(x[['key1']], x[['key2']])]
    # calculate proportions for seen records
    sugg[,gtprop := goodTuringProportions(freq)]
    # calculate prevalence of unseen grams
    P0 = goodTuring(sugg$freq)$P0

    # subset bigrams that match key2
    lower = bigrams[x[['key2']]]
    # calculate proportion of bigrams that's already covered in dt3
    lower_CoverP = lower[suggest %in% sugg$suggest, gtprop] %>% sum
    # remove suggestions already in higher ngram, keep select amount
    sugg_lower = lower[!suggest %in% sugg$suggest, tail(.SD, keep)]
    # rescale prop after removing covered ngrams proportion from 1
    sugg_lower[, gtprop := gtprop / (1 - lower_CoverP)]
    # rescale prop as share of P0
    sugg_lower[, gtprop := gtprop * P0]
    # add key keys to lower gram suggestions
    sugg_lower[,key1 := x[['key1']]]
    sugg_lower[,key2 := x[['key2']]]
    # add ngram length to dt2
    sugg[, ngramL := 3L ]
    # bind lower level ngram with higher
    combined = rbindlist(list(sugg, sugg_lower), use.names=TRUE, fill = FALSE)
    # keep only the top select amount
    combined[order(gtprop), tail(.SD, keep)]
  }) %>%
  rbindlist(use.names=FALSE, fill = FALSE) %>%
  setkey(key1, gtprop)

devtools::use_data(trigrams, overwrite=TRUE)
