library(quanteda)
library(magrittr)
library(edgeR)
library(data.table)
library(foreach)

# Parameters
keep = 15  # number of suggestions to keep. Applied at each level

timestamp()

# Load monograms
monograms = readRDS(file.path('data-raw', 'monograms.rds'))

dfm2 = readRDS(file.path('data-raw', 'dfm2.rds'))
dt2 = docfreq(dfm2) %>%
  data.table(
    gram = names(.),
    freq = .,
    stringsAsFactors=FALSE
  ) %>%
  .[order(freq)]

# Drop dfms and reclaim some memory
rm(dfm2)
gc()

# Spread gram
dt2 = dt2 %>%
  tidyr::separate(gram, c("key1", "suggest"), sep = " ", remove=TRUE) %>%
  setkey(key1)

# create keygroups for iteration
keygroups = dt2

keygroups = keygroups[ , .(COUNT = .N), by=key1]

doParallel::registerDoParallel(cores=4)

bigrams =
  foreach(x = 1:nrow(keygroups)) %dopar% {
    kg = keygroups[x]
    # subset keygroup records
    sugg = dt2[.(kg[['key1']])]
    # calculate proportions for seen records
    sugg[,gtprop := goodTuringProportions(freq)]
    # calculate prevalence of unseen grams
    P0 = goodTuring(sugg$freq)$P0

    # add ngram length to dt2
    sugg[, ngramL := 2L ]

    if(P0 %in% c(0,1)) { # catch groups where Good-Turing estimation fails
      combined = sugg[NA]  # return a blank record for group
    } else {
      # calculate proportion of dt1 that's already covered in dt2
      lower_CoverP = monograms[suggest %in% sugg$suggest, gtprop] %>% sum
      # remove suggestions already in higher ngram, keep select amount
      sugg_lower = monograms[!suggest %in% sugg$suggest, tail(.SD, keep)]
      # rescale prop after removing covered ngrams proportion from 1
      sugg_lower[, gtprop := gtprop / (1 - lower_CoverP)]
      # rescale prop as share of P0
      sugg_lower[, gtprop := gtprop * P0]
      # add key value and ngram length to dt1
      sugg_lower[, key1 := kg[['key1']]]
      # bind lower level ngram with higher
      combined = rbindlist(list(sugg, sugg_lower), use.names=TRUE, fill = FALSE)
    }

    # keep only a specified number of suggestions
    combined[order(gtprop), tail(.SD, keep)]
  } %>%
  rbindlist(use.names=FALSE, fill = FALSE)

setkey(bigrams, key1, gtprop)
bigrams = bigrams[complete.cases(bigrams),]

saveRDS(bigrams, file.path('data-raw','bigrams.rds'))

timestamp()
