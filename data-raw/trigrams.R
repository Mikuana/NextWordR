library(quanteda)
library(magrittr)
library(edgeR)
library(data.table)
library(progress)
timestamp()

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
chunksize = 10000
iterations =  ceiling(nrow(keygroups) / chunksize)

pb <- progress_bar$new(total = iterations)

# TODO: deal with NaN in gtprop caused by a single record in keygroup
for(i in 1:iterations) {
  pb$tick()

  rowstart = (i-1)*chunksize + 1
  rowend = ifelse(i==iterations, nrow(keygroups), i*chunksize)

  trigrams =
    apply(keygroups[rowstart:rowend,], 1, function(x) {
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


  saveRDS(trigrams, stringr::str_interp('trigrams${i}.rds'))
  gc()
}

# Discover all of the RDS files that were written using pattern matching
files = tools::list_files_with_exts('.', 'rds', full.names = FALSE)
files = files[grepl('trigrams\\d{1,}', files)]
trigrams =
  lapply(files, function(x){
    readRDS(x)
  }) %>%
  rbindlist

trigrams[,key1 := as.factor(key1)]
trigrams[,key2 := as.factor(key2)]
trigrams[,suggest := as.factor(suggest)]
setkey(trigrams, key1, key2, gtprop)

saveRDS(trigrams, 'trigrams.rds')
file.remove(files)

timestamp()
