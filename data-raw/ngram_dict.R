library(dplyr)
library(data.table)

monograms = readRDS(file.path('data-raw', 'monograms.rds'))
bigrams = readRDS(file.path('data-raw', 'bigrams.rds'))
trigrams = readRDS(file.path('data-raw', 'trigrams.rds'))

ngrams = rbindlist(
  list(trigrams, bigrams, monograms),
  use.names = TRUE,
  fill = TRUE
)

# cast as factors
ngrams[, key1 := as.factor(key1)]
ngrams[, key2 := as.factor(key2)]
ngrams[, suggest := as.factor(suggest)]
# set key groups as data.table keys
setkey(ngrams, key1, key2, gtprop)

# Keep only top 5 suggestions for each key group
ngrams = ngrams[, tail(.SD, 5), by=.(key1, key2)]

ngram_dict = ngrams

devtools::use_data(ngram_dict)
