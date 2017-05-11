library(quanteda)
library(magrittr)
library(edgeR)
library(data.table)

# Parameters
keep = 20  # number of suggestions to keep. Applied at each level

timestamp()

# Load document feature matrices and convert to table
dfm1 = readRDS(file.path('data-raw', 'dfm1.rds'))
monograms = docfreq(dfm1) %>%
  data.table(
    suggest = names(.),
    freq = .,
    stringsAsFactors=FALSE
  )

monograms = monograms[,gtprop := goodTuringProportions(freq)][order(gtprop)]
monograms[, ngramL := 1]  # add ngram length to monograms

saveRDS(monograms, file.path('data-raw', 'monograms.rds'))

timestamp()
