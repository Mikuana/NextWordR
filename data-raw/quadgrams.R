library(quanteda)
library(magrittr)
library(edgeR)
library(data.table)
library(foreach)

# Parameters
keep = 5   # number of suggestions to keep. Applied at each level

timestamp()

# load trigrams
trigrams = readRDS(file.path('data-raw','trigrams.rds'))

dfm4 = readRDS(file.path('data-raw','dfm4.rds'))
dt4 = docfreq(dfm4) %>%
  data.table(
    gram = names(.),
    freq = .,
    stringsAsFactors=FALSE
  ) %>%
  .[order(freq)]

# Drop dfms and reclaim some memory
rm(dfm4)
gc()

# Spread gram
dt4 = dt4 %>%
  tidyr::separate(
    gram, c("key1", "key2", "key3", "suggest"), sep = " ", remove=TRUE
  ) %>%
  setkey(key1, key2, key3)

# create keygroups for iteration
keygroups = dt4

keygroups = keygroups[ , .(COUNT = .N), by=.(key1, key2, key3)]

doParallel::registerDoParallel(cores=4)

quadgram_chunker = function(ixs, fp) {
  quadgrams =
    foreach(x = ixs) %dopar% {
      kg = keygroups[x]
      # subset keygroup records
      sugg = dt4[.(kg[['key1']], kg[['key2']], kg[['key3']])]
      # calculate proportions for seen records
      sugg[,gtprop := goodTuringProportions(freq)]
      # calculate prevalence of unseen grams
      P0 = goodTuring(sugg$freq)$P0

      # add ngram length to dt4
      sugg[, ngramL := 4L ]

      if(P0 %in% c(0,1)) { # catch groups where Good-Turing estimation fails
        combined = sugg  # return only observed suggestions
      } else {
        # subset trigrams that match key2
        lower = trigrams[.(kg[['key2']], kg[['key3']])]
        # calculate proportion of trigrams that's already covered in dt4
        lower_CoverP = lower[suggest %in% sugg$suggest, gtprop] %>% sum
        # remove suggestions already in higher ngram, keep select amount
        sugg_lower = lower[!suggest %in% sugg$suggest, tail(.SD, keep)]
        # rescale prop after removing covered ngrams proportion from 1
        sugg_lower[, gtprop := gtprop / (1 - lower_CoverP)]
        # rescale prop as share of P0
        sugg_lower[, gtprop := gtprop * P0]
        # add key keys to lower gram suggestions
        sugg_lower[,key1 := kg[['key1']]]
        sugg_lower[,key2 := kg[['key2']]]
        sugg_lower[,key3 := kg[['key3']]]
        # bind lower level ngram with higher
        combined = rbindlist(list(sugg, sugg_lower), use.names=TRUE, fill = FALSE)
      }
      # keep only a specified number of suggestions
      combined[order(gtprop), tail(.SD, keep)]
    } %>%
    rbindlist(use.names=FALSE, fill = FALSE)

  if(file.exists(fp)) {
    quadgrams = rbindlist(list(readRDS(fp), quadgrams), use.names=FALSE, fill = FALSE)
  }
  saveRDS(quadgrams, fp, FALSE)
  gc()
}

seqs = function(length, chunksize) {
  seed = seq(from=1, to=length, by=chunksize)
  most = lapply(head(seed, -1), function(x) {
    x:(x+chunksize-1)
  })
  last = tail(seed,1):length
  c(most, list(last))
}

gfp = file.path('data-raw', 'quadgrams.rds')
if(file.exists(gfp)) { file.remove(gfp)}

for(i in seqs(nrow(keygroups), 20000)) {
  quadgram_chunker(i, gfp)
}

quadgrams = readRDS(gfp)
setkey(quadgrams, key1, key2, key3, gtprop)
quadgrams = quadgrams[complete.cases(quadgrams),]
saveRDS(quadgrams, gfp)

timestamp()
