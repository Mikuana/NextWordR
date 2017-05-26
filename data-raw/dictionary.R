library(data.table)

# maximum number of suggestions to retain for each keypair
maxsuggest = 3
# filter suggestions by the minimum number of occurences
freqfloor = 1
# define pattern which allow keys and suggestions without any characters outside the pattern
ec = "[^a-zA-Z0-9\\-_@#&]"


gram1 = readRDS(file.path('data-raw','gram1.rds'))
g1 = gram1[,tail(.SD, maxsuggest)]
g1[,key1:='']
g1[,key2:='']
g1[,key3:='']
g1[,key4:='']
g1[,key5:='']
rm(gram1)
gc()


gram2 = readRDS(file.path('data-raw','gram2.rds'))
g2 = gram2[!grepl(ec, key1) & !grepl(ec, suggest)]
# To handle ties and NaN gtprop estimates, random numbers are assigned
set.seed(86253)
g2[, runif := runif(.I)]
# Order before selecting last (top) of each key group
setorder(g2, key1, gtprop, freq, runif)
# Keep only a limited number of suggestion per key group
g2 = g2[freq > freqfloor,tail(.SD, maxsuggest), by=key1]
g2[,key2:='']
g2[,key3:='']
g2[,key4:='']
g2[,key5:='']
rm(gram2)
gc()


gram3 = readRDS(file.path('data-raw','gram3.rds'))
g3 = gram3[!grepl(ec, key1) & !grepl(ec, key2) & !grepl(ec, suggest)]
set.seed(86253)
g3[, runif := runif(.I)]
setorder(g3, key1, key2, gtprop, freq, runif)
g3 = g3[freq > freqfloor, tail(.SD, maxsuggest), by=.(key1, key2)]
g3[,key3:='']
g3[,key4:='']
g3[,key5:='']
rm(gram3)
gc()


gramfreq4 = readRDS(file.path('data-raw','gramfreq4.rds'))
g4 = gramfreq4[!grepl(ec, key1) & !grepl(ec, key2) & !grepl(ec, key3) & !grepl(ec, suggest)]
set.seed(86253)
g4[, gtprop := edgeR::goodTuringProportions(freq), by=.(key1, key2, key3)]
g4[, runif := runif(.I)]
setorder(g4, key1, key2, key3, gtprop, freq, runif)
g4 = g4[freq > freqfloor, tail(.SD, maxsuggest), by=.(key1, key2, key3)]
g4[,key4:='']
g4[,key5:='']
rm(gramfreq4)
gc()


gramfreq5 = readRDS(file.path('data-raw','gramfreq5.rds'))
g5 = gramfreq5[!grepl(ec, key1) & !grepl(ec, key2) & !grepl(ec, key3) & !grepl(ec, key4) & !grepl(ec, suggest)]
set.seed(86253)
g5[, gtprop := edgeR::goodTuringProportions(freq), by=.(key1, key2, key3, key4)]
g5[, runif := runif(.I)]
setorder(g5, key1, key2, key3, key4, gtprop, freq, runif)
g5 = g5[freq > freqfloor, tail(.SD, maxsuggest), by=.(key1, key2, key3, key4)]
g5[,key5:='']
rm(gramfreq5)
gc()


gramfreq6 = readRDS(file.path('data-raw','gramfreq6.rds'))
g6 = gramfreq6[!grepl(ec, key1) & !grepl(ec, key2) & !grepl(ec, key3)
               & !grepl(ec, key4) & !grepl(ec, key5) & !grepl(ec, suggest)]
set.seed(86253)
g6[, gtprop := edgeR::goodTuringProportions(freq), by=.(key1, key2, key3, key4, key5)]
g6[, runif := runif(.I)]
setorder(g6, key1, key2, key3, key4, key5, gtprop, freq, runif)
g6 = g6[freq > freqfloor, tail(.SD, maxsuggest), by=.(key1, key2, key3, key4, key5)]
rm(gramfreq6)
gc()

grams = rbindlist(
  list(g6,g5,g4,g3,g2,g1),
  use.names=TRUE, fill = TRUE
)

grams[, key1 := as.factor(key1)]
grams[, key2 := as.factor(key2)]
grams[, key3 := as.factor(key3)]
grams[, key4 := as.factor(key4)]
grams[, key5 := as.factor(key5)]
grams[, suggest := as.factor(suggest)]
setkey(grams, key1, key2, key3, key4, key5, gtprop, freq, runif)
grams = grams[, -c('gtprop', 'freq', 'runif')]
saveRDS(grams, 'data-raw/grams.rds')
