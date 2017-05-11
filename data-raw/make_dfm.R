library(quanteda)
library(magrittr)

corp = readRDS(file.path('data-raw', 'corp.train.rds'))

dfm1 = dfm(
  corp, ngrams=1, tolower=TRUE, concatenator = " ",
  remove_numbers = TRUE, remove_punct = TRUE, remove_symbols = TRUE
)
saveRDS(dfm1, file.path('data-raw', 'dfm1.rds'))
rm(dfm1)
gc()

dfm2 = dfm(
  corp, ngrams=2, tolower=TRUE, concatenator = " ",
  remove_numbers = TRUE, remove_punct = TRUE, remove_symbols = TRUE
)
saveRDS(dfm2, file.path('data-raw', 'dfm2.rds'))
rm(dfm2)
gc()

dfm3 = dfm(
  corp, ngrams=3, tolower=TRUE, concatenator = " ",
  remove_numbers = TRUE, remove_punct = TRUE, remove_symbols = TRUE
)
saveRDS(dfm3, file.path('data-raw', 'dfm3.rds'))
rm(dfm3)
gc()

dfm4 = dfm(
  corp, ngrams=4, tolower=TRUE, concatenator = " ",
  remove_numbers = TRUE, remove_punct = TRUE, remove_symbols = TRUE
)
saveRDS(dfm4, file.path('data-raw', 'dfm4.rds'))
rm(dfm4)
gc()


rm(corp)
gc()
