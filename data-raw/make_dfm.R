library(quanteda)
library(dplyr)

# if you don't have the raw data already, use our function manage_data()
fp = function(x) { file.path(paste0('en_US.', x,'.txt')) }
txt.twitter = readLines(fp('twitter'), encoding='ISO-8859-2')
txt.blogs = readLines(fp('blogs'), encoding='ISO-8859-2')
txt.news = readLines(fp('news'), encoding='UTF-8')
rm(fp)


set.seed(57130)
txt =
  list(txt.twitter, txt.blogs, txt.news) %>%
  sapply(., function(data) {
    ix = caret::createDataPartition(1:length(data), p=0.04, list=FALSE)
    data[ix]
  }) %>%
  unlist

# ignore the EOF errors

rm(txt.blogs, txt.news, txt.twitter)
gc()
corp = corpus(txt)

dfm1 = dfm(
  corp, ngrams=1, tolower=TRUE, concatenator = " ",
  remove_numbers = TRUE, remove_punct = TRUE, remove_symbols = TRUE
)
saveRDS(dfm1, 'dfm1.rds')
rm(dfm1)
gc()


dfm2 = dfm(
  corp, ngrams=2, tolower=TRUE, concatenator = " ",
  remove_numbers = TRUE, remove_punct = TRUE, remove_symbols = TRUE
)
saveRDS(dfm2, 'dfm2.rds')
rm(dfm2)
gc()


dfm3 = dfm(
  corp, ngrams=3, tolower=TRUE, concatenator = " ",
  remove_numbers = TRUE, remove_punct = TRUE, remove_symbols = TRUE
)
saveRDS(dfm3, 'dfm3.rds')
rm(dfm3)
gc()

rm(corp, txt)
gc()
