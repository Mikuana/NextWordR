library(quanteda)
library(magrittr)

# if you don't have the raw data already, use manage_data()
fp = function(x) { file.path('data-raw', paste0('en_US.', x,'.txt')) }
txt.twitter = readLines(fp('twitter'), encoding='ISO-8859-2')
txt.blogs = readLines(fp('blogs'), encoding='ISO-8859-2')
txt.news = readLines(fp('news'), encoding='UTF-8')

set.seed(57130)
txt =
  list(txt.twitter, txt.blogs, txt.news) %>%
  sapply(., function(data) {
    ix = caret::createDataPartition(1:length(data), p=0.1, list=FALSE)
    data[ix]
  }) %>%
  unlist

rm(txt.blogs, txt.news, txt.twitter)
gc()

set.seed(983212)
ix.test = caret::createDataPartition(1:length(txt), p=0.3, list=FALSE) %>% drop
txt.test = txt[ix.test]
txt.train = txt[-ix.test]

corp.train = corpus(txt.train)
corp.test = corpus(txt.test)

saveRDS(corp.train, file.path('data-raw', 'corp.train.rds'))
saveRDS(corp.test, file.path('data-raw', 'corp.test.rds'))

gc()
