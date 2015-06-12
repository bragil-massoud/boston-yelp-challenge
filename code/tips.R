library(magrittr)
library(dplyr)
library(tidyr)
library(jsonlite)
library(lubridate)
library(SnowballC)
library(tm)
library(topicmodels)
library(data.table)

basepath = "/mnt//r-devel/simon//boston-yelp-challenge/data/"
source(paste0(basepath, "../code/helpers.R"))

tips= multi_json(paste0(basepath, "yelp_academic_dataset_tip.json"))
tips$date = ymd(tips$date)
tips$tip_id = 1:nrow(tips)

tips.mat = tfidf_matrix(tips$text)

words = apply(tips.mat, 2, function(x) sum(x)/sum(x>0))
#perform lda, commented out and saved to disk to save time

#tips10.lda = LDA(tips.mat, k=10, control=list(verbose=T))
#save(tips10.lda, file=paste0(basepath, "tips10.lda"))

load(file=paste0(basepath, "tips10.lda"))
terms(tips10.lda, 50)
cumlda(tips, "tip_id", tips.mat, tips10.lda, "tips_topics_cumulative10.csv", id2yelp, "tip_")
