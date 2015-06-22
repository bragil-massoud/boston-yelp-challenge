library(magrittr)
library(dplyr)
library(tidyr)
library(jsonlite)
library(lubridate)
library(SnowballC)
library(tm)
library(data.table)

basepath = "/mnt//r-devel/simon//boston-yelp-challenge/data/"
source(paste0(basepath, "../code/helpers.R"))

business_train=read.csv(paste0(basepath, "business_train.csv"))
business_train$date = ymd(business_train$date)

words = fread(paste0("zcat ", basepath, "words100.csv.gz"))
words$date = ymd(words$date )
business_train_words =
  data.table(select(words, restaurant_id, date, starts_with("word_")),
             key=c("restaurant_id", "date"))[
               data.table(business_train,
                          key=c("restaurant_id", "date")),
               roll=T, nomatch=NA
               ]