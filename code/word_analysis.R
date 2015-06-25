library(magrittr)
library(dplyr)
library(tidyr)
library(jsonlite)
library(lubridate)
library(SnowballC)
library(tm)
library(data.table)
library(broom)
library(ggplot2)

#basepath = "/mnt//r-devel/simon//boston-yelp-challenge/data/"
basepath = "~/Desktop/DeveloperSivan/boston-yelp-challenge/data/"
source(paste0(basepath, "../code/helpers.R"))
source(paste0(basepath, "../code/playground.R"))

# read basic trainging data, date, restaurant, violations...
business_train=read.csv(paste0(basepath, "business_train.csv"))
business_train$date = ymd(business_train$date)

# load table with restaurant, date, and mean # of mentions of each word up to that date
words = fread(paste0(basepath, "words100.csv"))
words$date = ymd(words$date )
#join the two tables by taking the word informattion from the last review before the inspection
business_train_words =
  data.table(select(words, restaurant_id, date, starts_with("word_")),
             key=c("restaurant_id", "date"))[
               data.table(business_train,
                          key=c("restaurant_id", "date")),
               roll=T, nomatch=NA
               ]

#convert back to data.frame because data.table is weird
business_train_words = data.frame(business_train_words)
#sample plot: violations with reviews mentioning fish or not

cond_cont(business_train_words, "word_fish", cuts=1)

View(head(business_train_words))

#creating test set: 
words_test = head(business_train_words, 200)
#hypothesis: reviews with word x have more violations.
relwords = c()
estimates = c()
pvalues = c()
for(i in 1:ncol(business_train_words)){
  if(grepl("word", names(words_test)[i])){
    form = formula(paste("V1 ~ ", names(words_test)[i]))
    wordReg = lm(form, data = business_train_words)
    wordReg_tidy = tidy(wordReg)
    relwords = c(relwords, wordReg_tidy$term[2])
    estimates = c(estimates, wordReg_tidy$estimate[2])
    pvalues = c(pvalues, wordReg_tidy$p.value[2])  
  }
}

Regresults = data.frame(words = relwords, estimates = estimates, pvalues = pvalues) %>% arrange(pvalues)
Regresults$Bonferronicorrected = Regresults$pvalues*nrow(Regresults)
Regresults_relevant = subset(Regresults, log(pvalues) < -10)

words = data.frame(words)
Relevant_words = words[,Regresults_relevant$words]
Relevant_words$restaurant_id = words$restaurant_id
Relevant_words$date = words$date

write.csv(Relevant_words, paste(basepath, "Relevant_words.csv "))
View(names(words))

ggplot(data=Regresults, aes(log(Regresults$pvalues))) + geom_histogram()



fishReg = lm(V1 ~ word_absolut, data = business_train_words)
