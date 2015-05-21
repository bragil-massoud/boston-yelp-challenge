library(magrittr)
library(dplyr)
library(tidyr)
library(jsonlite)
library(caret)
library(party)
library(randomForest)
library(lubridate)
library(rattle)
library(doMC)
library(data.table)

basepath = "/mnt//r-devel/simon//boston-yelp-challenge/data/"
source(paste0(basepath, "../code/helpers.R"))

registerDoMC(cores = 7)

#boston restaurant inspection to yelp mapping
id2yelp = read.csv(paste0(basepath, "restaurant_ids_to_yelp_ids.csv"))

#previous inspection results
train_lab = read.csv(paste0(basepath, "train_labels.csv"))
names(train_lab) = c("id", "date", "restaurant_id", "V1", "V2", "V3")

#map inspections to yelp ids, taking care of multiple ids?
train_yelp = gather(id2yelp, key, business_id, -restaurant_id) %>%
  filter(business_id != "") %>%
  select(restaurant_id, business_id) %>%
  inner_join(train_lab)

#yelp business meta data
business = multi_json(paste0(basepath, "yelp_academic_dataset_business.json"))
#clean col names
names(business) = gsub(" ", ".", names(business))
#we don;t care which credit card, convert list col to simple logical col by applying any()
business$attributes.Accepts.Credit.Cards = sapply(business$attributes.Accepts.Credit.Cards, any)
#convert list based category col to mutiple logical columns, one per category
categories = multi_model_matrix(select(business, business_id, data=categories), "nocategory")
#convert list based neighborhood col to mutiple logical columns, one per neighborhood
neighborhoods =  multi_model_matrix(select(business, business_id, data=neighborhoods), "unknown_neighborhood")
business = inner_join(business, categories) %>%
  inner_join(neighborhoods) %>%
  select(-categories, -neighborhoods)
#replace NA with F
business[is.na(business)] = F
#more col name cleanup
names(business) = gsub("[ /&-]", ".", names(business))

#join previous inspection data with yelp business data to form training set
business_train = inner_join(business, train_yelp)
business_train[is.na(business_train)] = 0
business_train$date = ymd(business_train$date)
#convert some cols to factor for randomforest
business_train$attributes.BYOB.Corkage = as.factor(business_train$attributes.BYOB.Corkage)
business_train$attributes.Ages.Allowed = as.factor(business_train$attributes.Ages.Allowed)
business_train$attributes.Smoking = as.factor(business_train$attributes.Smoking)
business_train$attributes.Alcohol = as.factor(business_train$attributes.Alcohol)
business_train$attributes.Wi.Fi = as.factor(business_train$attributes.Wi.Fi)
business_train$attributes.Noise.Level = as.factor(business_train$attributes.Noise.Level)
business_train$attributes.Attire = as.factor(business_train$attributes.Attire)

#convert all opening hours given as HH:MM to fractional hours as numeric
business_train=mutate_each(business_train,
                           funs(ifelse(. == "FALSE", NA,
                                       as.numeric(sub(":.*", "", .)) + as.numeric(sub(".*:", "", .))/60)),
                           contains("hours."))

#define new feature: open late friday night
business_train$late_night=!is.na(business_train$hours.Friday.close) & (
  business_train$hours.Friday.close > 23 | business_train$hours.Friday.close < 5)


#rolling join with review data: always join latest data before inspection
#read topic data in as data.table for rolling join
review_topics_cumulative = fread(paste0(basepath, "review_topics_cumulative.csv"))
review_topics_cumulative$date = ymd(review_topics_cumulative$date )
business_train_lda =
  data.table(select(review_topics_cumulative, business_id, date, starts_with("X")),
             key=c("business_id", "date"))[
               data.table(business_train,
                          key=c("business_id", "date")),
               roll=T, nomatch=0
               ]


business_train_frac=sample_frac(business_train_lda, 0.01)
rf.caret =train(select(business_train_frac,-V1,-V2,-V3,-date,-id,-restaurant_id,-business_id,
                       -full_address,-open,-name,-latitude,-longitude,
                       -type, -state, -city,-contains("hours.")),
                business_train_frac$V1,
                method="rf",
                trControl = trainControl(number = 10, verboseIter=T))
varImpPlot(rf.caret$finalModel)

rpart=train(select(business_train_frac,-V1,-V2,-V3,-id,-restaurant_id,-business_id,
                       -full_address,-open,-name,-latitude,-longitude,
                       -type, -state, -city),
            business_train_frac$V1,
      method="rpart")
fancyRpartPlot(rpart$finalModel)



