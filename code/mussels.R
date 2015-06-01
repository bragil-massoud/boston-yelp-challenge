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

#previous inspection results
train_lab = read.csv(paste0(basepath, "train_labels.csv"))
names(train_lab) = c("id", "date", "restaurant_id", "V1", "V2", "V3")

#map inspections to yelp ids, taking care of multiple ids?
train_yelp = id2yelp %>%
  filter(business_id != "") %>%
  select(restaurant_id, business_id) %>%
  inner_join(train_lab)

#join with information on how many reviews (And the data range) we have in case of multiple
#yelp ids for a business (we will only keep on yelp entry for metadata for now, but use all reviews)
active_dates = read.csv(paste0(basepath, "active_dates.csv"))
set.seed(123)
best_yelp = active_dates %>%
  group_by(restaurant_id) %>%
  mutate(n = n + runif(length(n))) %>%
  top_n(1, n) %>%
  ungroup() %>%
  select(business_id, restaurant_id)
  
train_yelp = inner_join(train_yelp, best_yelp) %>% select(-id)

#yelp business meta data
business = multi_json(paste0(basepath, "yelp_academic_dataset_business.json"))
#clean col names
names(business) = gsub(" ", ".", names(business))
#we don;t care which credit card, convert list col to simple logical col by applying any()
business$attributes.Accepts.Credit.Cards = sapply(business$attributes.Accepts.Credit.Cards, any)
#convert list based category col to mutiple logical columns, one per category
categories = multi_model_matrix(select(business, business_id, data=categories), "nocategory", "cat_")
#convert list based neighborhood col to mutiple logical columns, one per neighborhood
neighborhoods =  multi_model_matrix(select(business, business_id, data=neighborhoods), "unknown_neighborhood", "hood_")
business = inner_join(business, categories) %>%
  inner_join(neighborhoods) %>%
  select(-categories, -neighborhoods)
#replace NA with F
business[is.na(business)] = F
#more col name cleanup
names(business) = gsub("[ /&()'-]", ".", names(business))

#convert some cols to factor for randomforest
business$attributes.BYOB.Corkage = as.factor(business$attributes.BYOB.Corkage)
business$attributes.Ages.Allowed = as.factor(business$attributes.Ages.Allowed)
business$attributes.Smoking = as.factor(business$attributes.Smoking)
business$attributes.Alcohol = as.factor(business$attributes.Alcohol)
business$attributes.Wi.Fi = as.factor(business$attributes.Wi.Fi)
business$attributes.Noise.Level = as.factor(business$attributes.Noise.Level)
business$attributes.Attire = as.factor(business$attributes.Attire)

#business all opening hours given as HH:MM to fractional hours as numeric
business=mutate_each(business,
                     funs(ifelse(. == "FALSE", NA,
                                 as.numeric(sub(":.*", "", .)) + as.numeric(sub(".*:", "", .))/60)),
                     contains("hours."))

#define new feature: open late friday night
business$late_night=!is.na(business$hours.Friday.close) & (
  business$hours.Friday.close > 23 | business$hours.Friday.close < 5)

business$lunch_only=!is.na(business$hours.Wednesday.close) & (
  business$hours.Wednesday.close < 15)

business$ncat = apply(select(business, contains("cat_")), 1, sum)

business[is.na(business)] = 0
write.csv(business, paste0(basepath, "business.csv"), row.names=F)

#join previous inspection data with yelp business data to form training set
business_train = inner_join(business, train_yelp)
business_train$date = ymd(business_train$date)
business_train$weekday = as.factor(wday(business_train$date))
business_train$month = as.factor(month(business_train$date))

#rolling join with review data: always join latest data before inspection
#read topic data in as data.table for rolling join
review_topics_cumulative = fread(paste0(basepath, "review_topics_cumulative25.csv"))
review_topics_cumulative$date = ymd(review_topics_cumulative$date )
business_train_lda =
  data.table(select(review_topics_cumulative, restaurant_id, date, starts_with("X")),
             key=c("restaurant_id", "date"))[
               data.table(business_train,
                          key=c("restaurant_id", "date")),
               roll=T, nomatch=0
               ]


business_train_frac=sample_frac(business_train_lda, 0.5)
#rf.caret =train(select(business_train_frac,-V1,-V2,-V3,-date,-id,-restaurant_id,-business_id,
#                       -full_address,-open,-name,-latitude,-longitude,
#                       -type, -state, -city,-contains("hours.")),
#                business_train_frac$V1,
#                method="rf",
#                trControl = trainControl(number = 5, verboseIter=T))
#varImpPlot(rf.caret$finalModel)

#rpart=train(select(business_train_frac,-V1,-V2,-V3,-restaurant_id,-business_id,
#                       -full_address,-open,-name,-latitude,-longitude,
#                       -type, -state, -city, -date),
#            business_train_frac$V3,
#      method="rpart",
#      tuneGrid = data.frame(cp = 0.005),
#      trControl = trainControl(number = 1, verboseIter=T))
#fancyRpartPlot(rpart$finalModel)


rf.V1 = train(select(business_train_lda,-V1,-V2,-V3,-date,-restaurant_id,-business_id,
                     -full_address,-open,-name,-latitude,-longitude,
                     -type, -state, -city,-contains("hours.")),
              log(business_train_lda$V1+1),
              method="rf",
              tuneGrid = data.frame(.mtry = c(10)),
              trControl = trainControl(number = 1, verboseIter=T),
              importance=T)

rf.V2 = train(select(business_train_lda,-V1,-V2,-V3,-date,-restaurant_id,-business_id,
                     -full_address,-open,-name,-latitude,-longitude,
                     -type, -state, -city,-contains("hours.")),
              log(business_train_lda$V2+1),
              method="rf",
              tuneGrid = data.frame(.mtry = c(10)),
              trControl = trainControl(number = 1, verboseIter=T),
              importance=T)

rf.V3 = train(select(business_train_lda,-V1,-V2,-V3,-date,-restaurant_id,-business_id,
                     -full_address,-open,-name,-latitude,-longitude,
                     -type, -state, -city,-contains("hours.")),
              log(business_train_lda$V3+1),
              method="rf",
              tuneGrid = data.frame(.mtry = c(10)),
              trControl = trainControl(number = 1, verboseIter=T),
              importance=T)

models = list(V1 = rf.V1$finalModel, V2 = rf.V2$finalModel, V3 = rf.V3$finalModel)
save(models, file=paste0(basepath, "models_rf10_lda25.Rdata"))
