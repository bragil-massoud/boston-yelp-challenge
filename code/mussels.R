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

basepath = "Desktop/DeveloperSivan/boston-yelp-challenge/data/"
#basepath = "Desktop/DeveloperSivan/boston-yelp-challenge/data/"
source(paste0(basepath, "../code/helpers.R"))

#registerDoMC(cores = 7)

#previous inspection results
train_lab = read.csv(paste0(basepath, "train_labels.csv"))
names(train_lab) = c("id", "date", "restaurant_id", "V1", "V2", "V3")

#explore:
View(summary(train_lab))
#10% of the data lacks the real date and shows last day of 2012 and 2013
#JEbamOR restaurantID has been controlled most frequently (45)
#V2 is least frequent in the data

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
#sivan keep credit card information
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

business$full_address = gsub("\\n", " ", business$full_address)

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
business = data.frame(business)
business[sapply(business, is.logical)] = lapply(business[sapply(business, is.logical)], as.factor)

write.csv(business, paste0(basepath, "business.csv"), row.names=F)

#join previous inspection data with yelp business data to form training set
business_train = inner_join(business, train_yelp)
business_train$date = ymd(business_train$date)
business_train$weekday = as.factor(paste0("WD", wday(business_train$date)))
business_train$weekday = ifelse(business_train$weekday == "WD1", "SUN", ifelse(business_train$weekday == "WD7", "SAT", "WORK"))
business_train$month = as.factor(paste0("M", lubridate::month(business_train$date)))
business_train$weird_date = paste(month(business_train$date), day(business_train$date), sep="-") == "12-30"
business_train$month=NULL
business_train$year = as.factor(paste0("Y", year(business_train$date)))

#business_train = group_by(business_train, business_id) %>%
#  arrange(date) %>%
#  mutate(prev_V1 = lag(V1), prev_V2 = lag(V2), prev_V3 = lag(V3),
#         avg_V1 = cummean(ifelse(is.na(lag(V1)), 0, lag(V1))),
#         avg_V2 = cummean(ifelse(is.na(lag(V2)), 0, lag(V2))),
#         avg_V3 = cummean(ifelse(is.na(lag(V3)), 0, lag(V3))),
#         days_since_last=as.numeric(date-lag(date), "days"))

business_train[is.na(business_train)] = 0

#rolling join with review data: always join latest data before inspection
#read topic data in as data.table for rolling join
#sivan: loko at review_lda_topics.R and investigate topics
review_topics_cumulative = fread(paste0(basepath, "review_topics_cumulative25.csv"))
review_topics_cumulative$date = ymd(review_topics_cumulative$date )
business_train_lda =
  data.table(select(review_topics_cumulative, restaurant_id, date, starts_with("X")),
             key=c("restaurant_id", "date"))[
               data.table(business_train,
                          key=c("restaurant_id", "date")),
               roll=T, nomatch=0
               ]

tips_topics_cumulative = fread(paste0(basepath, "tips_topics_cumulative10.csv"))
tips_topics_cumulative$date = ymd(tips_topics_cumulative$date )
business_train_lda_tips =
  data.table(select(tips_topics_cumulative, restaurant_id, date, starts_with("tip_X")),
             key=c("restaurant_id", "date"))[
               data.table(business_train_lda,
                          key=c("restaurant_id", "date")),
               roll=T, nomatch=NA
               ]

review_numeric = fread(paste0(basepath, "review_numeric.csv"))
review_numeric$date = ymd(review_numeric$date )
business_train_lda_tips =
  data.table(select(review_numeric, restaurant_id, date, starts_with("stars"), stars_entropy),
             key=c("restaurant_id", "date"))[
               data.table(business_train_lda_tips,
                          key=c("restaurant_id", "date")),
               roll=T, nomatch=NA
               ]

business_train_lda_tips[is.na(business_train_lda_tips)] = 0
business_train_lda_tips = data.frame(business_train_lda_tips)

write.csv(business_train_lda_tips, paste0(basepath, "business_train.csv"), row.names=F)



