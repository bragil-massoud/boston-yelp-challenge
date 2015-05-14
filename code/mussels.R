library(magrittr)
library(dplyr)
library(tidyr)
library(jsonlite)
library(caret)
library(party)
library(randomForest)
library(lubridate)
library(rattle)
library(SnowballC)
library(tm)

# col name data required
multi_model_matrix = function(df, emptycolname) {
  max_cat =  max(sapply(df$data, length))
  cat_cols = paste(rep("Category", max_cat), 1:max_cat,sep="")
  df$data = sapply(df$data,function(x) paste(x, collapse=","))
  df = separate(df,
                data,
                into=cat_cols,
                sep = ",", remove=T, extra="drop")
  df = gather_(df, 'dummy', 'category', cat_cols)
  df %<>% filter(!is.na(category))
  df$category = ifelse(df$category=="", emptycolname, df$category)
  df$indicator = T
  df$dummy = NULL
  spread(df, category, indicator, fill=F)
}

id2yelp = read.csv("~/Documents/yelp_boston/restaurant_ids_to_yelp_ids.csv")



train_lab = read.csv("~/Documents/yelp_boston/train_labels.csv")
names(train_lab) = c("id", "date", "restaurant_id", "V1", "V2", "V3")

train_yelp = gather(id2yelp, key, business_id, -restaurant_id) %>%
  filter(business_id != "") %>%
  select(restaurant_id, business_id) %>%
  inner_join(train_lab)


business = paste("[",
                 paste(readLines("~/Documents/yelp_boston/yelp_academic_dataset_business.json"),
                       collapse=", "),
                 "]")
business = flatten(fromJSON(business))
names(business) = gsub(" ", ".", names(business))
business$attributes.Accepts.Credit.Cards = sapply(business$attributes.Accepts.Credit.Cards, any)
categories = multi_model_matrix(select(business, business_id, data=categories), "nocategory")
neighborhoods =  multi_model_matrix(select(business, business_id, data=neighborhoods), "unknown_neighborhood")
business = inner_join(business, categories) %>%
  inner_join(neighborhoods) %>%
  select(-categories, -neighborhoods)
business[is.na(business)] = F
names(business) = gsub("[ /&-]", ".", names(business))

business_train = inner_join(business, train_yelp)
business_train[is.na(business_train)] = 0
business_train$date = ymd(business_train$date)
business_train$attributes.BYOB.Corkage = as.factor(business_train$attributes.BYOB.Corkage)
business_train$attributes.Ages.Allowed = as.factor(business_train$attributes.Ages.Allowed)
business_train$attributes.Smoking = as.factor(business_train$attributes.Smoking)
business_train$attributes.Alcohol = as.factor(business_train$attributes.Alcohol)
business_train$attributes.Wi.Fi = as.factor(business_train$attributes.Wi.Fi)
business_train$attributes.Noise.Level = as.factor(business_train$attributes.Noise.Level)
business_train$attributes.Attire = as.factor(business_train$attributes.Attire)

names(business_train)[sapply(business_train, anyNA)]

hours = group_by(business_train, hours.Friday.open, hours.Friday.close) %>%
  summarise(n=n(), V1=mean(V1))
ggplot(hours,aes(x=hours.Friday.open, y=hours.Friday.close, fill=V1)) + geom_tile()

business_train=mutate_each(business_train,
                           funs(ifelse(. == "FALSE",
                                       NA,
                                       as.numeric(sub(":.*", "", .)) +
                                         as.numeric(sub(".*:", "", .))/60)
                                ),
                           contains("hours."))

business_train$late_night=!is.na(business_train$hours.Friday.close) & (
  business_train$hours.Friday.close > 23 | 
    business_train$hours.Friday.close < 5)

review = paste("[",
               paste(readLines("~/Documents/yelp_boston/yelp_academic_dataset_review.json"),
                     collapse=", "),
               "]")
review=fromJSON(review)
rc = Corpus(VectorSource(review$text))
rc = tm_map(rc, removePunctuation)
rc = tm_map(rc, stemDocument)
rc = tm_map(rc, removeWords, c(stopwords("english"))) 
ndoc = length(rc)
review.mat = as.data.frame(
  as.matrix(
    DocumentTermMatrix(rc, control=list(bounds=list(global=(c(ndoc/1000, ndoc/5)))))
  )
)


tip = paste("[",
            paste(readLines("~/Documents/yelp_boston/yelp_academic_dataset_tip.json"),
                  collapse=", "),
            "]")
tip=fromJSON(tip)



user = paste("[",
             paste(readLines("~/Documents/yelp_boston/yelp_academic_dataset_user.json"),
                   collapse=", "),
             "]")
user=fromJSON(user)



checkin = paste("[",
                paste(readLines("~/Documents/yelp_boston/yelp_academic_dataset_checkin.json"),
                      collapse=", "),
                "]")
checkin=fromJSON(checkin)



rpart=train(select(business_train,-V1,-V2,-V3,-id,-restaurant_id,-business_id,
                       -full_address,-open,-name,-latitude,-longitude,
                       -type, -state, -city),
      business_train$V1,
      method="rpart")
fancyRpartPlot(rpart$finalModel)

model2=train(select(business_train, Chinese, German, Hotels, Indian),
             business_train$V3,
             method="rf")

rf=randomForest(select(business_train,-V1,-V2,-V3,-date,-id,-restaurant_id,-business_id,
                       -full_address,-open,-name,-latitude,-longitude,
                       -type, -state, -city,-contains("hours.")),
                business_train$V3)

business_train_frac=sample_frac(business_train, 0.1)
rf.caret =train(select(business_train_frac,-V1,-V2,-V3,-date,-id,-restaurant_id,-business_id,
                           -full_address,-open,-name,-latitude,-longitude,
                           -type, -state, -city,-contains("hours.")),
                    business_train_frac$V1,
                method="rf",
                trControl = trainControl(number = 10, verboseIter=T))
