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
library(doMC)
library(topicmodels)

basepath = "/mnt//r-devel/simon//boston-yelp-challenge/data/"
registerDoMC(cores = 7)

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

id2yelp = read.csv(paste0(basepath, "restaurant_ids_to_yelp_ids.csv"))



train_lab = read.csv(paste0(basepath, "train_labels.csv"))
names(train_lab) = c("id", "date", "restaurant_id", "V1", "V2", "V3")

train_yelp = gather(id2yelp, key, business_id, -restaurant_id) %>%
  filter(business_id != "") %>%
  select(restaurant_id, business_id) %>%
  inner_join(train_lab)


business = paste("[",
                 paste(readLines(paste0(basepath, "yelp_academic_dataset_business.json")),
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
               paste(readLines(paste0(basepath, "yelp_academic_dataset_review.json")),
                     collapse=", "),
               "]")
review=flatten(fromJSON(review))
review$text = gsub("not\\s+", " not_", review$text)
review$text = gsub("n't\\s+", " not_", review$text)
rc = Corpus(VectorSource(review$text))
ndoc = length(rc)
review.mat = as.matrix(DocumentTermMatrix(rc,
                                          control=list(stemming = TRUE, stopwords = TRUE, minWordLength = 3,
                                                       removeNumbers = TRUE, removePunctuation = TRUE,
                                                       bounds=list(global=c(ndoc/1000, ndoc/2)),
                                                       weighting = function(x) weightTfIdf(x, normalize = FALSE))
                                          )
                       )
review.mat = round(review.mat * 100)
rowTotals = apply(review.mat , 1, sum)
review.mat = review.mat[rowTotals> 0, ]     
words = apply(review.mat, 2, function(x) sum(x)/sum(x>0))

review.lda = LDA(review.mat, k=10, control=list(verbose=T))
save(review.lda, file=paste0(basepath, "review.lda"))
review.lda = load(file=paste0(basepath, "review.lda"))
review.topics = topics(review.lda, 1)
review.topics.terms <- terms(review.lda, 50)
review.topics.terms

review.lda@beta[,which(review.lda@terms=="notfresh")]




review_topics = inner_join(select(review, -text),
                           data.frame(review_id = review[rownames(review.mat),]$review_id,
                                      review.lda@gamma)) %>%
  inner_join(unique(select(business_train, business_id)))

relevant_review_words = function(last_date) {
  r = filter(review_topics, date <= last_date) %>% mutate(date = last_date) %>%
    group_by(date, business_id) %>%
    summarise_each(funs(mean), starts_with("X"))
  #print(paste(last_date, nrow(r)))
  return(r)
}

review_topics_tall = gather(review_topics, topic, value, starts_with("X"))

review_topics_tall %<>%
  group_by(business_id, topic) %>%
  arrange(date) %>%
  mutate(value = cumsum(value), numreview = row_number(), value = value/numreview)
  

reviews_till_date = business_train %>% group_by(date) %>% do(relevant_review_words(.$date[1]))

tip = paste("[",
            paste(readLines(paste0(basepath, "yelp_academic_dataset_tip.json")),
                  collapse=", "),
            "]")
tip=fromJSON(tip)



user = paste("[",
             paste(readLines(paste0(basepath, "yelp_academic_dataset_user.json")),
                   collapse=", "),
             "]")
user=fromJSON(user)



checkin = paste("[",
                paste(readLines(paste0(basepath, "yelp_academic_dataset_checkin.json")),
                      collapse=", "),
                "]")
checkin=fromJSON(checkin)


business_train_frac=sample_frac(business_train, 0.01)
rf.caret =train(select(business_train_frac,-V1,-V2,-V3,-date,-id,-restaurant_id,-business_id,
                       -full_address,-open,-name,-latitude,-longitude,
                       -type, -state, -city,-contains("hours.")),
                business_train_frac$V1,
                method="rf",
                trControl = trainControl(number = 10, verboseIter=T))

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


