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
library(gbm)

basepath = "/mnt//r-devel/simon//boston-yelp-challenge/data/"
source(paste0(basepath, "../code/helpers.R"))

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



submission = read.csv(paste0(basepath, "SubmissionFormat.csv")) %>%
  select(id, date, restaurant_id)

business = read.csv(paste0(basepath, "business.csv"))

business_train = read.csv(paste0(basepath, "business_train.csv"))


submission_data = inner_join(submission, id2yelp) %>%
  inner_join(best_yelp) %>%
  inner_join(business) %>%
  mutate(date = ymd(date))
submission_data$weekday = as.factor(paste0("WD", wday(submission_data$date)))
submission_data$weekday = as.factor(ifelse(submission_data$weekday == "WD1", "SUN", ifelse(submission_data$weekday == "WD7", "SAT", "WORK")))
submission_data$month = as.factor(paste0("M", lubridate::month(submission_data$date)))
submission_data$weird_date = paste(month(submission_data$date), day(submission_data$date), sep="-") == "12-30"
submission_data$month=NULL
submission_data$year = as.factor(paste0("Y", year(submission_data$date)))


previous = rbind(select(submission_data, business_id, date) %>% mutate(V1=NA, V2=NA, V3=NA),
                 select(business_train, business_id, date, V1, V2, V3)) %>%
  group_by(business_id) %>%
  arrange(date) %>%
  mutate(prev_V1 = lag(V1), prev_V2 = lag(V2), prev_V3 = lag(V3),
         avg_V1 = cummean(ifelse(is.na(lag(V1)), 0, lag(V1))),
         avg_V2 = cummean(ifelse(is.na(lag(V2)), 0, lag(V2))),
         avg_V3 = cummean(ifelse(is.na(lag(V3)), 0, lag(V3))),
         days_since_last=as.numeric(date-lag(date), "days")) %>%
  filter(is.na(V1)) %>%
  select(-V1, -V2, -V3)
previous[is.na(previous)] = 0

#submission_data = inner_join(submission_data, previous)

#special hack for test submission: use reviews from the future
review_topics_cumulative = fread(paste0(basepath, "review_topics_cumulative25.csv"))
review_topics_cumulative$date = ymd(review_topics_cumulative$date )
submission_data_lda =
  data.table(select(review_topics_cumulative, restaurant_id, date, starts_with("X")),
             key=c("restaurant_id", "date"))[
               data.table(submission_data,
                          key=c("restaurant_id", "date")),
               roll="nearest", nomatch=0
               ]

tips_topics_cumulative = fread(paste0(basepath, "tips_topics_cumulative10.csv"))
tips_topics_cumulative$date = ymd(tips_topics_cumulative$date )
submission_data_lda =
  data.table(select(tips_topics_cumulative, restaurant_id, date, starts_with("tip_X")),
             key=c("restaurant_id", "date"))[
               data.table(submission_data_lda,
                          key=c("restaurant_id", "date")),
               roll=T, nomatch=NA
               ]

review_numeric = fread(paste0(basepath, "review_numeric.csv"))
review_numeric$date = ymd(review_numeric$date )
submission_data_lda =
  data.table(select(review_numeric, restaurant_id, date, starts_with("stars"), stars_entropy),
             key=c("restaurant_id", "date"))[
               data.table(submission_data_lda,
                          key=c("restaurant_id", "date")),
               roll=T, nomatch=NA
               ]

submission_data_lda[is.na(submission_data_lda)] = 0
submission_data_lda = as.data.frame(submission_data_lda)

check_cols(business_train, submission_data_lda)

load(file=paste0(basepath, "models_rf16_lda25_lda10_lag.Rdata"))
rf.V1 = models$V1
rf.V2 = models$V2
rf.V3 = models$V3
results_rf = data.frame(id = submission_data_lda$id, date = submission_data_lda$date, restaurant_id = as.character(submission_data_lda$restaurant_id),
                     V1=round(exp(predict(rf.V1, submission_data_lda))-1),
                     V2=round(exp(predict(rf.V2, submission_data_lda))-1),
                     V3=round(exp(predict(rf.V3, submission_data_lda))-1))
write_submission(results_rf, submission, submission_data_lda, "submission_rf_")


load(file=paste0(basepath, "models_rfcat16_lda25_lda10.Rdata"))
rf.cat = models$rf.cat$finalModel
rf.cat.trans = models$rf.cat.trans
results_rfcat = data.frame(id = submission_data_lda$id, date = submission_data_lda$date, restaurant_id = as.character(submission_data_lda$restaurant_id),
                        violation_cat=predict(rf.cat, submission_data_lda))
results_rfcat %<>% inner_join(rf.cat.trans) %>%
  select(-violation_cat, -nr, -perc, -cum_n, -cum_perc) %>%
  mutate(V1 = round(exp(V1) - 1), V2 = round(exp(V2) - 1), V3 = round(exp(V3) - 1))

write_submission(results_rfcat, submission, submission_data_lda, "submission_rfcat_")


load(file=paste0(basepath, "models_rf_pca_16_lda25_lda10.Rdata"))
rf.pca.V1 = models$pca.V1
rf.pca.V2 = models$pca.V2
rf.pca.V3 = models$pca.V3
rf.pca = models$pca
results_rfpca = data.frame(id = submission_data_lda$id, date = submission_data_lda$date, restaurant_id = as.character(submission_data_lda$restaurant_id),
                           Comp.1=predict(rf.pca.V1, submission_data_lda),
                           Comp.2=predict(rf.pca.V2, submission_data_lda),
                           Comp.3=predict(rf.pca.V3, submission_data_lda))
inspection_space = as.matrix(select(results_rfpca, Comp.1, Comp.2, Comp.3)) %*% t(rf.pca$loadings)
results_rfpca$Comp.1 = round(exp(inspection_space[,1])-1)
results_rfpca$Comp.2 = round(exp(inspection_space[,2])-1)
results_rfpca$Comp.3 = round(exp(inspection_space[,3])-1)

write_submission(results_rfpca, submission, submission_data_lda, "submission_rfpca_")



load(file=paste0(basepath, "models_gbm_lda25_lda10.Rdata"))
submission_data_lda_gbm = data.frame(submission_data_lda)
submission_data_lda_gbm[sapply(submission_data_lda_gbm, is.logical)] =
  lapply(submission_data_lda_gbm[sapply(submission_data_lda_gbm, is.logical)], as.factor)
submission_data_lda_gbm = submission_data_lda_gbm[, names(select(business_train,-V1,-V2,-V3,-date,-restaurant_id,-business_id,
                                                         -full_address,-open,-name,-latitude,-longitude,-attributes.Accepts.Credit.Cards,
                                                         -type, -state, -city,-contains("hours.")))]
results_gbm = data.frame(id = submission_data_lda$id, date = submission_data_lda$date, restaurant_id = as.character(submission_data_lda$restaurant_id),
                     V1=round(exp(predict(models$V1, n.trees =  100, submission_data_lda_gbm)-1)),
                     V2=round(exp(predict(models$V2, n.trees =  100, submission_data_lda_gbm)-1)),
                     V3=round(exp(predict(models$V3, n.trees =  100, submission_data_lda_gbm)-1)))

write_submission(results_gbm, submission, submission_data_lda, "submission_rfgbm_")


qplot(results_rf$V3, results_gbm$V3) + geom_jitter()

