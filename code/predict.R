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

load(file=paste0(basepath, "models.Rdata"))

submission = read.csv(paste0(basepath, "SubmissionFormat.csv")) %>%
  select(id, date, restaurant_id)

business = read.csv(paste0(basepath, "business.csv"))

submission_data = inner_join(submission, id2yelp) %>%
  inner_join(best_yelp) %>%
  inner_join(business) %>%
  mutate(date = ymd(date))

#special hack for test submission: use reviews from the future
review_topics_cumulative = fread(paste0(basepath, "review_topics_cumulative.csv"))
review_topics_cumulative$date = ymd(review_topics_cumulative$date )
submission_data_lda =
  data.table(select(review_topics_cumulative, restaurant_id, date, starts_with("X")),
             key=c("restaurant_id", "date"))[
               data.table(submission_data,
                          key=c("restaurant_id", "date")),
               roll="nearest", nomatch=0
               ]

submission_data_lda[is.na(submission_data_lda)] = 0

results = data.frame(id = submission_data_lda$id,
                     date = submission_data_lda$date,
                     restaurant_id = as.character(submission_data_lda$restaurant_id),
                     V1=round(exp(predict(models$V1, newdata=submission_data_lda)-1)),
                     V2=round(exp(predict(models$V2, newdata=submission_data_lda)-1)),
                     V3=round(exp(predict(models$V3, newdata=submission_data_lda)-1)))

names(results) = c("id", "date", "restaurant_id", "*", "**", "***")
results$restaurant_id = as.character(submission_data_lda$restaurant_id)
results  = inner_join(select(submission, id, date, restaurant_id) %>% mutate(date = ymd(date)), results)

write.csv(results, paste0(basepath, "submission_", Sys.Date(), ".csv"), row.names=F, quote=F)

