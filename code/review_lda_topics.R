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

review= multi_json(paste0(basepath, "yelp_academic_dataset_review.json"))
review$date = ymd(review$date)

# create a table of how long each yelp entry was active, to accurately map restaurant meta data in case of name changes 
# to inspection dates
active_dates = group_by(review, business_id) %>%
  summarize(start = min(date), end=max(date), n=n(), peak=median(date)) %>%
  inner_join(id2yelp)

write.csv(active_dates, paste0(basepath, "active_dates.csv"), row.names=F)

review_numeric = select(review, date, stars, business_id, review_id) %>%
  inner_join(id2yelp) %>%
  select(-business_id, -key) %>%
  mutate(stars =  paste0("stars_", stars), dummy = 1) %>%
  spread(stars, dummy, 0) %>%
  group_by(restaurant_id) %>%
  arrange(date) %>%
  mutate(stars_1 = cumsum(stars_1),
         stars_2 = cumsum(stars_2),
         stars_3 = cumsum(stars_3),
         stars_4 = cumsum(stars_4),
         stars_5 = cumsum(stars_5),
         total = stars_1+stars_2+stars_3+stars_4+stars_5) %>%
  mutate(stars_1 = stars_1/total,
         stars_2 = stars_2/total,
         stars_3 = stars_3/total,
         stars_4 = stars_4/total,
         stars_5 = stars_5/total,
         stars_entropy = stars_1 * log(stars_1+1e-9) +
           stars_2 * log(stars_2+1e-9) +
           stars_3 * log(stars_3+1e-9) +
           stars_4 * log(stars_4+1e-9) +
           stars_5 * log(stars_5+1e-9)) %>%
  select(-review_id, -total)
review_numeric = review_numeric[!duplicated(select(review_numeric, restaurant_id, date)),]
write.csv(review_numeric, paste0(basepath, "review_numeric.csv"), row.names=F)

review.mat = tfidf_matrix(review$text)

#debug: show list of top scoring tf-idf words
words = apply(review.mat, 2, function(x) sum(x)/sum(x>0))
words = data.frame(word = names(words), score = words)
words %<>% arrange(desc(score))
ggplot(words, aes(x=score)) + geom_histogram()

#perform lda, commented out and saved to disk to save time
#review5.lda = LDA(review.mat, k=5, control=list(verbose=T))
#save(review5.lda, file=paste0(basepath, "review5.lda"))
#review10.lda = LDA(review.mat, k=10, control=list(verbose=T))
#save(review10.lda, file=paste0(basepath, "review10.lda"))
#review25.lda = LDA(review.mat, k=25, control=list(verbose=T))
#save(review25.lda, file=paste0(basepath, "review25.lda"))

load(file=paste0(basepath, "review5.lda"))
load(file=paste0(basepath, "review10.lda"))
load(file=paste0(basepath, "review25.lda"))


cumlda(review, "review_id", review.mat, review5.lda, "review_topics_cumulative5.csv", id2yelp)
cumlda(review, "review_id", review.mat, review10.lda, "review_topics_cumulative10.csv", id2yelp)
cumlda(review, "review_id", review.mat, review25.lda, "review_topics_cumulative25.csv", id2yelp)



