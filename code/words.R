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

review= multi_json(paste0(basepath, "yelp_academic_dataset_review.json"))
review$date = ymd(review$date)

review.mat = tfidf_matrix(review$text, counts=T, mindoc=100, maxdoc=3)

colnames(review.mat) = paste0("word_", colnames(review.mat))
review$rownames = review$review_id
review.mat.df = inner_join(select(review, -text, -type),
                           data.frame(rownames = review[rownames(review.mat),]$review_id, review.mat),
                           by = "rownames")

#review.mat.df.tall = gather(review.mat.df, word, score, starts_with("word_"))
#review.mat.df.tall = filter(review.mat.df.tall, score > 0)
#review.mat.df.tall =
#  inner_join(review.mat.df.tall, id2yelp) %>%
#  group_by(restaurant_id, word)

#review.mat.df.tall = arrange(review.mat.df.tall, date) %>%
#  mutate(score = cumsum(score), numreview = row_number(), score = score/numreview) %>%
#  select(restaurant_id, date, word, score, numreview)

#review.mat.df.cum = review.mat.df.tall %>%
#  ungroup() %>%
#  head(100) %>%
#  spread(key=word, value = score, fill=0)
review.mat.df.cum = review.mat.df %>%
  inner_join(id2yelp) %>%
  group_by(restaurant_id) %>%
  arrange(desc(date)) %>%
  mutate_each(funs(cummean), starts_with("word_")) %>%
  ungroup()  %>% 
  arrange(restaurant_id, desc(date)) %>%
  select(restaurant_id, date, starts_with("word_"))

review.mat.df.cum = review.mat.df.cum[!duplicated(select(review.mat.df.cum, restaurant_id, date)),]

write.csv(review.mat.df.cum, paste0(basepath, "words100.csv"), row.names=F)
