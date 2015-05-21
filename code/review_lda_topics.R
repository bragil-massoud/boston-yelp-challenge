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
#negation tagging fuse not and don't to following word
review$text = gsub("not\\s+", " not_", review$text)
review$text = gsub("n't\\s+", " not_", review$text)
#create corpus and document term matrix
#stem, remove stopwords, numbers and punctuation, words < 3 and words appearing in every second doc
#weight by tf-idf
rc = Corpus(VectorSource(review$text))
ndoc = length(rc)
review.mat = as.matrix(DocumentTermMatrix(rc,
                                          control=list(stemming = TRUE, stopwords = TRUE, minWordLength = 3,
                                                       removeNumbers = TRUE, removePunctuation = TRUE,
                                                       bounds=list(global=c(ndoc/1000, ndoc/2)),
                                                       weighting = function(x) weightTfIdf(x, normalize = FALSE))
                                          )
                       )
#convert tf-idf to integer to fool topicmodels library into using them as word counts
review.mat = round(review.mat * 100)
#remove empty documents, will crash topicmodels
rowTotals = apply(review.mat , 1, sum)
review.mat = review.mat[rowTotals> 0, ]  
#debug: show list of top scoring tf-idf words
words = apply(review.mat, 2, function(x) sum(x)/sum(x>0))

#perform lda, commented out and saved to disk to save time
#review.lda = LDA(review.mat, k=10, control=list(verbose=T))
#save(review.lda, file=paste0(basepath, "review.lda"))
load(file=paste0(basepath, "review.lda"))

#show topics and their most important words
review.topics = topics(review.lda, 1)
review.topics.terms <- terms(review.lda, 50)
review.topics.terms

#debug: which topic associated with "notfresh"
review.lda@beta[,which(review.lda@terms=="notfresh")]

review_topics = inner_join(select(review, -text),
                           data.frame(review_id = review[rownames(review.mat),]$review_id,
                                      review.lda@gamma))

review_topics_tall = gather(review_topics, topic, value, starts_with("X"))

review_topics_tall %<>%
  group_by(business_id, topic) %>%
  arrange(date) %>%
  mutate(value = cumsum(value), numreview = row_number(), value = value/numreview)

review_topics_cumulative = review_topics_tall %>%
  spread(key=topic, value = value)

review_topics_cumulative = review_topics_cumulative[!duplicated(select(review_topics_cumulative, business_id, date))]

write.csv(review_topics_cumulative, paste0(basepath, "review_topics_cumulative.csv"), row.names=F)
