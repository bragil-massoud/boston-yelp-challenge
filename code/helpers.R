#boston restaurant inspection to yelp mapping
id2yelp = read.csv(paste0(basepath, "restaurant_ids_to_yelp_ids.csv")) %>%
  gather(key, business_id, -restaurant_id)
names(id2yelp) = c("restaurant_id", "key", "business_id")


# col name data required
multi_model_matrix = function(df, emptycolname, prefix="") {
  max_cat =  max(sapply(df$data, length))
  cat_cols = paste(rep("Category", max_cat), 1:max_cat,sep="")
  df$data = sapply(df$data,function(x) paste(paste(prefix, x, sep=""), collapse=","))
  df = separate(df,
                data,
                into=cat_cols,
                sep = ",", remove=T, extra="drop")
  df = gather_(df, 'dummy', 'category', cat_cols)
  names(df) = c("business_id", "dummy", "category")
  df %<>% filter(!is.na(category))
  df$category = ifelse(df$category=="", emptycolname, as.character(df$category))
  df$indicator = T
  df$dummy = NULL
  spread(df, category, indicator, fill=F)
}

multi_json = function(filename) {
  json = paste("[",
        paste(readLines(filename),
              collapse=", "),
        "]")
  return(flatten(fromJSON(json)))
}

tfidf_matrix = function(text) {
  #negation tagging fuse not and don't to following word
  text = gsub("not\\s+", " not_", text)
  text = gsub("n't\\s+", " not_", text)
  #create corpus and document term matrix
  #stem, remove stopwords, numbers and punctuation, words < 3 and words appearing in every second doc
  #weight by tf-idf
  rc = Corpus(VectorSource(text))
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
  return(review.mat)
}


cumlda = function(data, idcol, tdf, lda, filename, id2yelp, prefix="") {
  #debug: show topics and their most important words
  review.topics = topics(lda, 1)
  review.topics.terms <- terms(lda, 150)
  review.topics.terms
  
  #debug: texts that talk a lot about topic 10
  #review[rownames(tdf)[which(lda@gamma[,5]>0.7)],]
  
  #debug: which topic associated with "service"
  lda@beta[,which(lda@terms=="service")]
  
  data[,c("rownames")] = data[,c(idcol)]
  data_topics = inner_join(select(data, -text),
                           data.frame(rownames = data[rownames(tdf),c(idcol)], lda@gamma))
  
  data_topics_tall = gather(data_topics, topic, value, starts_with("X"))
  
  data_topics_tall =
    inner_join(data_topics_tall, id2yelp) %>%
    group_by(restaurant_id, topic)
  
  data_topics_tall = arrange(data_topics_tall, date) %>%
    mutate(value = cumsum(value), numreview = row_number(), value = value/numreview) %>%
    select(restaurant_id, date, topic, value, numreview)
  
  data_topics_cumulative = data_topics_tall %>%
    ungroup() %>%
    mutate(topic = paste0(prefix, topic)) %>%
    spread(key=topic, value = value)
  
  data_topics_cumulative = data_topics_cumulative[!duplicated(select(data_topics_cumulative, restaurant_id, date)),]
  
  write.csv(data_topics_cumulative, paste0(basepath, filename), row.names=F)
}

write_submission = function(results, submission,submission_lda, name) {
  names(results) = c("id", "date", "restaurant_id", "*", "**", "***")
  results$restaurant_id = as.character(submission_lda$restaurant_id)
  results  = inner_join(select(submission, id, date, restaurant_id) %>% mutate(date = ymd(date)), results)
  
  write.csv(results, paste0(basepath, name, Sys.Date(), ".csv"), row.names=F, quote=F)
}

check_cols = function(business_train, submission_data_lda) {
  #check factor levels between train and test
  print(setdiff(names(business_train), names(submission_data_lda)))
  print(setdiff(names(submission_data_lda), names(business_train)))
  
  train_class = data.frame(var = names(business_train), cltrain = as.character(sapply(business_train, class)))
  sub_class = data.frame(var = names(submission_data_lda), clsub = as.character(sapply(submission_data_lda, class)))
  classes = inner_join(train_class, sub_class)
  print(filter(classes, as.character(cltrain) != as.character(clsub)))
  
  levels_sub = sapply((submission_data_lda[,c(which(sapply(submission_data_lda, class)=="factor"))]), levels)
  levels_train = sapply((business_train[,c(which(sapply(business_train, class)=="factor"))]), levels)
  for(f in unique(c(names(levels_sub), names(levels_train)))){
    diff_levels = setdiff(levels_sub[[f]], levels_train[[f]])
    if(!is.null(diff_levels) && length(diff_levels > 0) && !(f %in%  c("name", "city", "full_address"))) {
      print(f)
      print(diff_levels)
    }
  }
  
}

rmsle = function(data, models, truths, weights) {
  predictions = sapply(models, function(m) predict(m, data)) %*% weights
  truths = as.matrix(truths) %*% weights
}