cond_cont = function(data, var, cuts=10, minc=10) {
  if(cuts > 0) {
    if(cuts == 1) {
      data[,c(var)] = data[,c(var)] > median(data[,c(var)])
    } else {
      data[,c(var)] = cut(data[,c(var)], quantile(data[,c(var)], 0:cuts/cuts))
    }
  } 
  data = data %>%
    select_(.dots = c(var, 'V1', "V2", "V3")) %>%
    mutate(V1 = round(log(1+V1)), V2 = round(log(1+V2)), V3 = round(log(1+V3))) %>%
    gather(level, violations, V1, V2, V3) %>%
    group_by_(.dots = c(var, "level", "violations")) %>%
    dplyr::summarize(n=n()) %>%
    group_by_(.dots = c(var, "level")) %>%
    mutate(total = sum(n), n = n / sum(n)) %>%
    filter(total > minc)
  
  ggplot(data, aes_string(x=var, y="violations", fill="n")) + geom_tile() +facet_grid(~level)+theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
}
cond_cont(business_train, "stars_entropy", cuts=5)
cond_cont(business_train, "stars_5", cuts=3)
cond_cont(business_train, "X20", cuts=5)
cond_cont(business_train, "stars", cuts=0, minc=50)
cond_cont(business_train, "latitude", cuts=15)
cond_cont(business_train, "longitude", cuts=15)
t=business_train %>% mutate(date = floor_date(date, "month"))
cond_cont(t, "date", cuts=0, minc=10)

cond_cont(business_train_words, "word_fish", cuts=1)

cond_discrete = function(data, var, logscale=T, minc=50) {
  data = data %>%
    select_(.dots = c(var, 'V1', "V2", "V3"))
  if(logscale) {
    data %<>% mutate(V1 = round(log(1+V1)), V2 = round(log(1+V2)), V3 = round(log(1+V3)))
  }
  data %<>%
    gather(level, violations, V1, V2, V3) %>%
    group_by_(.dots = c(var, "level", "violations")) %>%
    dplyr::summarize(n=n()) %>%
    group_by_(.dots = c(var, "level")) %>%
    mutate(total = sum(n), n = n / total) %>%
    filter(total > minc)
 
  ggplot(data, aes_string(x=var, y="violations", fill="n")) + geom_tile() +facet_grid(~level) +theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
}
cond_discrete(business_train, "weekday")
cond_discrete(business_train, "weird_date")
cond_discrete(business_train, "late_night")
cond_discrete(business_train, "lunch_only")
cond_discrete(business_train, "hood_Back.Bay")
business_train$date = ymd(business_train$date)
cond_discrete(business_train, "date",minc=5)

d = group_by(results_rf, V1log = round(log(1+`*`), 0), V2log = round(log(1+`**`),0)) %>% dplyr::summarise(n=n())
ggplot(d, aes(x=V2log, y=V1log, fill=n)) + geom_tile()  



business_train_lag = group_by(business_train, business_id) %>%
  arrange(date) %>%
  mutate(prev_V1 = lag(V1), prev_V2 = lag(V2), prev_V3 = lag(V3),
         avg_V1 = cummean(ifelse(is.na(lag(V1)), 0, lag(V1))),
         avg_V2 = cummean(ifelse(is.na(lag(V2)), 0, lag(V2))),
         avg_V3 = cummean(ifelse(is.na(lag(V3)), 0, lag(V3))),
         days_since_last=as.numeric(ymd(date)-lag(ymd(date)), "days"))
business_train_lag = data.frame(business_train_lag)

cond_discrete(business_train_lag, "prev_V2", logscale=T)
cond_cont(business_train_lag, "avg_V1")
cond_cont(business_train_lag, "avg_V2", cuts=2)
cond_cont(business_train_lag, "avg_V3", cuts=2)
cond_discrete(business_train_lag, "days_since_last", logscale=T)

View(business_train_lag %>% filter(business_id == "0a9pgOiIpgcaO9mlCy0ZQA") %>% select(business_id, name, date, V1, V2, V3, prev_V1, prev_V2, prev_V3, avg_V1, avg_V2, avg_V3, days_since_last))



