cond_cont = function(data, var, cuts=10, minc=10) {
  if(cuts > 0) {
    data[,c(var)] = cut(data[,c(var)], quantile(data[,c(var)], 0:cuts/cuts))
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
  
  ggplot(data, aes_string(x=var, y="violations", fill="n")) + geom_tile() +facet_grid(~level)
}
cond_cont(business_train, "stars_entropy", cuts=5)
cond_cont(business_train, "stars_5", cuts=3)
cond_cont(business_train, "X10", cuts=5)
cond_cont(business_train, "stars", cuts=0, minc=50)
t=business_train %>% mutate(date = floor_date(date, "month"))
cond_cont(t, "date", cuts=0, minc=10)

cond_discrete = function(data, var) {
  data = data %>%
    select_(.dots = c(var, 'V1', "V2", "V3")) %>%
    mutate(V1 = round(log(1+V1)), V2 = round(log(1+V2)), V3 = round(log(1+V3))) %>%
    gather(level, violations, V1, V2, V3) %>%
    group_by_(.dots = c(var, "level", "violations")) %>%
    dplyr::summarize(n=n()) %>%
    group_by_(.dots = c(var, "level")) %>%
    mutate(n = n / sum(n))

  ggplot(data, aes_string(x=var, y="violations", fill="n")) + geom_tile() +facet_grid(~level)
}
cond_discrete(business_train, "weekday")
cond_discrete(business_train, "weird_date")

d = group_by(results_rf, V1log = round(log(1+`*`), 0), V2log = round(log(1+`**`),0)) %>% dplyr::summarise(n=n())
ggplot(d, aes(x=V2log, y=V1log, fill=n)) + geom_tile()  
