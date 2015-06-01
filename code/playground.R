
hours = group_by(business_train, hours.Friday.open, hours.Friday.close) %>%
  summarise(n=n(), V1=mean(V1))
ggplot(hours,aes(x=hours.Friday.open, y=hours.Friday.close, fill=V1)) + geom_tile()



ggplot(business_train, aes(x=as.factor(ncat), y=V1)) + geom_violin()
