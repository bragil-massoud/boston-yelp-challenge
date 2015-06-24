library(dplyr)
library(lubridate)
basepath = "Desktop/DeveloperSivan/boston-yelp-challenge/data/"
source(paste0(basepath, "../code/MoveColumnsInDataFrame.R"))

yelpTC = read.csv("Desktop/DeveloperSivan/boston-yelp-challenge/data/business_train_old.csv", stringsAsFactors = F)


restaurants =  group_by(yelpTC, date) %>% summarize(Count = n()) %>% ungroup() %>% arrange(-Count)

dates = subset(restaurants, Count < 50)

dates$date = as.Date(dates$date,"%Y-%m-%d")
dates$Year = as.numeric(format(dates$date, format = "%Y"))
dates$Month = as.numeric(format(dates$date, format = "%m"))
dates$Day = as.numeric(format(dates$date, format = "%d"))
dates$Peak = ifelse(dates$Count > 10, ifelse(dates$Count > 20, "Peak", "Normal"), "Low")

yelpTC$date = as.Date(yelpTC$date,"%Y-%m-%d")
TOTAL = left_join(yelpTC, dates, by = "date")

TOTAL = moveMe(TOTAL, "Year", "after", "date")
TOTAL = moveMe(TOTAL, "Month", "after", "Year")
TOTAL = moveMe(TOTAL, "Day", "after", "Month")
TOTAL = moveMe(TOTAL, "Peak", "after", "Day")

write.csv(yelpTC, "Desktop/DeveloperSivan/boston-yelp-challenge/data/business_train_old.csv")
write.csv(TOTAL, "Desktop/DeveloperSivan/boston-yelp-challenge/data/business_train.csv")

