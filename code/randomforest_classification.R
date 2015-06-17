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

basepath = "/mnt//r-devel/simon//boston-yelp-challenge/data/"
source(paste0(basepath, "../code/helpers.R"))


business_train_lda_tips=read.csv(paste0(basepath, "business_train.csv"))

business_train_lda_tips %<>% mutate(V1 = pmin(round(log(V1+1)), 3),
                                    V2 = pmin(round(log(V2+1)), 1),
                                    V3 = pmin(round(log(V3+1)), 2))

cat_size = group_by(business_train_lda_tips, V1, V2, V3) %>%
  summarise(nr=n()) %>%
  ungroup() %>%
  arrange(desc(nr)) %>%
  mutate(perc = 100 * nr / sum(nr),
         cum_n = cumsum(nr),
         cum_perc = cumsum(perc)) %>%
  mutate(violation_cat = paste(V1, V2, V3, sep="_"))

print(cat_size)

business_train_lda_tips = inner_join(business_train_lda_tips, cat_size)
business_train_lda_tips=sample_frac(business_train_lda_tips, 1)


rf.cat = train(select(business_train_lda_tips,-V1,-V2,-V3,-date,-restaurant_id,-business_id,
                     -full_address,-open,-name,-latitude,-longitude,
                     -type, -state, -city,-contains("hours."),
                     -nr, -perc, -cum_n, -cum_perc, -violation_cat),
               as.factor(business_train_lda_tips$violation_cat),
              method="rf",
              tuneGrid = data.frame(.mtry = c(16)),
              trControl = trainControl(method = "none", verboseIter=T),
              importance=T)
varImpPlot(rf.cat$finalModel, type=1)

models = list(rf.cat = rf.cat, rf.cat.trans = cat_size)
save(models, file=paste0(basepath, "models_rfcat16_lda25_lda10.Rdata"))




