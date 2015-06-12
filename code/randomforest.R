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
business_train_lda_tips=sample_frac(business_train_lda_tips, 1)


rf.V1 = train(select(business_train_lda_tips,-V1,-V2,-V3,-date,-restaurant_id,-business_id,
                     -full_address,-open,-name,-latitude,-longitude,
                     -type, -state, -city,-contains("hours.")),
              log(business_train_lda_tips$V1+1),
              method="rf",
              tuneGrid = data.frame(.mtry = c(16)),
              trControl = trainControl(method = "none", verboseIter=T),
              importance=T)
varImpPlot(rf.V1$finalModel, type=1)

rf.V2 = train(select(business_train_lda_tips,-V1,-V2,-V3,-date,-restaurant_id,-business_id,
                     -full_address,-open,-name,-latitude,-longitude,
                     -type, -state, -city,-contains("hours.")),
              log(business_train_lda_tips$V2+1),
              method="rf",
              tuneGrid = data.frame(.mtry = c(16)),
              trControl = trainControl(method = "none", verboseIter=T),
              importance=T)
varImpPlot(rf.V2$finalModel, type=1)

rf.V3 = train(select(business_train_lda_tips,-V1,-V2,-V3,-date,-restaurant_id,-business_id,
                     -full_address,-open,-name,-latitude,-longitude,
                     -type, -state, -city,-contains("hours.")),
              log(business_train_lda_tips$V3+1),
              method="rf",
              tuneGrid = data.frame(.mtry = c(16)),
              trControl = trainControl(method = "none", verboseIter=T),
              importance=T)
varImpPlot(rf.V3$finalModel, type=1)

estimatedTotalRMSLE =(min(rf.V1$results$RMSE) + 2 * min(rf.V2$results$RMSE) + 5 * min(rf.V3$results$RMSE)) /3

models = list(V1 = rf.V1$finalModel, V2 = rf.V2$finalModel, V3 = rf.V3$finalModel)
save(models, file=paste0(basepath, "models_rf16_lda25_lda10_lag.Rdata"))




