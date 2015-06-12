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
library(gbm)

basepath = "/mnt//r-devel/simon//boston-yelp-challenge/data/"
source(paste0(basepath, "../code/helpers.R"))


business_train_lda_tips=read.csv(paste0(basepath, "business_train.csv"))
business_train_lda_tips=sample_frac(business_train_lda_tips, 1)

business_train_lda_tips = data.frame(business_train_lda_tips)
business_train_lda_tips[sapply(business_train_lda_tips, is.logical)] =
  lapply(business_train_lda_tips[sapply(business_train_lda_tips, is.logical)], as.factor)

grid=expand.grid(n.trees=c(75,150), interaction.depth = c(10,15), shrinkage=c(0.05, 0.1, 0.2), n.minobsinnode=10)
gbm.V1 = train(select(business_train_lda_tips,-V1,-V2,-V3,-date,-restaurant_id,-business_id,
                      -full_address,-open,-name,-latitude,-longitude,
                      -type, -state, -city,-contains("hours.")),
               log(business_train_lda_tips$V1+1),
               method="gbm",
               tuneGrid = grid,
               trControl = trainControl(number = 5, verboseIter=T))


gbm.V2 = train(select(business_train_lda_tips,-V1,-V2,-V3,-date,-restaurant_id,-business_id,
                      -full_address,-open,-name,-latitude,-longitude,
                      -type, -state, -city,-contains("hours.")),
               log(business_train_lda_tips$V2+1),
               method="gbm",
               tuneGrid = grid,
               trControl = trainControl(number = 5, verboseIter=T))

gbm.V3 = train(select(business_train_lda_tips,-V1,-V2,-V3,-date,-restaurant_id,-business_id,
                      -full_address,-open,-name,-latitude,-longitude,
                      -type, -state, -city,-contains("hours.")),
               log(business_train_lda_tips$V3+1),
               method="gbm",
               tuneGrid = grid,
               trControl = trainControl(number = 5, verboseIter=T))

estimatedTotalRMSLE =(min(gbm.V1$results$RMSE) + 2 * min(gbm.V2$results$RMSE) + 5 * min(gbm.V3$results$RMSE)) /3

models = list(V1 = gbm.V1$finalModel, V2 = gbm.V2$finalModel, V3 = gbm.V3$finalModel)
save(models, file=paste0(basepath, "models_gbm_lda25_lda10.Rdata"))
