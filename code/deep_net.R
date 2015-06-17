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
library(h2o)

basepath = "/mnt//r-devel/simon//boston-yelp-challenge/data/"
source(paste0(basepath, "../code/helpers.R"))


business_train_lda_tips=read.csv(paste0(basepath, "business_train.csv"))
business_train_lda_tips=sample_frac(business_train_lda_tips, 1)
business_train_lda_tips %<>% mutate(V1 = log(V1+1),
                                    V2 = log(V2+1)
                                    V3 = log(V3+1))
write.csv(business_train_lda_tips, paste0(basepath, "business_train_log.csv"), row.names=F)




localH2O = h2o.init(ip = "10.240.5.189", start=F)
#h2o.downloadAllLogs(localH2O)
business_train_lda_tips_h20 = h2o.importFile(localH2O, path = paste0(basepath, "business_train_log.csv"))
x = which(names(business_train_lda_tips) %in% setdiff(names(business_train_lda_tips),
                                                      select(head(business_train_lda_tips),-V1,-V2,-V3,-date,-restaurant_id,-business_id,
                                                             -full_address,-open,-name,-latitude,-longitude,
                                                             -type, -state, -city,-contains("hours."))))
y = which(names(business_train_lda_tips)== "V1")
deep.V1 = h2o.deeplearning(x=1:x, y=y, data=business_train_lda_tips_h20, nfolds=5,
                           hidden=list(c(200,100, 100), c(200,200, 100),c(500,500, 100)), epochs=0.1, activation="Tanh",
                           classification=FALSE)
sapply(deep.V1@xval, function(x) (x@model$valid_class_error))
deep.V1@model$varimp[1:10]


estimatedTotalRMSLE =(min(rf.V1$results$RMSE) + 2 * min(rf.V2$results$RMSE) + 5 * min(rf.V3$results$RMSE)) /3

models = list(V1 = rf.V1$finalModel, V2 = rf.V2$finalModel, V3 = rf.V3$finalModel)
save(models, file=paste0(basepath, "models_rf10_lda25_lda10_lag.Rdata"))
