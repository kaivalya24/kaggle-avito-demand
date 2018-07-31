#xgboost using caret with 5 fold cross validation
fitcontrol <- trainControl(method="cv",number=5,savePredictions = "all",verboseIter = T)
xgb <- train(form=label_train ~ .,data=train_rf,method="xgbTree",trControl=fitcontrol,
             tuneGrid=expand.grid( nrounds = 500,max_depth = 18,eta = 0.05,gamma = 0
                                   ,colsample_bytree = 0.7,min_child_weight = 11
                                   ,subsample = 0.8))
gbm <- train(form=label_train ~ .,data=train_rf,method="gbm_h2o",trControl=fitcontrol,
             tuneGrid=expand.grid( ntrees = 50,max_depth = 18,min_rows=10,learn_rate=0.01,col_sample_rate=0.7))
#h2o ensemble
library(h2oEnsemble)
h2o.init(nthreads=2)
h2o.removeAll()
train_h2o<-as.h2o(train_rf,"train_h2o")
test_h2o <- as.h2o(test_rf,"test_h2o")
esmbl <- h2o.ensemble(x=colnames(train_rf1),y="deal_probability",training_frame =train_h2o,family="gaussian",cvControl = list(V=5),seed=100,
                      keep_levelone_data = T)
perf <- h2o.ensemble_performance(esmbl, newdata = test_h2o)
modeling_table_test_m <- data.matrix(modeling_table_test)
final_test <- as.h2o(modeling_table_test_m)
predictions <- predict(esmbl,final_test)
deal_probability <- as.data.frame(predictions$pred)
final_output <- cbind(modeling_table_test,deal_probability)
submission <- subset(final_output,select=c("item_id","deal_probability"))
write.csv(submission,file="kaivalya_submission1.csv",row.names=F)