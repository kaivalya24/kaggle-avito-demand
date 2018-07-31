library(xgboost)
library(mlr)
p1<- list(objective = "reg:logistic",
          booster = "gbtree",
          eval_metric = "rmse",
          nthread = 8,
          eta = 0.1,
          max_depth = 18,
          min_child_weight = 11,
          gamma = 0,
          subsample = 0.8,
          colsample_bytree = 0.7,
          alpha = 2.25,
          lambda = 0,
          nrounds = 500)
bst1<- xgboost(data=dTrain,params=p1,nrounds=p1$nrounds,print_every_n = 10)
pred1<- predict(bst1,dTest)
rmse1 <- sqrt(mean((test[,17]-pred1)^2))


#Hyperparameter tuning
lrn <- makeLearner("regr.xgboost",predict.type = "response")
lrn$par.vals <- list(booster = "gbtree", objective="reg:logistic", eval_metric="rmse",eta=0.05,nrounds=20)
traintask <- makeRegrTask (data = data.frame(train_subset),target = "deal_probability")
testtask <- makeRegrTask (data = data.frame(test_subset),target = "deal_probability")
params <- makeParamSet( makeIntegerParam("max_depth",lower = 10L,upper = 20L), 
                        makeNumericParam("min_child_weight",lower = 5L,upper = 15L), 
                        makeNumericParam("subsample",lower = 0.5,upper = 1), 
                        makeNumericParam("colsample_bytree",lower = 0.5,upper = 1),
                        makeNumericParam("gamma", lower=0.0, upper=5))
rdesc <- makeResampleDesc("CV",iters=5L)
set.seed(100)
ctrl <- makeTuneControlRandom(maxit = 100L)
mytune <- tuneParams(learner = lrn, task = traintask, resampling = rdesc, par.set = params, control = ctrl,show.info=T)
print(mytune)

#tuned model
p2<- list(objective = "reg:logistic",
          booster = "gbtree",
          eval_metric = "rmse",
          nthread = 8,
          eta = 0.05,
          max_depth = 19,
          min_child_weight = 11.2,
          gamma = 1.57,
          subsample = 0.854,
          colsample_bytree = 0.965,
          alpha = 2.25,
          lambda = 0,
          nrounds = 500)
bst2<- xgboost(data=dTrain,params=p2,nrounds=p2$nrounds,print_every_n = 10)
pred2<- predict(bst2,dTest)
rmse2 <- sqrt(mean((test[,17]-pred2)^2))
rmse2

#feature_correlation
library(ggcorrplot)
cor_mat<-cor_pmat(train)
ggcorrplot(cor_mat)
cor_mat


#random forest (taking forever)
library(randomForest)
train_rf <- data.matrix(modeling_table[trainIndex,])
train_rf1 <- data.frame(subset(train_rf,select=-c(deal_probability)))
label_train <- as.numeric(subset(train_rf,select=c(deal_probability)))
test_rf <- data.matrix(modeling_table[-trainIndex,])
test_rf1 <- data.frame(subset(test_rf,select=-c(user_id,item_id,title,description,deal_probability)))
label_test <- as.numeric(subset(test_rf,select=c(deal_probability)))
train_rf1[is.na(train_rf1)] <- 0
train_rf[is.na(train_rf)] <- 0
test_rf1[is.na(test_rf1)] <- 0
test_rf[is.na(test_rf)] <- 0
rf <- randomForest(train_rf1,label_train,test_rf1,label_test,ntree=50,mtry=7,importance=T,do.trace=T)

#gbm
library(gbm)
gbm <- gbm(label_train ~ .,data=train_rf1,n.trees=500,verbose=T)
pred_gbm <- predict.gbm(gbm,newdata=test_rf1,n.trees=500)
pred_train_gbm <- predict.gbm(gbm,newdata=train_rf1,n.trees=500)
rmse_gbm <- sqrt(mean((test[,17]-pred_gbm)^2))
rmse_gbm_train <- sqrt(mean((train[,17]-pred_train_gbm)^2))
summary(gbm)
#fitted_gbm <- fitted(gbm)

#logistic regression
train_log <- subset(train_rf1,select=-c(has_image))
log_reg <- glm(label_train ~ .,data=train_rf1,control = glm.control(trace=T))
summary(log_reg)
pred_log <- predict(log_reg,newdata = test_rf1,type="response")
rmse_log <- sqrt(mean((test[,17]-pred_log)^2))
pred_log_train <- predict(log_reg,newdata = train_rf1,type="response")
rmse_log_train <- sqrt(mean((train[,17]-pred_log_train)^2))


#ensembling 
#averaging
pred_avg <- (pred+pred_gbm+pred_log)/3
rmse_avg <-  sqrt(mean((test[,17]-pred_avg)^2))
#weighted average
pred_weight <- (pred*0.25+pred_gbm*0.5+pred_log*0.25)/3
rmse_weight <-  sqrt(mean((test[,17]-pred_weight)^2))
#stacking
train_stack <- cbind(pred_train,pred_nn_train$net.result,pred_log_train)
colnames(train_stack) <- c("pred","pred_nn","pred_log")
train_stack_df <- data.frame(train_stack)
test_stack <- cbind(pred,pred_nn$net.result,pred_log)
colnames(test_stack) <- c("pred","pred_nn","pred_log")
test_stack_df <- data.frame(test_stack)
dTrain_stack <- xgb.DMatrix(data=train_stack,label=label,missing=NaN)
stack_xgboost <- xgboost(data=dTrain_stack,params=p,nrounds=p$nrounds,print_every_n = 10)
dTest_stack <- xgb.DMatrix(data=test_stack,label=label_t,missing=NaN)
pred_stack <- predict(stack_xgboost,dTest_stack)
rmse_stack <- sqrt(mean((train[,17]-pred_stack)^2))
log_stack <- glm(label ~ .,data=train_stack_df,control=glm.control(trace=T))
pred_stack_log <- predict(log_stack,test_stack_df)
rmse_stack_log <- sqrt(mean((train[,17]-pred_stack_log)^2))


#neuralnet
library(neuralnet)
n <- names(train_rf1)
f <- as.formula(paste("label_train ~", paste(n[!n %in% "label_rf1"], collapse = " + ")))
nn <- neuralnet(formula=f,data=train_rf1,hidden=15,learningrate = 0.0001,algorithm='backprop',
                err.fct='sse',linear.output = F,stepmax=50)
plot(nn)
pred_nn <- compute(nn,test_rf1)
rmse_nn <- sqrt(mean((test[,17]-pred_nn$net.result)^2))
pred_nn_train <- compute(nn,train_rf1)
rmse_nn_train <- sqrt(mean((train[,17]-pred_nn_train$net.result)^2))
