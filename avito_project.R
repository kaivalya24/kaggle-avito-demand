setwd("C:\\Users\\kaiva\\OneDrive\\Desktop\\Avito\\Data")
library(dplyr)
#library(ngram)
library(tm)
library(openxlsx)
library(stringi)
library(caret)
train_data <- read.csv("train.csv")
test_data <- read.csv("test.csv")
summary(train_data)
#train_active <- read.csv("train_active.csv")
train_data$title_length <- nchar(as.character(train_data$title))
test_data$title_length <- nchar(as.character(test_data$title))
train_data$description_length <- nchar(as.character(train_data$description))
test_data$description_length <- nchar(as.character(test_data$description))
#title_vsource <- VectorSource(train_data$title)
#title_corpus <- VCorpus(train_data$title,language="ru")
image_data_1<- read.xlsx('image_output1.xlsx',sheet=1)
image_data_2<- read.xlsx('image_output1.xlsx',sheet=2)
image_final <- rbind(image_data_1,image_data_2)
image_final<- image_final[,-1]
image_test <- read.xlsx('image_output_test.xlsx',sheet=1)
image_test<- image_test[,-1]
jpg<-stri_locate_first_regex(image_final$ID,"\\.")-1
image_final$image_ID <- substring(image_final$ID,1,jpg[,1])
jpg1<-stri_locate_first_regex(image_test$ID,"\\.")-1
image_test$image_ID <- substring(image_test$ID,1,jpg1[,1])

comma<-stri_locate_first_regex(image_final$size,"\\,")-1
image_final$size_x <- as.numeric(substring(image_final$size,2,comma[,1]))
comma1<-stri_locate_first_regex(image_test$size,"\\,")-1
image_test$size_x <- as.numeric(substring(image_test$size,2,comma1[,1]))
#summary(image_final)
bracket <- stri_locate_first_regex(image_final$size,"\\)")-1
image_final$size_y <- as.numeric(substring(image_final$size,comma[,1]+2,bracket[,1]))
bracket1 <- stri_locate_first_regex(image_test$size,"\\)")-1
image_test$size_y <- as.numeric(substring(image_test$size,comma1[,1]+2,bracket1[,1]))
image_final <- image_final[,-2]
image_test <- image_test[,-2]
final_table<-left_join(train_data,image_final,c(image="image_ID"))
final_table_test<-left_join(test_data,image_test,c(image="image_ID"))
final_table$has_image <- ifelse(is.na(final_table$ID)==T,0,1)
final_table_test$has_image <- ifelse(is.na(final_table_test$ID)==T,0,1)
modeling_table <- subset(final_table,select=-c(image,ID))
modeling_table_test <- subset(final_table_test,select=-c(image,ID))
modeling_table_test<-modeling_table_test[,c(colnames(dTrain))]
#modeling_training
set.seed(100)
trainIndex <- createDataPartition(modeling_table$deal_probability, p = .7, 
                                  list = FALSE, 
                                  times = 1)
#train_nm<-modeling_table[trainIndex,]
train <- data.matrix(modeling_table[trainIndex,])
train_subset <- subset(train,select=-c(user_id,item_id,title,description))
#train_nm <- subset(train_nm,select=-c(user_id,item_id,title,description))
#test_nm<-modeling_table[-trainIndex,]
test <- data.matrix(modeling_table[-trainIndex,])
test_subset <- subset(test,select=-c(user_id,item_id,title,description))
#test_nm <- subset(test_nm,select=-c(user_id,item_id,title,description))
library(xgboost)
data<- subset(train,select=-c(deal_probability))
label<-subset(train,select=c(deal_probability))
dTrain <- xgb.DMatrix(data=data,label=label,missing=NaN)
p<- list(objective = "reg:logistic",
         booster = "gbtree",
         eval_metric = "rmse",
         nthread = 8,
         eta = 0.05,
         max_depth = 18,
         min_child_weight = 11,
         gamma = 0,
         subsample = 0.8,
         colsample_bytree = 0.7,
         alpha = 2.25,
         lambda = 0,
         nrounds = 500)
bst<- xgboost(data=dTrain,params=p,nrounds=p$nrounds,print_every_n = 10)
#xgb with cv
bst_cv <- xgb.cv(data=dTrain,params=p,nrounds=p$nrounds,print_every_n = 10,nfold=5,verbose=T,prediction=T)


data_t<- subset(test,select=-c(deal_probability))
label_t=subset(test,select=c(deal_probability))
dTest <- xgb.DMatrix(data=data_t,label=label_t,missing=NaN)
dTest_final <- xgb.DMatrix(data=data_t,label=label_t,missing=NaN)
pred <- predict(bst,dTest)
test_mat<-data.matrix(modeling_table_test)
dTest_final <- xgb.DMatrix(data=test_mat,missing=NaN)
deal_probability <- predict(object=bst,newdata=dTest_final)
pred_train <- predict(bst,dTrain)
pred_cv <- predict(bst_cv,dTrain)
submission <- data.frame(cbind(test_data,deal_probability))
final_output <- submission[,c("item_id","deal_probability")]
#fitted_xgb <- fitted.values(bst)
test1<-data.frame(cbind(pred,test))
test1$variance <- (test1$deal_probability-test1$pred)^2
rmse <- sqrt(mean(test1$variance))
rmse_train <- sqrt(mean((train[,17]-fitted_xgb)^2))
importance_matrix <- xgb.importance(colnames(test[,-17]), model = bst)
# Nice graph
xgb.plot.importance(importance_matrix)
write.csv(final_output,file="kaivalya_submission.csv",row.names=F)
