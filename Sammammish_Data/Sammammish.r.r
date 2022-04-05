library(rpart)
library(dplyr)
library(lubridate)
library(magrittr)

setwd("C:\\Users\\muj_m\\Desktop\\aly_6050")
sam_train <- read.csv("final week\\Sammamish-Training.csv")
sam_test <- read.csv("final week\\Sammamish-Test1.csv")

#preprocessing

#dates
sam_train$List.Date <- mdy(sam_train$List.Date) 
sam_test$List.Date <- dmy(sam_test$List.Date) 

#correcting the names
colnames(sam_test)[1] <- "ML.Number"
colnames(sam_train)[1] <- "ML.Number"


#removing row because it has many empty columns
sam_train <- subset(sam_train, ML.Number!=847824)

#deleting columns as they are mostly characters and most of them have empty rows
s_t1 <- sam_train[, -c(1:2,4:5,7,9:11,14,18,20:23,25:26,28:33,35:37,42:44,46:51,53:62,64,66:72,75:83,86:92,96:103,107,109)]
s_ts1 <- sam_test[, -c(1:2,4:5,7,9:11,14,18,20:23,25:26,28:33,35:37,42:44,46:51,53:62,64,66:72,75:83,86:92,96:103,107,109)]

#Factors
#converting to factor

coli <- c(4:6,10:13,17:18,22:23,25:31)

s_t1 %<>%
  mutate_each_(funs(factor(.)),coli)

s_ts1 %<>%
  mutate_each_(funs(factor(.)),coli)

#filling empty values

#categorical missing values
getmode <- function(v){
  v=v[nchar(as.character(v))>0]
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

# mean for numerical values
#for train
for (cols in colnames(s_t1)) {
  if (cols %in% names(s_t1[,sapply(s_t1, is.numeric)])) {
    s_t1<-s_t1%>%mutate(!!cols := replace(!!rlang::sym(cols), is.na(!!rlang::sym(cols)), mean(!!rlang::sym(cols), na.rm=TRUE)))
  }
  else {
    
    s_t1<-s_t1%>%mutate(!!cols := replace(!!rlang::sym(cols), !!rlang::sym(cols)=="", getmode(!!rlang::sym(cols))))
    
  }
}

#for test
for (cols in colnames(s_ts1)) {
  if (cols %in% names(s_ts1[,sapply(s_ts1, is.numeric)])) {
    s_ts1<-s_ts1%>%mutate(!!cols := replace(!!rlang::sym(cols), is.na(!!rlang::sym(cols)), mean(!!rlang::sym(cols), na.rm=TRUE)))
  }
  else {
    
    s_ts1<-s_ts1%>%mutate(!!cols := replace(!!rlang::sym(cols), !!rlang::sym(cols)=="", getmode(!!rlang::sym(cols))))
    
  }
}

#rounding train
s_t1<- as_tibble(s_t1) %>% mutate_at(vars(DOM,Lot.Size,Lot.Square.Footage), funs(round(., 0)))

#rounding test
s_ts1<- as_tibble(s_ts1) %>% mutate_at(vars(DOM,Lot.Size,Lot.Square.Footage), funs(round(., 0)))

#checking if empty
list_na <- colnames(s_t1)[ apply(s_t1, 2, anyNA) ]
list_na

list_na <- colnames(s_t1)[ apply(s_ts1, 2, anyNA) ]
list_na

#### linear regression #####
library(ggplot2)

#train data (tr)
tr <- s_t1 
#test data (ms)
ms <- s_ts1 

#we find that data is normal by applying log
ggplot(tr, aes(List.Price)) + geom_density(fill="blue")

ggplot(tr, aes(log(List.Price))) + geom_density(fill="blue")


#Converting the variables to char as there are new rows within test and its causing issue with prediction
tr$Style<- is.character(s_t1$Style)
tr$Exterior <- is.character(s_t1$Exterior)

ms$Style<- is.character(s_ts1$Style)
ms$Exterior <- is.character(s_ts1$Exterior)

tr$Energy.Source<- is.character(s_t1$Energy.Source)
ms$Energy.Source <- is.character(s_ts1$Energy.Source)

#linear regression with training model
model1 <- lm(log(List.Price)~.,data=tr)
summary(model1)

#function for getting rmse & r2
eval_results <- function(true, predicted, df) {
  SSE <- sum((predicted - true)^2)
  SST <- sum((true - mean(true))^2)
  R_square <- 1 - SSE / SST
  RMSE = sqrt(SSE/nrow(df))
  
  # Model performance metrics
  data.frame(
    RMSE = RMSE,
    Rsquare = R_square
  )
  
}

#prediction with training model
pred_test0<-predict(model1, newdata=tr)
exp(pred_test0)

#prediction with testing model
pred_test1<-predict(model1, newdata=ms)
exp(pred_test1)


#metrics for train prediction
eval_results(tr$List.Price, exp(pred_test0), tr)

#metrics for test prediction
eval_results(ms$List.Price, exp(pred_test1), ms)

##### random forest #####

library(randomForest)

rf_model = randomForest(log(List.Price) ~ ., data=tr,importance=TRUE,ntree = 50)
summary(rf_model)
rf_model

#training values prediction
pred2 = predict(rf_model,tr)
pred2

#train metrics
eval_results(tr$List.Price, exp(pred2), tr)

###test values prediction

#was getting error here, so found a solution for that error
ms <- rbind(tr[1, ] , ms)
ms <- ms[-1,]

#prediction
pred3 = predict(rf_model,ms)
exp(pred3)


#test metrics for rf
eval_results(ms$List.Price, exp(pred3), ms)

###decision tree ####
library(rpart)
library(rpart.plot)

#decision tree
fitto <- rpart(log(List.Price)~., data=tr)
rpart.plot(fitto)
summary(fitto)

#prediction on train data
modelv<- predict(fitto,tr)
exp(modelv)

#prediction on test data
modelx<- predict(fitto,ms)
exp(modelx)


#rmse & r2 for decision tree train
eval_results(tr$List.Price, exp(modelv), tr)

#rmse & r2 for decision tree test.
eval_results(ms$List.Price, exp(modelx), ms)


##### all metrics for test ####

#metrics for test prediction ( linear reg)
eval_results(ms$List.Price, exp(pred_test1), ms)

#test metrics for rf
eval_results(ms$List.Price, exp(pred3), ms)

#test metrics for dt
eval_results(ms$List.Price, exp(modelx), ms)
