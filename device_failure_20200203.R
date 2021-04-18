##running required libraries

library(sqldf)
library(ROSE)
library(caret)
library(rpart)
library(rpart.plot)
library(ROCR)
library(randomForest)
library(ggplot2)
library(ggthemes)
library(ggthemr)
library(scales)
library(broom)
library(dplyr)
library(tidyverse)
library(lubridate)
library(cowplot)
library(corrplot)
library(caretEnsemble)
library(h2o)







###ingestion and cleaning


mydata=read.csv("/****/predictive_maintenance.csv")
str(mydata)
summary(mydata)


######CORRELATION ANALYSIS
#REMOVING DATE, ID





mydata_num<-sqldf("select metric1,metric2,metric3,metric4,metric5,metric6,metric7,metric8,metric9 from mydata")
findCorrelation( cor(mydata_num), cutoff = .5, names = TRUE )
res <- cor(mydata_num)
round(res, 2)
corrplot(res, type = "upper", order = "hclust", tl.col = "black", tl.srt = 45)

#variable exploration and modification

#extracting month ~ estimating risk

month_risk<-sqldf("select month, (sum_f*1.0*100/cnt) month_risk from (select strftime('%m',date) as month,count(*) cnt, sum(failure) as sum_f from mydata group by 1 order by 3 desc)a order by 2 desc")

drisk=sqldf("select month, (sum_f*1.0*100/cnt) month_risk from (select strftime('%m',date) as month,count(*) cnt, sum(failure) as sum_f from mydata group by 1 order by 3 desc)a order by 2 desc")
drisk1=sqldf("select a.*, strftime('%m',a.date) as month from mydata a")
mydata1=sqldf("select a.* , b.month_risk from drisk1 a inner join drisk b on a.month=b.month")
mydata=mydata1


#extracting day ~ estimating risk

drisk=sqldf("select day, (sum_f*1.0*100/cnt) day_risk from (select strftime('%d',date) as day,count(*) cnt, sum(failure) as sum_f from mydata group by 1 order by 3 desc)a order by 2 desc")
drisk1=sqldf("select a.*, strftime('%d',a.date) as day from mydata a")
mydata1=sqldf("select a.* , b.day_risk from drisk1 a inner join drisk b on a.day=b.day")
mydata=mydata1

#removing day & month
mydata=subset(mydata,select=-c(day,month))


#adding flags for device id's 

mydata$id_type<-substr(mydata$id,1,3)
mydata$id_S1F<-as.numeric(mydata$id_type=="S1F")
mydata$id_W1F<-as.numeric(mydata$id_type=="W1F")
mydata$id_Z1F<-as.numeric(mydata$id_type=="Z1F")



#mydata$id_type<-substr(mydata$id,1,3)
#mydata$id_S1F<-as.numeric(mydata$id_type=="S1F")
#mydata$id_W1F<-as.numeric(mydata$id_type=="W1F")
#mydata$id_Z1F<-as.numeric(mydata$id_type=="Z1F")


#analyzing variables - histograms

#metric 2
a=sqldf("select metric2 from mydata where metric2>0")
hist(a$metric2, freq=FALSE, breaks=30,col="blue", xlab="Metric2", main="Metric2 histogram")
curve(dnorm(x, mean=mean(a$metric2), sd=sd(a$metric2)), add=TRUE, col="red") #line
mydata$metric2_dummy<-as.numeric(mydata$metric2>0)



#metric 3
a=sqldf("select metric3 from mydata where metric3>0")
hist(a$metric3, freq=FALSE, breaks=30,col="blue", xlab="metric3", main="Metric3 histogram")
curve(dnorm(x, mean=mean(a$metric3), sd=sd(a$metric3)), add=TRUE, col="red") #line
mydata$metric3_dummy<-as.numeric(mydata$metric3>0)



#metric 4
a=sqldf("select metric4 from mydata where metric4>0")
hist(a$metric4, freq=FALSE, breaks=30,col="blue", xlab="metric4", main="Metric4 histogram")
curve(dnorm(x, mean=mean(a$metric4), sd=sd(a$metric4)), add=TRUE, col="red") #line
mydata$metric4_dummy<-as.numeric(mydata$metric4>0)


#analyzing - failure~metric2
mytable=table(mydata$failure, mydata$metric2_dummy)
prop.table(mytable,2)


#analyzing - failure~metric3
mytable=table(mydata$failure, mydata$metric3_dummy)
prop.table(mytable,1)


hist(mydata$metric5, freq=FALSE, breaks=30,col="blue", xlab="metric5", main="Metric5 histogram")
curve(dnorm(x, mean=mean(mydata$metric5), sd=sd(mydata$metric5)), add=TRUE, col="red") #line

hist(mydata$metric6, freq=FALSE, breaks=30,col="blue", xlab="metric6", main="Metric6 histogram")
curve(dnorm(x, mean=mean(mydata$metric6), sd=sd(mydata$metric6)), add=TRUE, col="red") #line




#Creating Ratio Variables
mydata$att6_att5=as.numeric(mydata$metric6/mydata$metric5)


#metric7
a=sqldf("select metric7 from mydata where metric7>0")
hist(a$metric7, freq=FALSE, breaks=30,col="blue", xlab="metric7", main="Metric7 histogram")
curve(dnorm(x, mean=mean(a$metric7), sd=sd(a$metric7)), add=TRUE, col="red") #line

mydata$metric7_dummy<-as.numeric(mydata$metric7>0)

mytable=table(mydata$failure, mydata$metric7_dummy)
prop.table(mytable,1)
prop.table(mytable,2)

#metric8
##removing
mydata=subset(mydata,select=-c(metric8))



#metric9
a=sqldf("select metric9 from mydata where metric9>0")
hist(a$metric9, freq=FALSE, breaks=30,col="blue", xlab="metric9", main="Metric9 histogram")
curve(dnorm(x, mean=mean(a$metric9), sd=sd(a$metric9)), add=TRUE, col="red") #line

mydata$metric9_dummy<-as.numeric(mydata$metric9>0)


mytable=table(mydata$failure, mydata$metric9_dummy)
prop.table(mytable,1)
prop.table(mytable,2)



#Converting to right data types
mydata$date=as.Date(mydata$date)
mydata$metric1=as.numeric(mydata$metric1)
mydata$metric2=as.numeric(mydata$metric2)
mydata$metric4=as.numeric(mydata$metric4)
mydata$metric5=as.numeric(mydata$metric5)
mydata$metric6=as.numeric(mydata$metric6)
mydata$metric7=as.numeric(mydata$metric7)
mydata$metric9=as.numeric(mydata$metric9)
mydata$day_risk=as.numeric(mydata$day_risk)
mydata$month_risk=as.numeric(mydata$month_risk)
mydata$id_type=as.factor(mydata$id_type)
mydata$id_S1F=as.factor(mydata$id_S1F)
mydata$id_W1F=as.factor(mydata$id_W1F)
mydata$id_Z1F=as.factor(mydata$id_Z1F)
mydata$metric2_dummy=as.factor(mydata$metric2_dummy)
mydata$metric3_dummy=as.factor(mydata$metric3_dummy)
mydata$metric4_dummy=as.factor(mydata$metric4_dummy)
mydata$metric7_dummy=as.factor(mydata$metric7_dummy)
mydata$metric9_dummy=as.factor(mydata$metric9_dummy)
mydata$failure=as.factor(mydata$failure)




####removing unneccassary rows
max_date=sqldf("select metric1,max(date) max_date from mydata group by 1")
join_state=sqldf("select a.* from mydata a inner join max_date b on a.metric1=b.metric1 and a.date=b.max_date")
mydata_final=sqldf("select a.* from mydata a inner join max_date b on a.metric1=b.metric1 and a.date=b.max_date")



mydata_final=sqldf("select id,date,failure,
metric1, metric2,metric3,metric4,metric5, metric6, metric7,metric9, month_risk,
day_risk, id_type, id_S1F, id_W1F, id_Z1F, metric2_dummy, metric3_dummy,
metric4_dummy, att6_att5, metric7_dummy, metric9_dummy from mydata_final")


#Removing index value = 0 
mydata_final=sqldf("select * from mydata_final where metric1>0")



# scaling the continous variables between 0-1.

mydata_final$metric1_s=((mydata_final$metric1-min(mydata_final$metric1))/(max(mydata_final$metric1)-min(mydata_final$metric1)))
mydata_final$metric2_s=((mydata_final$metric2-min(mydata_final$metric2))/(max(mydata_final$metric2)-min(mydata_final$metric2)))
mydata_final$metric3_s=((mydata_final$metric3-min(mydata_final$metric3))/(max(mydata_final$metric3)-min(mydata_final$metric3)))
mydata_final$metric4_s=((mydata_final$metric4-min(mydata_final$metric4))/(max(mydata_final$metric4)-min(mydata_final$metric4)))
mydata_final$metric5_s=((mydata_final$metric5-min(mydata_final$metric5))/(max(mydata_final$metric5)-min(mydata_final$metric5)))
mydata_final$metric6_s=((mydata_final$metric6-min(mydata_final$metric6))/(max(mydata_final$metric6)-min(mydata_final$metric6)))
mydata_final$metric7_s=((mydata_final$metric7-min(mydata_final$metric7))/(max(mydata_final$metric7)-min(mydata_final$metric7)))
mydata_final$metric9_s=((mydata_final$metric9-min(mydata_final$metric9))/(max(mydata_final$metric9)-min(mydata_final$metric9)))
mydata_final$att6_att5_s=((mydata_final$att6_att5 -min(mydata_final$att6_att5 ))/(max(mydata_final$att6_att5 )-min(mydata_final$att6_att5 )))





aa=sqldf("select id, 
      min(metric1_s) min_metric1_s,max(metric1_s) max_metric1_s,median(metric1_s) med_metric1_s,avg(metric1_s) avg_metric_2_s, case when max(metric1_s)>0 then min(metric1_s)/max(metric1_s) else 0 end as ratio_metric1_s ,
      min(metric2_s) min_metric2_s,max(metric2_s) max_metric2_s,median(metric2_s) med_metric2_s,avg(metric2_s) avg_metric_2_s, case when max(metric2_s)>0 then min(metric2_s)/max(metric2_s) else 0 end as ratio_metric2_s ,
      min(metric3_s) min_metric3_s,max(metric3_s) max_metric3_s,median(metric3_s) med_metric3_s,avg(metric3_s) avg_metric_2_s, case when max(metric3_s)>0 then min(metric3_s)/max(metric3_s) else 0 end as ratio_metric3_s ,
      min(metric4_s) min_metric4_s,max(metric4_s) max_metric4_s,median(metric4_s) med_metric4_s,avg(metric4_s) avg_metric_2_s, case when max(metric4_s)>0 then min(metric4_s)/max(metric4_s) else 0 end as ratio_metric4_s ,
      min(metric5_s) min_metric5_s,max(metric5_s) max_metric5_s,median(metric5_s) med_metric5_s,avg(metric5_s) avg_metric_2_s, case when max(metric5_s)>0 then min(metric5_s)/max(metric5_s) else 0 end as ratio_metric5_s ,
      min(metric6_s) min_metric6_s,max(metric6_s) max_metric6_s,median(metric6_s) med_metric6_s,avg(metric6_s) avg_metric_2_s, case when max(metric6_s)>0 then min(metric6_s)/max(metric6_s) else 0 end as ratio_metric6_s ,
      min(metric7_s) min_metric7_s,max(metric7_s) max_metric7_s,median(metric7_s) med_metric7_s,avg(metric7_s) avg_metric_2_s, case when max(metric7_s)>0 then min(metric7_s)/max(metric7_s) else 0 end as ratio_metric7_s ,
      min(metric9_s) min_metric9_s,max(metric9_s) max_metric9_s,median(metric9_s) med_metric9_s,avg(metric9_s) avg_metric_2_s, case when max(metric9_s)>0 then min(metric9_s)/max(metric9_s) else 0 end as ratio_metric9_s ,
      min(month_risk) min_month_risk,max(month_risk) max_month_risk,median(month_risk) med_month_risk,avg(month_risk) avg_metric_2_s, case when max(month_risk)>0 then min(month_risk)/max(month_risk) else 0 end as ratio_month_risk ,
      min(day_risk) min_day_risk,max(day_risk) max_day_risk,median(day_risk) med_day_risk,avg(day_risk) avg_metric_2_s, case when max(day_risk)>0 then min(day_risk)/max(day_risk) else 0 end as ratio_day_risk ,
      min(att6_att5_s) min_att6_att5_s,max(att6_att5_s) max_att6_att5_s,median(att6_att5_s) med_att6_att5_s,avg(att6_att5_s) avg_metric_2_s, case when max(att6_att5_s)>0 then min(att6_att5_s)/max(att6_att5_s) else 0 end as ratio_att6_att5_s ,
      (max(date)-min(date)) as tenure, 
      max(failure) as failure,
      max(id_S1F) id_S1F, max(id_W1F) id_W1F, max(id_Z1F) id_Z1F, max(metric2_dummy) metric2_dummy, 
      max(metric3_dummy) metric3_dummy , max(metric4_dummy) metric4_dummy, 
      max(metric7_dummy) metric7_dummy, max(metric9_dummy) metric9_dummy
      from mydata_final group by 1")




aa$id_S1F=as.factor(aa$id_S1F)
aa$id_W1F=as.factor(aa$id_W1F)
aa$id_Z1F=as.factor(aa$id_Z1F)
aa$metric2_dummy=as.factor(aa$metric2_dummy)
aa$metric3_dummy=as.factor(aa$metric3_dummy)
aa$metric4_dummy=as.factor(aa$metric4_dummy)
aa$metric7_dummy=as.factor(aa$metric7_dummy)
aa$metric9_dummy=as.factor(aa$metric9_dummy)




#Caret
#splitting the data into train and test

aa1=subset(aa,select=-c(id))

index <- createDataPartition(aa1$failure, p=0.9, list=FALSE)
trainSet <- aa1[ index,]
testSet <- aa1[-index,]
str(trainSet)
table(aa1$failure)



######################
#variable Selection

#Feature selection using rfe in caret




control <- rfeControl(functions = rfFuncs,
                      method = "repeatedcv",
                      repeats = 5,
                      verbose = FALSE)
outcomeName<-'failure'
predictors<-names(trainSet)[!names(trainSet) %in% outcomeName]
failure_Pred_Profile <- rfe(trainSet[,predictors], trainSet[,outcomeName],rfeControl = control)
predictors(failure_Pred_Profile)


failure_Pred_Profile
pred=predictors(failure_Pred_Profile)

plot(failure_Pred_Profile,type=c("g","o"))

model_rf<-train(trainSet[,pred],trainSet[,outcomeName],method='rf')
importance<-varImp(model_rf, scale=FALSE)
plot(importance)







fitControl <- trainControl(
  method = "repeatedcv",
  number = 5,
  repeats = 5)

#using grid search

#Creating grid
grid <- expand.grid(n.trees=c(10,20,50,100,500,1000),shrinkage=c(0.01,0.05,0.1,0.5),n.minobsinnode = c(3,5,10),interaction.depth=c(1,5,10))
# training the model
model_gbm<-train(trainSet[,pred],trainSet[,outcomeName],method='gbm',trControl=fitControl,tuneGrid=grid,verbose=FALSE)

plot(model_gbm)

print(model_gbm)


#using tune length
model_gbm<-train(trainSet[,pred],trainSet[,outcomeName],method='gbm',trControl=fitControl,tuneLength=10, verbose=FALSE)
print(model_gbm)

plot(model_gbm)
#varImp(model_gbm)



#Predictions
predictions<-predict.train(object=model_gbm,testSet[,pred],type="raw")
table(predictions)

confusionMatrix(predictions,testSet[,outcomeName])




model_rf<-train(trainSet[,pred],trainSet[,outcomeName],method='rf')


# Create model with default paramters
control <- trainControl(method="repeatedcv", number=10, repeats=3)
seed <- 7
metric <- "Accuracy"
set.seed(seed)
mtry <- sqrt(length(pred))
tunegrid <- expand.grid(.mtry=mtry)
model_rf <- train(trainSet[,pred],trainSet[,outcomeName],method='rf', metric=metric, tuneGrid=tunegrid, trControl=control)
print(model_rf)


# Random Search
control <- trainControl(method="repeatedcv", number=10, repeats=3, search="random")
set.seed(seed)
mtry <- sqrt(length(pred))
rf_random <-train(trainSet[,pred],trainSet[,outcomeName],method='rf', metric=metric, tuneLength=15, trControl=control)
print(rf_random)
plot(rf_random)


#Grid Search
control <- trainControl(method="repeatedcv", number=10, repeats=3, search="grid")
set.seed(seed)
tunegrid <- expand.grid(.mtry=c(1:15))
rf_gridsearch <- train(trainSet[,pred],trainSet[,outcomeName],method='rf', metric=metric, tuneGrid=tunegrid, trControl=control)
print(rf_gridsearch)
plot(rf_gridsearch)


#tune manually

# Manual Search
control <- trainControl(method="repeatedcv", number=10, repeats=3, search="grid")
tunegrid <- expand.grid(.mtry=c(sqrt(length(pred))))
modellist <- list()
for (ntree in c(1000, 1500, 2000, 2500)) {
  set.seed(seed)
  fit <- train(trainSet[,pred],trainSet[,outcomeName],method='rf', metric=metric, tuneGrid=tunegrid, trControl=control, ntree=ntree)
  key <- toString(ntree)
  modellist[[key]] <- fit
}
# compare results
results <- resamples(modellist)
summary(results)
dotplot(results)

modellist[[4]]



#Predictions
predictions<-predict.train(object=rf_random,testSet[,pred],type="raw")
table(predictions)
confusionMatrix(predictions,testSet[,outcomeName])




control <- trainControl(method="repeatedcv", number=10, repeats=3)
modelada_1 <- train(trainSet[,pred],trainSet[,outcomeName],method='adaboost',trControl=control)

print(modelada_1)
varImp(modelada_1, scale=FALSE)
plot(varImp(modelada_1, scale=FALSE))


#Predictions
predictions<-predict.train(object=modelada_1,testSet[,pred],type="raw")
table(predictions)
confusionMatrix(predictions,testSet[,outcomeName])




h2o.init()

# Identify predictors and response
y <- "failure"
x <- setdiff(names(aa), y)


aa$failure <- as.factor(aa$failure)  
#mydata_final$ID  <- 1:nrow(mydata_final)  
aa.hex  <- as.h2o(aa)


#Split data into Train/Validation/Test Sets
split_h2o <- h2o.splitFrame(aa.hex, c(0.8, 0.1), seed = 1234 )
train_conv_h2o <- h2o.assign(split_h2o[[1]], "train" ) # 80%
valid_conv_h2o <- h2o.assign(split_h2o[[2]], "valid" ) # 10%
test_conv_h2o  <- h2o.assign(split_h2o[[3]], "test" )  # 10%
#Model

# Set names for h2o
target <- "failure"
predictors <- setdiff(names(train_conv_h2o), target)



# Run the automated machine learning 
automl_h2o_models <- h2o.automl(x = pred, y = target,
                                training_frame=train_conv_h2o, 
                                leaderboard_frame = valid_conv_h2o,
                                max_models=5, seed=1)

automl_h2o_models
automl_h2o_models@leader


# Predict on hold-out test set
pred_conversion <- h2o.predict(object = automl_h2o_models@leader, newdata = test_conv_h2o)
#Confusion matrix on test data set
h2o.table(pred_conversion$predict, test_conv_h2o$converted)

automl_h2o_models@leader


h2o.varimp_plot(automl_h2o_models@leader, num_of_features = NULL)


h2o.confusionMatrix(automl_h2o_models@leader, test_conv_h2o)


control <- trainControl(method="repeatedcv", number=10, repeats=3)
set.seed(7)
model_rf_1 <- train(trainSet[,pred],trainSet[,outcomeName],method='rf', trControl=control)
modelGbm_1 <- train(trainSet[,pred],trainSet[,outcomeName],method='gbm',trControl=control, verbose=FALSE)
modelglm_1 <- train(trainSet[,pred],trainSet[,outcomeName],method='glm',trControl=control)
modelada_1 <- train(trainSet[,pred],trainSet[,outcomeName],method='adaboost',trControl=control)


# collect resamples
results <- resamples(list(RF=model_rf_1, GBM=modelGbm_1, GLM=modelglm_1, ADA=modelada_1))
# summarize the distributions
summary(results)
# boxplots of results
bwplot(results)


