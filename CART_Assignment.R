####Import library
library(MASS)
library(rpart)
library(rpart.plot)
library(rattle)
library(RColorBrewer)
library(ggplot2)
#### Import dataset
Bank_data <- read.csv(file="/Users/rsklanu/Cart_assignmet/PL_XSELL.csv", header=TRUE)
#View(Bank_data)
summary(Bank_data)
sapply(Bank_data,function(x){sum(is.na(x))})
str(Bank_data)
### Divinding data to ›train and test
Bank_data=Bank_data[c(-1,-40)]
set.seed(60)
n=nrow(Bank_data)
trainIndex<-sample(1:n, size = round(0.70*n), replace=FALSE)
traindata<-Bank_data[trainIndex,]
testdata<-Bank_data[-trainIndex,]
#View(traindata)
#View(testdata)
prop.table(table(traindata$TARGET))
prop.table(table(testdata$TARGET))
#Bank_tree=rpart(TARGET~.,data=traindata)
#res<-predict(Bank_tree,newdata = testdata)
#prp(Bank_tree)
#fancyRpartPlot(Bank_tree)


accu=integer();
accut=integer();
minbu=integer();

for (i in seq(1,n,50)){
  Bank_tree=rpart(TARGET~.,data=traindata,method = "class")
  res<-predict(Bank_tree,newdata = traindata,type="class")
  result<-predict(Bank_tree,newdata = testdata,type="class")
  xy=table(traindata$TARGET,res)
  tempo=((xy[[1]]+xy[[4]])/nrow(traindata))
  accut=c(accut,tempo) ##Ac
  xx=table(testdata$TARGET,result)
  temp=((xx[[1]]+xx[[4]])/nrow(testdata))
  accu=c(accu,temp) ##Accuracy
  minbu=c(minbu,i)
}
accut

summary(accu)
summary(accut)
plot(accut~minbu,type="l")
plot(accu~minbu,type="l")

plotdata<-cbind(accu,minbu)
fortify(data.frame(plotdata))
ggplot(as.data.frame(plotdata),aes(x=minbu,y=accu,fill=factor(accu)))+geom_bar(stat="identity",position="dodge")

Bank_tree_final=rpart(TARGET~.,data=traindata,minbucket=1000,method = "class")
prp(Bank_tree_final)

plot(Bank_tree_final,uniform = "true",margin = 0.2)
text(Bank_tree_final,use.n = TRUE, all=TRUE, cex=.5)
fancyRpartPlot(Bank_tree_final)


##### Without date
Bank_data_date <- read.csv(file="/Users/rsklanu/Cart_assignmet/PL_XSELL.csv", header=TRUE)
#View(Bank_data)
summary(Bank_data_date)
sapply(Bank_data_date,function(x){sum(is.na(x))})
str(Bank_data_date)
### Divinding data to train and test
Bank_data_date=Bank_data_date[c(-1,-11,-40)]
set.seed(60)
n=nrow(Bank_data_date)
trainIndex_date<-sample(1:n, size = round(0.70*n), replace=FALSE)
traindata_date<-Bank_data_date[trainIndex_date,]
testdata_date<-Bank_data_date[-trainIndex_date,]
prop.table(table(traindata_date$TARGET))
prop.table(table(testdata_date$TARGET))
accu_d=integer();
accut_d=integer();
minbu_d=integer();

for (i in seq(1,n,50)){
  Bank_data_date=rpart(TARGET~.,data=traindata_date,method = "class")
  res_d<-predict(Bank_data_date,newdata = traindata_date,type="class")
  result_d<-predict(Bank_data_date,newdata = testdata_date,type="class")
  xy_d=table(traindata_date$TARGET,res_d)
  tempo_d=((xy_d[[1]]+xy_d[[4]])/nrow(traindata_date))
  accut_d=c(accut_d,tempo_d) ##Ac
  xx_d=table(testdata_date$TARGET,result_d)
  temp_d=((xx_d[[1]]+xx_d[[4]])/nrow(testdata_date))
  accu_d=c(accu_d,temp_d) ##Accuracy
  minbu_d=c(minbu_d,i)
}
summary(accut_d)
summary(accu_d)
plot(accut_d~minbu_d,type="l")
plot(accu_d~minbu_d,type="l")

plotdata<-cbind(accu,minbu)
fortify(data.frame(plotdata))
ggplot(as.data.frame(plotdata),aes(x=minbu,y=accu,fill=factor(accu)))+geom_bar(stat="identity",position="dodge")

Bank_tree_final_date=rpart(TARGET~.,data=traindata_date,minbucket=1000,method = "class")
prp(Bank_tree_final_date)
fancyRpartPlot(Bank_tree_final_date)

######With Random
Bank_data_random <- read.csv(file="/Users/rsklanu/Cart_assignmet/PL_XSELL.csv", header=TRUE)
#View(Bank_data)
summary(Bank_data_random)
sapply(Bank_data_random,function(x){sum(is.na(x))})
str(Bank_data_random)
### Divinding data to train and test
Bank_data_random=Bank_data_random[c(-1)]
set.seed(60)
n=nrow(Bank_data_random)
trainIndex_random<-sample(1:n, size = round(0.70*n), replace=FALSE)
traindata_random<-Bank_data_random[trainIndex_random,]
testdata_random<-Bank_data_random[-trainIndex_random,]
prop.table(table(traindata_random$TARGET))
prop.table(table(testdata_random$TARGET))
accu_d=integer();
accut_d=integer();
minbu_d=integer();

for (i in seq(1,n,50)){
  Bank_data_random=rpart(TARGET~.,data=traindata_random,method = "class")
  res_d<-predict(Bank_data_random,newdata = traindata_random,type="class")
  result_d<-predict(Bank_data_random,newdata = testdata_random,type="class")
  xy_d=table(traindata_random$TARGET,res_d)
  tempo_d=((xy_d[[1]]+xy_d[[4]])/nrow(traindata_random))
  accut_d=c(accut_d,tempo_d) ##Ac
  xx_d=table(testdata_random$TARGET,result_d)
  temp_d=((xx_d[[1]]+xx_d[[4]])/nrow(testdata_random))
  accu_d=c(accu_d,temp_d) ##Accuracy
  minbu_d=c(minbu_d,i)
}
summary(accut_d)
summary(accu_d)
plot(accut_d~minbu_d,type="l")
plot(accu_d~minbu_d,type="l")

plotdata<-cbind(accu,minbu)
fortify(data.frame(plotdata))
ggplot(as.data.frame(plotdata),aes(x=minbu,y=accu,fill=factor(accu)))+geom_bar(stat="identity",position="dodge")

Bank_tree_final_random=rpart(TARGET~.,data=traindata_random,minbucket=500,method = "class")
prp(Bank_tree_final_random)
fancyRpartPlot(Bank_tree_final_random)


###### Scaling data

Bank_data_scale <- read.csv(file="/Users/rsklanu/Cart_assignmet/PL_XSELL.csv", header=TRUE)
#View(Bank_data)
summary(Bank_data_scale)
sapply(Bank_data_scale,function(x){sum(is.na(x))})
str(Bank_data_scale)
### Divinding data to train and test
corp=round(cor(Bank_data_scale[c(2,4,7,8,11,12,13,19,20,21,22,26,27,31,32,33,34,35,36)]),2)
library('corrplot') #package corrplot

corrplot(corp,method = "color", addCoef.col="grey",tl.cex = 0.5,number.cex  = 0.5)
Bank_data_scale=Bank_data_scale[c(-1,-40)]
Bank_data_scale=scale(Bank_data_scale)
View(Bank_data_scale)

Bank_data_scale=Bank_data_scale[c(1,2,3,4,5,6,7,8,9,10,11,12,13,19,20,21,22,26,27,31,32,33,34,35,36)]
scaled=scale(Bank_data_scale[c(-1,-3,-5,-6,-9,-10)])
View(scaled)
str(scaled)
scaled=cbind(scaled,Bank_data_scale[c(1,3,5,6,10)])
#scaled=scaled[-25]
set.seed(60)
n=nrow(scaled)
trainIndex_scale<-sample(1:n, size = round(0.70*n), replace=FALSE)
traindata_scale<-scaled[trainIndex_scale,]
testdata_scale<-scaled[-trainIndex_scale,]
prop.table(table(traindata_scale$TARGET))
prop.table(table(testdata_scale$TARGET))
accu_d=integer();
accut_d=integer();
minbu_d=integer();

for (i in seq(1,n,50)){
  scaled=rpart(TARGET~.,data=traindata_scale,method = "class")
  res_d<-predict(scaled,newdata = traindata_scale,type="class")
  result_d<-predict(scaled,newdata = testdata_scale,type="class")
  xy_d=table(traindata_scale$TARGET,res_d)
  tempo_d=((xy_d[[1]]+xy_d[[4]])/nrow(traindata_scale))
  accut_d=c(accut_d,tempo_d) ##Ac
  xx_d=table(testdata_scale$TARGET,result_d)
  temp_d=((xx_d[[1]]+xx_d[[4]])/nrow(testdata_scale))
  accu_d=c(accu_d,temp_d) ##Accuracy
  minbu_d=c(minbu_d,i)
}
summary(accut_d)
summary(accu_d)
plot(accut_d~minbu_d,type="l")
plot(accu_d~minbu_d,type="l")

plotdata<-cbind(accu,minbu)
fortify(data.frame(plotdata))
ggplot(as.data.frame(plotdata),aes(x=minbu,y=accu,fill=factor(accu)))+geom_bar(stat="identity",position="dodge")

Bank_tree_final_scale=rpart(TARGET~.,data=traindata_scale,minbucket=100,method = "class")
prp(Bank_tree_final_scale)
fancyRpartPlot(Bank_tree_final_scale)
####

###Random forest

library(randomForest)

Bank_data_forest <- read.csv(file="/Users/rsklanu/Cart_assignmet/PL_XSELL.csv", header=TRUE)
Bank_data_forest=Bank_data_forest[c(-1,-40)]
Bank_data_forest=Bank_data_forest[c(1,2,3,4,5,6,7,8,9,10,11,12,13,19,20,21,22,26,27,31,32,33,34,35,36)]
View(scaled_forest)
summary(scaled_forest)
class(scaled_forest$ACC_OP_DATE)
scaled_forest=scale(Bank_data_forest[c(-1,-3,-5,-6,-9,-10)])
scaled_forest=cbind(scaled_forest,Bank_data_forest[c(1,3,5,6,10)])
library(lubridate)
scaled_forest$ACC_OP_DATE = parse_date_time(x = scaled_forest$ACC_OP_DATE,orders = c("d m y", "d B Y", "m/d/y"))
rf=randomForest(TARGET~.,ntree=1000,mtry=5,data = scaled_forest)
pred=predict(rf)
pred[pred>=0.5]=1
pred[pred<0.5]=0
t=table(scaled_forest$TARGET,pred)
ac=(t[[1]]+t[[4]])/nrow(scaled_forest)
varImpPlot(rf)

######

##### date removed
Bank_data_d <- read.csv(file="/Users/rsklanu/Cart_assignmet/PL_XSELL.csv", header=TRUE)
View(Bank_data_d)
summary(Bank_data_d)
sapply(Bank_data_d,function(x){sum(is.na(x))})
str(Bank_data_d)
### Divinding data to train and test
Bank_data_d=Bank_data_d[c(-1,-40)]
set.seed(60)
n=nrow(Bank_data_d)
trainIndex_d<-sample(1:n, size = round(0.70*n), replace=FALSE)
traindata_d<-Bank_data_d[trainIndex_d,]
testdata_d<-Bank_data_d[-trainIndex_d,]
prop.table(table(traindata_d$TARGET))
prop.table(table(testdata_d$TARGET))
accu_d=integer();
accut_d=integer();
minbu_d=integer();

for (i in seq(1,n,50)){
  Bank_data_d=rpart(TARGET ~ ., data = traindata_d,control=rpart.control(minbucket =10,minsplit = 10,xval = 10, cp=0.0028),method = "class")
  res_d<-predict(Bank_data_d,newdata = traindata_d,type="class")
  result_d<-predict(Bank_data_d,newdata = testdata_d,type="class")
  xy_d=table(traindata_d$TARGET,res_d)
  tempo_d=((xy_d[[1]]+xy_d[[4]])/nrow(traindata_d))
  accut_d=c(accut_d,tempo_d) ##Ac
  xx_d=table(testdata_d$TARGET,result_d)
  temp_d=((xx_d[[1]]+xx_d[[4]])/nrow(testdata_d))
  accu_d=c(accu_d,temp_d) ##Accuracy
  minbu_d=c(minbu_d,i)
}
summary(accut_d)
summary(accu_d)
plot(accut_d~minbu_d,type="l")
plot(accu_d~minbu_d,type="l")

plotdata<-cbind(accu,minbu)

Bank_tree_final_d=rpart(TARGET ~ ., data = traindata_d,control=rpart.control(minbucket =10,minsplit = 10,xval = 10, cp=0.0028),method = "class")
prp(Bank_tree_final_d)
fancyRpartPlot(Bank_tree_final_d)
##### date transformed to date
Bank_data_dt <- read.csv(file="/Users/rsklanu/Cart_assignmet/PL_XSELL.csv", header=TRUE)
View(Bank_data_dt)
summary(Bank_data_dt)
sapply(Bank_data_dt,function(x){sum(is.na(x))})
str(Bank_data_dt)
### Divinding data to train and test
Bank_data_dt=Bank_data_dt[c(-1,-40)]
Bank_data_dt$ACC_OP_DATE = parse_date_time(x = Bank_data_dt$ACC_OP_DATE,orders = c("d m y", "d B Y", "m/d/y"))
set.seed(60)
n=nrow(Bank_data_dt)
trainIndex_d<-sample(1:n, size = round(0.70*n), replace=FALSE)
traindata_dt<-Bank_data_dt[trainIndex_d,]
testdata_dt<-Bank_data_dt[-trainIndex_d,]
prop.table(table(traindata_dt$TARGET))
prop.table(table(testdata_dt$TARGET))
accu_d=integer();
accut_d=integer();
minbu_d=integer();

for (i in seq(1,n,50)){
  Bank_data_dt=rpart(TARGET ~ ., data = traindata_dt,control=rpart.control(minbucket =10,minsplit = 10,xval = 10, cp=0.0028),method = "class")
  res_d<-predict(Bank_data_dt,newdata = traindata_dt,type="class")
  result_d<-predict(Bank_data_dt,newdata = testdata_dt,type="class")
  xy_d=table(traindata_dt$TARGET,res_d)
  tempo_d=((xy_d[[1]]+xy_d[[4]])/nrow(traindata_dt))
  accut_d=c(accut_d,tempo_d) ##Ac
  xx_d=table(testdata_dt$TARGET,result_d)
  temp_d=((xx_d[[1]]+xx_d[[4]])/nrow(testdata_dt))
  accu_d=c(accu_d,temp_d) ##Accuracy
  minbu_d=c(minbu_d,i)
}
summary(accut_d)
summary(accu_d)
plot(accut_d~minbu_d,type="l")
plot(accu_d~minbu_d,type="l")

plotdata<-cbind(accu,minbu)

Bank_tree_final_dt=rpart(TARGET ~ ., data = traindata_dt,control=rpart.control(minbucket =10,minsplit = 10,xval = 10, cp=0.0028),method = "class")
prp(Bank_tree_final_dt)
fancyRpartPlot(Bank_tree_final_dt)
