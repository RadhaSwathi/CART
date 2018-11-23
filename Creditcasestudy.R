library(rpart)
library(rpart.plot)
library(rattle)
library(RColorBrewer)
library(ggplot2)

###Read train
Train_data <- read.csv(file="/Users/rsklanu/CART/Machinelearning/Credit_Risk_Train_data.csv", header=TRUE, sep=",")
View(Train_data)
summary(Train_data)
sapply(Train_data,function(x){sum(is.na(x))})
str(Train_data)

###read test
Test_data <- read.csv(file="/Users/rsklanu/CART/Machinelearning/Credit_Risk_Test_data.csv", header=TRUE, sep=",")
View(Test_data)
summary(Test_data)
sapply(Test_data,function(x){sum(is.na(x))})
str(Test_data)


## combine and replace null
Test_data=Test_data[,c(-1)] #remove extra col
index=nrow(Train_data) #pointer of merge
names(Train_data)#Check if names are same
names(Test_data)
##make names equal
colnames(Train_data)[13]<-colnames(Test_data)[13]
###Merge
Alldata<-rbind(Train_data,Test_data)
summary(Alldata)
sapply(Alldata,function(x){sum(is.na(x))})
##To find propotion
prop.table(table(Alldata$Gender))
prop.table(table(Alldata$Married))
prop.table(table(Alldata$Dependents))
prop.table(table(Alldata$Self_Employed))
prop.table(table(Alldata$Credit_History))
###FILL missng value
Alldata$Gender[Alldata$Gender=='']='Male'
Alldata$Married[Alldata$Married=='']='Yes'
Alldata$Dependents[Alldata$Dependents=='']='0'
Alldata$Self_Employed[Alldata$Self_Employed=='']='No'
#histogram observe sckewness and select mean or  median
hist(Alldata$LoanAmount,breaks = 50)
Alldata$LoanAmount[is.na(Alldata$LoanAmount)==TRUE]= 126

ggplot(Alldata,aes(x=Loan_Amount_Term,y=LoanAmount,fill=factor(Loan_Amount_Term)))+geom_bar(stat="identity",position="dodge")
ggplot(Alldata,aes(x=LoanAmount,y=Loan_Amount_Term,fill=factor(Loan_Amount_Term)))+geom_bar(stat="identity",position="dodge")
Alldata$Loan_Amount_Term[is.na(Alldata$Loan_Amount_Term)==TRUE]=360

###transferrinall na value to test and rest to train use linear regression to
temptestIndex<-which(is.na(Alldata$Credit_History))
temptest<-Alldata[temptestIndex,]
temptrain<-Alldata[-temptestIndex,]
temptrain<-temptrain[c(-1)]
temptest<-temptest[c(-1)]

null=glm(Credit_History~1,data=temptrain)
high=glm(Credit_History~.,data=temptrain)

#train.fitFwd<-step(null, scope=list(lower=null, upper=high), direction="forward")
train.fitFwd=glm(formula = Credit_History ~ Loan_Status + Property_Area + Dependents, data = temptrain, family = "binomial")

temptest$Credit_History<-predict(train.fitFwd,newdata = temptest,type = "response")
###round could be used
temptest$Credit_History[temptest$Credit_History>0.5]=1
temptest$Credit_History[temptest$Credit_History<=0.5]=0
##filling missing value
Alldata$Credit_History[which(is.na(Alldata$Credit_History))]=temptest$Credit_History
summary(Alldata)
########Split Alldata to test and train.
idx=nrow(Alldata)-index
Train_data=head(Alldata,index)
Test_data=tail(Alldata,idx)

####decision tree
#head(traindat)
#mtree=rpart(Loan_Status~.,data=traindat,method = "class")

Train_data<-Train_data[c(-1)]
Test_data<-Test_data[c(-1)]

accu=integer();
accut=integer();
minbu=integer();
for (i in seq(1,index,5)){
  mtree=rpart(Loan_Status~.,data=Train_data,method = "class")
  res<-predict(mtree,newdata = Train_data,type = "class")
  xy=table(Train_data$Loan_Status,res)
  tempo=((xy[[1]]+x[[4]])/nrow(Train_data))
  accut=c(accut,tempo) ##Ac

  result<-predict(mtree,newdata = Test_data,type = "class")
  x=table(Test_data$Loan_Status,result)
  temp=((x[[1]]+x[[4]])/nrow(Test_data))
  accu=c(accu,temp) ##Accuracy
  minbu=c(minbu,i)
}

###All min bucket is same ther for choose any
plotdata<-cbind(accu,minbu)
fortify(data.frame(plotdata))
ggplot(as.data.frame(plotdata),aes(x=minbu,y=accu,fill=factor(accu)))+geom_bar(stat="identity",position="dodge")
mtreef=rpart(Loan_Status~.,data=Train_data,minbucket=11,method = "class")
prp(mtreef)
plot(accut~minbu,type="l")
plot(accu~minbu,type="l")
#Decision seems to be dependeny on only one factor credit_H<1
mtreef=rpart(Loan_Status~.,data=Train_data,minbucket=11,method = "class")
prp(mtreef)

head(Train_data)
Train_data=Train_data[c(-10)]
mtreef=rpart(Loan_Status~.,data=Train_data,minbucket=11,method = "class")
prp(mtreef)

accu=integer();
accut=integer();
minbu=integer();
for (i in seq(1,index,5)){
  mtree=rpart(Loan_Status~.,data=Train_data,method = "class")
  res<-predict(mtree,newdata = Train_data,type = "class")
  xy=table(Train_data$Loan_Status,res)
  tempo=((xy[[1]]+xy[[4]])/nrow(Train_data))
  accut=c(accut,tempo) ##Ac
  
  result<-predict(mtree,newdata = Test_data,type = "class")
  x=table(Test_data$Loan_Status,result)
  temp=((x[[1]]+x[[4]])/nrow(Test_data))
  accu=c(accu,temp) ##Accuracy
  minbu=c(minbu,i)
}

summary(accu)



plotdata<-cbind(accu,minbu)
fortify(data.frame(plotdata))
ggplot(as.data.frame(plotdata),aes(x=minbu,y=accu,fill=factor(accu)))+geom_bar(stat="identity",position="dodge")
mtreef=rpart(Loan_Status~.,data=Train_data,minbucket=11,method = "class")
prp(mtreef)
plot(accut~minbu,type="l")
plot(accu~minbu,type="l")


#######baseline propotion
prop.table(table(Alldata$Loan_Status))
prop.table(table(Train_data$Loan_Status))
prop.table(table(Test_data$Loan_Status))


