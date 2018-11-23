library(MASS)
library(rpart)
library(rpart.plot)
library(rattle)
library(RColorBrewer)
View(Boston)
summary(Boston)
#b=Boston
#write.csv(b,file = "/Users/rsklanu/Pythonday1/Boston.csv")
luxh<-ifelse(Boston$medv>=mean(Boston$medv),1,0)
head(Boston)
Boston<-cbind(Boston[,c(-14)],luxh)
tree=rpart(luxh~.,data=Boston)
window()
prp(tree)
result<-predict(tree)
r<-ifelse(result>0.5,1,0)
table(Boston$luxh,ifelse(result>0.5,1,0))
mean(Boston$luxh==r) ##Accuracy
fancyRpartPlot(tree)

tree2=rpart(luxh~.,data=Boston,method = "class")
prp(tree2)
####iris
View(iris)
summary(iris)
set.seed(1)
n=nrow(iris)
trainIndex<-sample(1:n, size = round(0.85*n), replace=FALSE)
traindata<-iris[trainIndex,]
testdata<-iris[-trainIndex,]
iris_tree=rpart(Species~.,data=traindata)
res<-predict(iris_tree,newdata = testdata)
prp(iris_tree)
pred=ifelse(res[,1]>=0.33,"setosa",ifelse(res[,2]>0.33,"versicolor","virginica"))

mat=table(testdata$Species,pred)



##########Regression tree

summary(Boston)
head(Boston)
# set.seed(1)
# n=nrow(Boston)
# testIn<-sample(1:n,size=round(0.85*n),replace = FALSE)
# traindat=Boston[testIn,]
# testdat=Boston[-testIn,]
# modeltree=rpart(medv~.,data=traindat)
# window()
# prp(modeltree)
# result<-predict(modeltree,newdata = testdat)
# fancyRpartPlot(tree)
# acc=mean((testdat$medv-result)^2)
# sqrt(acc)

modeltree=rpart(medv~.,data=Boston)
window()
prp(modeltree)
result<-predict(modeltree,newdata = Boston)
fancyRpartPlot(tree)
acc=sqrt(mean((Boston$medv-result)^2))
summary(modeltree)
n=nrow(Boston)
mape=mean(abs(Boston$medv-result)/Boston$medv)

#####iteration for min bucket
summary(Boston)
head(Boston)
luxh<-ifelse(Boston$medv>=mean(Boston$medv),1,0)
Boston<-cbind(Boston[,c(-14)],luxh)
set.seed(1)
n=nrow(Boston)
testIn<-sample(1:n,size=round(0.85*n),replace = FALSE)
traindat=Boston[testIn,]
testdat=Boston[-testIn,]
i=as.integer(1)
col=nrow(Boston)
result=data.frame();
accu=integer();
accut=integer();
minbu=integer();
for (i in 1:50){
  mtree=rpart(luxh~.,data=traindat,minbucket=i,method = "class")
  res<-predict(mtree,newdata = traindat,type = "class")
  xy=table(traindat$luxh,res)
  tempo=((xy[[1]]+x[[4]])/nrow(traindat))
  accut=c(accut,tempo) ##Ac
  
  result<-predict(mtree,newdata = testdat,type = "class")
  x=table(testdat$luxh,result)
  temp=((x[[1]]+x[[4]])/nrow(testdat))
  accu=c(accu,temp) ##Accuracy
  minbu=c(minbu,i)
}
accut

plot(accut~minbu,type="l")
plot(accu~minbu,type="l")

#####CP

library(caret)
library(e1071)
head(Boston)

Boston$luxh=as.factor(Boston$luxh)
numfold=trainControl(method="cv",number=10)
cpgr=expand.grid(.cp=seq(0.01,0.7,0.01))
train(luxh~.,data = Boston,method="rpart",trControl=numfold,tuneGrid=cpgr)

tree_final=rpart(luxh~.,data = Boston,cp=0.04,method = "class")
pred=predict(tree_final,Boston)
pred=ifelse(pred[,2]>=0.5,1,0)
y=table(Boston$luxh,pred)
Accuracy=(y[[1]]+y[[4]])/nrow(Boston)

########Random forest
library(randomForest)
rf=randomForest(luxh~.,ntree=1000,mtry=5,data = Boston)
pred=predict(rf)
t=table(Boston$luxh,pred)
ac=(t[[1]]+t[[4]])/nrow(Boston)
varImp(rf)
varImpPlot(rf)
