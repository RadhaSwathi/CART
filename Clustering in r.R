library("MASS")
View(iristemp)
iristemp=iris[c(-5)]
iristemp_std=scale(iristemp)
#finding distance
d=dist(iristemp,method = "euclidean")
d[1:5]
#clustering (get a cluster matrix)
clust_iris=hclust(d,method = "ward.D")
#dendogram of clustering
plot(clust_iris)
#get result in values of cluster
clusterd=cutree(clust_iris,3)
#To visualize cluster with rectangle border
rect.hclust(clust_iris,k=3,border = "red")
#confusion matrix
table(iris$Species,clusterd)

######Iris k-mean clustering
set.seed(5)
kclus_iris=kmeans(iristemp,3)
table(iris$Species,kclus_iris$cluster)

#####Iterative K - means
kclus_iris=kmeans(iristemp,3,nstart = 10)
kclus_iris$cluster
kclus_iris$totss
# kclus_irisB=kmeans(iristemp,1,nstart = 10)
# sum(kclus_irisB$centers)^2
### to calculate totss
x1=mean(iris$Sepal.Length)
x2=mean(iris$Sepal.Width)
x3=mean(iris$Petal.Length)
x4=mean(iris$Petal.Width)
a=sum((iris$Sepal.Length-x1)^2+(iris$Sepal.Width-x2)^2+(iris$Petal.Length-x3)^2+(iris$Petal.Width-x4)^2)
table(iris$Species,kclus_iris$cluster)
###calculate withinss
irisresult=cbind(iristemp,kclus_iris$cluster)
x1=mean(irisresult$Sepal.Length[irisresult$cluster==1])
x2=mean(irisresult$Sepal.Width[irisresult$cluster==1])
x3=mean(irisresult$Petal.Length[irisresult$cluster==1])
x4=mean(irisresult$Petal.Width[irisresult$cluster==1])
b=sum((irisresult$Sepal.Length[irisresult$cluster==1]-x1)^2+(iristemp$Sepal.Width[irisresult$cluster==1]-x2)^2+(iristemp$Petal.Length[irisresult$cluster==1]-x3)^2+(iristemp$Petal.Width[irisresult$cluster==1]-x4)^2)
kclus_iris$withinss
kclus_iris$tot.withinss
kclus_iris$betweenss


####Selecting best Kmeans iteration
i=numeric()
itr_withinss=c()
for(k in 1:10)
{
  kclus_iris_itr=kmeans(iristemp[-5],k,nstart = 10)
  itr_withinss=c(itr_withinss,kclus_iris_itr$tot.withinss)
}
###Elbow graph
plot(1:10,itr_withinss,type="b")
names(kclus_iris)
#####Gowers distance and jogaurd distance method for categorical variable
library("factoextra")

### soft cluster
fan=fanny(as.matrix(iristemp),k=10,maxit = 20)
names(fan)
fan$membership
############Wine dataset########
Wine=read.csv("/Users/rsklanu/CART/wine.csv",header = TRUE,sep = ",")
summary(Wine)
View(Wine)
WinefrClus=Wine[,-1]
Winescale=scale(WinefrClus)
distance=dist(Winescale,method = "euclidean")
clust_wine=hclust(distance,method = "ward.D")
plot(clust_wine)
clust=cutree(clust_wine,3)
rect.hclust(clust_wine,k=3,border="blue")
table(Wine$Type,clust)
clust_wine=cbind(WinefrClus,clust)
mean(clust_wine$Alcohol[which(clust_wine$clust==1)])
mean(clust_wine$Alcohol[which(clust_wine$clust==2)])
mean(clust_wine$Alcohol[which(clust_wine$clust==3)])
sapply(clust_wine,function(x){mean(x[clust_wine$clust==1])})
sapply(clust_wine,function(x){mean(x[clust_wine$clust==2])})
sapply(clust_wine,function(x){mean(x[clust_wine$clust==3])})

#tapply to factor and then perform given operation
sapply(clust_wine,function(x){tapply(x, clust_wine$clust,mean)})

########################Data.txt#########
Data1=read.table("/Users/rsklanu/CART/data.txt",header=FALSE,sep = "|",quote = "\"")
View(Data1)
Data=read.delim("/Users/rsklanu/CART/data.txt",sep = "|",header = FALSE)
View(Data)
summary(Data)
colnames(Data)=c("ID","Title","ReleaseDate","VideoReleaseDate","IMDB","Unknown","Action","Adventure",
                        "Animation","Children's","Comedy","Crime","Documentary","Drama","Fantasy","FilmNoir",
                        "Horror","Musical","Mystery","Romance","Scifi","Thriller","War","Western"
                        )


###n_occur <- data.frame(table(Data$Title))
##dup=n_occur[n_occur$Freq > 1,]    for finding duplicate col

Data_clust=Data[,c(-1,-3,-4,-5)]
Data_clust=unique(Data_clust)
Data_clust=Data_clust[,-1]
data_distance=dist(Data_clust,method = "euclidean")
clust_data=hclust(data_distance,method = "ward.D")
plot(clust_data)
data_clust=cutree(clust_data,10)
rect.hclust(clust_data,k=10,border="blue")
summary(as.factor(data_clust))
Data_clust=cbind(Data_clust,data_clust)
View(Data_clust)
sapply(Data_clust, function(x){tapply(x, Data_clust$data_clust, mean)})


########################Data.txt#########

Data1=read.delim("/Users/rsklanu/CART/data.txt",sep = "|",header = FALSE)
View(Data1)
summary(Data1)
colnames(Data1)=c("ID","Title","ReleaseDate","VideoReleaseDate","IMDB","Unknown","Action","Adventure",
                 "Animation","Children's","Comedy","Crime","Documentary","Drama","Fantasy","FilmNoir",
                 "Horror","Musical","Mystery","Romance","Scifi","Thriller","War","Western"
)

Data_clust1=Data1[,c(-1,-4,-5)]
Data_clust1=unique(Data_clust1)
View(Data_clust1)
Data_clust1=Data_clust1[,-1]
Data_clust1$ReleaseDate=as.numeric(Data_clust1$ReleaseDate)
Data_clust1=data.frame(scale(Data_clust1))
data_distance1=dist(Data_clust1,method = "euclidean")
clust_data1=hclust(data_distance1,method = "ward.D")
plot(clust_data1)
data_clust1=cutree(clust_data1,10)
rect.hclust(clust_data1,k=10,border="blue")
summary(as.factor(data_clust1))
Data_clust1=cbind(Data_clust1,data_clust1)
View(Data_clust1)
sapply(Data_clust1[,-1], function(x){tapply(x, Data_clust1$data_clust1, mean)})

library(cluster)
c1=Data1[,c(-1,-4,-5)]
c1=unique(c1)
c1=c1[,c(-1,-2)]
clusplot(c1,data_clust1,main = "20 clust",color = T,shade = T,labels = c1[,1],lines=0)

#####K-mean clustering for iris
data_iris=


