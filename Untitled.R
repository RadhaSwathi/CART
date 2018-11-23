library(rpart)
library(rpart.plot)
library(ggplot2)
df<-data.frame(matrix(nrow = 7,ncol = 3))
colnames(df)=c('X1','X2','Y')
df$X1<-c(1,1.2,3,4,8,9,10)
df$X2<-c(11,12,13,11,2,3,1)
df$Y<-c(1,1,1,1,0,0,0)
df$Y[5]<-1
ggplot(df,aes(x=X1,y=X2,colour=Y))+geom_point()
tree=rpart(Y~X1+X2,data = df,minbucket=1)
prp(tree)

library("RColorBrewer")
library("rattle")
fancyRpartPlot(tree)

system('brew install gtk+')

local({
  if (Sys.info()[['sysname']] != 'Darwin') return()
  
  .Platform$pkgType = 'mac.binary.el-capitan'
  unlockBinding('.Platform', baseenv())
  assign('.Platform', .Platform, 'package:base')
  lockBinding('.Platform', baseenv())
  
  options(
    pkgType = 'both', install.packages.compile.from.source = 'always',
    repos = 'https://macos.rbind.org'
  )
})

install.packages(c('RGtk2', 'cairoDevice', 'rattle'))

1-(580/1000)

(780-580)/(10000^0.5)



2.5+(1.0*5)+(-2.5*-2)
exp(12.5)/(1+exp(12.5))
