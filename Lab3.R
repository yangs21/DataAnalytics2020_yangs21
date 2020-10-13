
head(EPI_data.df)

View(EPI_data.df)

column_working<-na.omit(EPI_data.df$EPI)
column_working
mean.EPI_data.EPI<-mean(column_working)	
mean.EPI_data.EPI
median.EPI_data.EPI<-median(column_working)	
median.EPI_data.EPI
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}
mode.EPI_data.EPI<-getmode(column_working)	
mode.EPI_data.EPI
sd.EPI_data.EPI<-sd(column_working)	
sd.EPI_data.EPI
var.EPI_data.EPI<-var(column_working)	
var.EPI_data.EPI

hist(EPI_data.df$EPI)

hist(EPI_data.df$EPI, seq(30., 95., 1.0), prob=TRUE)
lines (density(EPI_data.df$EPI,na.rm=TRUE,bw=1.)) 
rug(EPI_data.df$EPI)

hist(EPI_data.df$EPI, seq(30., 95., 1.0), prob=TRUE)
lines (density(EPI_data.df$EPI,na.rm=TRUE,bw="SJ"))


column_working2<-na.omit(EPI_data.df$DALY)
column_working2
mean.EPI_data.DALY<-mean(column_working2)	
mean.EPI_data.DALY
median.EPI_data.DALY<-median(column_working2)	
median.EPI_data.DALY
mode.EPI_data.DALY<-getmode(column_working2)	
mode.EPI_data.DALY
sd.EPI_data.DALY<-sd(column_working2)	
sd.EPI_data.DALY
var.EPI_data.DALY<-var(column_working2)	
var.EPI_data.DALY

hist(EPI_data.df$DALY)

hist(EPI_data.df$DALY)
rug(EPI_data.df$DALY)



##--2b--
attach(EPI_data.df)
names(EPI_data.df)
boxplot(EPI,AGRICULTURE,FORESTRY,FISHERIES)
lmEPI <- lm(EPI ~ AGRICULTURE+FORESTRY+FISHERIES)
lmEPI
summary(lmEPI)
cEPI <- coef(lmEPI)
cEPI

AGRICULTURE2<- c(seq(1,231,1))
AGRICULTURE2
FORESTRY2 <- c(seq(1,231,1))
FISHERIES2<- c(seq(1,231,1))
NEW <- data.frame(AGRICULTURE2,FORESTRY2,FISHERIES2)

pEPI <- predict(lmEPI,NEW,interval = "prediction")

cEPI <- predict(lmEPI,NEW,interval = "confidence")


names(EPI_data.df)
boxplot(CLIMATE,INDOOR_raw,EPI,WATER_H)
lmCLIMATE <- lm(CLIMATE ~ INDOOR_raw+EPI+WATER_H)
lmCLIMATE
summary(lmCLIMATE)
cCLI <- coef(lmCLIMATE)
cCLI
INDOOR_raw2 <- c(seq(1,231,1))
EPI2 <- c(seq(1,231,1))
WATER_H2 <- c(seq(1,231,1))
NEW2 <- data.frame(INDOOR_raw2,EPI2,WATER_H2)
pCLIMATE <- predict(lmCLIMATE,NEW2,interval = "prediction")

cCLIMATE <- predict(lmCLIMATE,NEW2,interval = "confidence")



boxplot(AIR_E,EPI,DALY,OZONE_pt)
lm_AIR_E <- lm(AIR_E ~ EPI+DALY+OZONE_pt)
lm_AIR_E
summary(lm_AIR_E)
cAIR_E <- coef(lm_AIR_E)
cAIR_E
EPI2 <- c(seq(1,231,1))
DALY2 <- c(seq(1,231,1))
OZONE_pt2 <- c(seq(1,231,1))
NEW3 <- data.frame(EPI2,DALY2,OZONE_pt2)
p_AIR_E <-  predict(lm_AIR_E,NEW3,interval = "prediction")
c_AIR_E <-  predict(lm_AIR_E,NEW3,interval = "confidence")



##-----Lab2_Part2-----
#--2a--
rm(list=ls())
multivariate<- read.csv(file="C:/Users/yexu3/Desktop/data analytics/dataset_multipleRegression.csv",header=T)
attach(multivariate)
lmmultivariate_1<- lm(ROLL ~ HGRAD + UNEM)
lmmultivariate_1
summary(lmmultivariate_1)
cmultivariate_1 <- coef(lmmultivariate_1)
cmultivariate_1


lmmultivariate_2 <- lm(ROLL ~ HGRAD + UNEM + INC)
lmmultivariate_2
summary(lmmultivariate_1)
cmultivariate_2 <- coef(lmmultivariate_2)
cmultivariate_2 

r1 <- function(x1,x2){
  r <- -8255.7510591 + 0.9422769*x1 + 698.2681316*x2
  return(r)
}

r2 <- function(y1,y2,y3){
  r <- -9153.2544627 + 0.4064837*y1 + 450.1245037*y2 + 4.2748577*y3
  return(r)
}

answer <- matrix(NA,nrow=1,ncol=2)
answer
answer[1,1] <- r1(90000,7)
answer[1,2] <- r2(90000,7,25000)
answer
#--2b--
rm(list=ls())
abalone <- read.csv(file="C:/Users/yexu3/Desktop/data analytics/abalone.csv",header=T)
dim<-dim(abalone)[1]
dim
sampling.rate=0.9
num.test.set.labels=dim*(1.-sampling.rate)
training <-sample(1:dim,sampling.rate*dim, replace=FALSE)

train<-subset(abalone[training,],select=c(Rings,Diameter))

testing<-setdiff(1:dim,training)

test<-subset(abalone[testing,],select=c(Rings,Diameter))

cg<-abalone$Sex[training]

true.labels<-abalone$Sex[testing]
true.labels
install.packages("kknn")
library(kknn)

classif<-knn(train,test,cg,k=5)
classif
attributes(.Last.value)

#--2c--
library(dplyr)
library(ggplot2)
library(tidyverse)	

data("iris")
iris

plot(iris)

scaledIris<-scale(iris[,-5])
k_meansIris<-kmeans(scaledIris,3)
k_meansIris

k<-list()
for(i in 1:10){
  k[[i]]<-kmeans(scaledIris, i)
}
k

betweenss_totalss<-list()
for(i in 1:10){
  betweenss_totalss[[i]]<-k[[i]]$betweenss/k[[i]]$totss
}
betweenss_totalss
plot(1:10, betweenss_totalss, type='b',ylab='Between SS / Total SS', xlab="clusters(k)")

for(i in 1:4){
  plot(iris, col=k[[i]]$cluster)
}

d<-dist(scaledIris)

hclust_itis<-hclust(d, "ward.D2")
plot(hclust_itis)

rect.hclust(hclust_itis,k=3,border="blue")

clusters<-cutree(hclust_itis, 3)
clusters
