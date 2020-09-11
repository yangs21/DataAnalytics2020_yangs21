library(MASS)
attach(Boston)
?Boston #Show documentation
help("Boston") #Show documentation
head(Boston, 5)
dim(Boston)
names(Boston)
str(Boston)
nrow(Boston)
ncol(Boston)
summary(Boston) #Summary for each and every column
summary(Boston$crim)

library(ISLR)
data(Auto)
head(Auto)
head(Auto, 9)
summary(Auto$mpg)
fivenum(Auto$mpg)
boxplot(Auto$mpg)

help("read.csv")
data1 <- read.csv(file.choose(), header = T)
