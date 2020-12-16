# Clear memory:
rm(list=ls())

# Change working directory
setwd("/Users/Mavis/RPI Lectures/Data Analytics/Project/Data")

# Import library
library(dplyr)
library(ggplot2)
library(tidyverse)	
library(reshape2)
library(factoextra)
library(pROC)
library(randomForest)

## -------------------- Import the Indicator_9.3.1 dataset ----------------------------------##
df <- read.csv(file="Indicator_9.3.1__Proportion_of_small-scale_industries_in_total_industry_value_added__percent.csv",header = TRUE,sep = ",")
# Drop unwanted columns:
small_business <- df[-c(5,6,14:27)]
# Drop country with no data:
small_business <- small_business[rowSums(is.na(small_business[,12:28])) != ncol(small_business[,12:28]),]
summary(small_business)
nrow(small_business)

# Calculate mean value from 2000 to 2016:
small_business$mean_value <- rowMeans(
  small_business[,12:28], na.rm = TRUE)
View(small_business)
# Plot histogram:
hist(small_business$mean_value)
# Plot boxplot:
boxplot(small_business$mean_value)
# Plot qqlot:
qqnorm(small_business$mean_value)
qqline(small_business$mean_value)

# Plot mean value by country in ascending order:
df1 <- small_business[order(small_business$mean_value),]
df1 <- df1[1:10,]
m1 <- df1$ISO3
df <- data.frame(df1$mean_value, m1)
View(df)
df <- melt(df, id.vars='m1')
ggplot(df, aes(reorder(x=m1, df1$mean_value), y=value, fill=m1)) + geom_bar(stat='identity') + 
  labs(title = "10 Countries with Lowest Percentage of Small Business",x = "Country", y = "Percentage")

# Plot mean value by country in descending order:
df2 <- small_business[order(-small_business$mean_value),]
View(df2)
df2 <- df2[1:10,]
m2 <- df2$ISO3
df <- data.frame(df2$mean_value, m2)
df <- melt(df, id.vars='m2')
ggplot(df, aes(reorder(x=m2, -df2$mean_value), y=value, fill=m2)) + geom_bar(stat='identity') + 
  labs(title = "10 Countries with Highest Percentage of Small Business", x = "Country", y = "Percentage")

## -------------------- Import the Indicator_8.5.1 dataset ------------------------------------##
df <- read.csv(file="Indicator_8.5.1%3A_Average_hourly_earnings_of_employees_by_sex_and_occupation__local_currency.csv",header=TRUE,sep=",")
# Drop unwanted columns:
earnings <- df[-c(1:12,20,21,23:26,35:38,59:64)]

# Select data related to females:
female_earnings <- earnings[!(earnings$sex_code == "_T"), ]
female_earnings <- female_earnings[!(
  female_earnings$sex_code=="M"), ]
# Drop country with no data:
female_earnings <- female_earnings[rowSums(
  is.na(female_earnings[,17:35])) != ncol(female_earnings[,17:35]),]
View(female_earnings)
summary(female_earnings)
# Select data related to males:
male_earnings <- earnings[!(earnings$sex_code == "_T"), ]
male_earnings <- earnings[!(earnings$sex_code == "F"), ]
# Drop country with no data:
male_earnings <- male_earnings[rowSums(is.na(male_earnings[,17:35])) != ncol(male_earnings[,17:35]),]
View(male_earnings)
summary(male_earnings)

# Calculate mean earnings for female data from 2000 to 2016:
female_earnings$mean_earnings <- rowMeans(female_earnings[,17:35], na.rm = TRUE)
parent_name <- female_earnings$parentName
ISO3 <- female_earnings$ISO3
mean_earnings_country <- female_earnings$mean_earnings
fe <- data.frame(parent_name, ISO3, mean_earnings_country)
country_earnings_fe <- data.frame(fe %>% group_by(ISO3) %>% summarise(mean_country_fe=mean(mean_earnings_country)))
View(country_earnings_fe)

# Calculate mean earnings for male data from 2000 to 2016:
male_earnings$mean_earnings <- rowMeans(male_earnings[,17:35], na.rm = TRUE)
parent_name <- male_earnings$parentName
ISO3 <- male_earnings$ISO3
mean_earnings_country <- male_earnings$mean_earnings
ma <- data.frame(parent_name, ISO3, mean_earnings_country)
country_earnings_ma <- data.frame(ma %>% group_by(ISO3) %>% summarise(mean_country_ma=mean(mean_earnings_country)))
View(country_earnings_ma)

# Merge the female earnings dataset and male earnings dataset 
# Delete NA values:
earnings_sex <- merge(country_earnings_fe, 
                      country_earnings_ma, by="ISO3", all=TRUE)
earnings_sex <- earnings_sex %>% drop_na()
earnings_sex$ratio <- earnings_sex$mean_country_fe/
  earnings_sex$mean_country_ma
View(earnings_sex)

# Plot histogram:
h <- hist(earnings_sex$ratio, Plot = FALSE)
plot(h, xaxt = "n", xlab = "Ratio", ylab = "Frequency", main = "Ratio of Earnings (Female to Male)", col = "light blue")
axis(1, h$mids, tick = FALSE, padj= -1.5)
# Plot boxplot:
boxplot(earnings_sex$ratio, col = "pink")
# Plot qqlot:
qqnorm(earnings_sex$ratio, col = "red")
qqline(earnings_sex$ratio, col = "purple")

# Plot ratio by country in ascending order:
df1 <- earnings_sex[order(earnings_sex$ratio),]
df1 <- df1[1:10,]
m1 <- df1$ISO3
df <- data.frame(df1$ratio, m1)
df <- melt(df, id.vars='m1')
ggplot(df, aes(reorder(x=m1, df1$ratio), y=value, fill=m1)) + geom_bar(stat='identity') + 
  labs(title = "10 Countries with Lowest Earning Ratios (Female to Male)", x = "Country", y = "Percentage")

# Plot ratio by country in descending order:
df2 <- earnings_sex[order(-earnings_sex$ratio),]
View(df2)
df2 <- df2[1:10,]
m2 <- df2$ISO3
df <- data.frame(df2$ratio, m2)
df <- melt(df, id.vars='m2')
ggplot(df, aes(reorder(x=m2, -df2$ratio), y=value, fill=m2)) + geom_bar(stat='identity') + 
  labs(title = "10 Countries with Highest Earning Ratios (Female to Male)", x = "Country", y = "Percentage")

## --------------- Combine the current small_business and earnings_sex dataset ----------------##
parentName <- small_business$parentName
ISO3 <- small_business$ISO3
mean_sb <- small_business$mean_value
df <- data.frame(parentName, ISO3, mean_sb)
data1 <- merge(earnings_sex, df, by="ISO3", all=TRUE)
data1 <- data1 %>% drop_na()
View(data1)
nrow(data1)

## ------------- Import the Women Business and the Law Index Score dataset ----------------------##
df <- read.csv(file="Women Business and the Law Index Score.csv",header = TRUE,sep = ",")
# Drop unwanted columns:
df <- df[-c(3:14, 65)]
# Drop country with no data:
women_index <- df[rowSums(is.na(df[,3:52])) != ncol(df[,3:52]),]
summary(women_index)

# Calculate mean value from 1970 to 2019:
women_index$mean_index <- rowMeans(women_index[,3:52], na.rm = TRUE)
View(women_index)
# Plot histogram:
h <- hist(women_index$mean_index, Plot = FALSE)
plot(h, xaxt = "n", xlab = "Score", ylab = "Frequency", main = "Women Business Index Score", col = "light green")
axis(1, h$mids, tick = FALSE, padj= -1.5)
# Plot boxplot:
boxplot(women_index$mean_index, col = "pink")
# Plot qqlot:
qqnorm(women_index$mean_index, col="light blue")
qqline(women_index$mean_index, col="blue")

# Plot women index score by country in ascending order:
df1 <- women_index[order(women_index$mean_index),]
df1 <- df1[1:10,]
m1 <- df1$Country.Code
df <- data.frame(df1$mean_index, m1)
df <- melt(df, id.vars='m1')
ggplot(df, aes(reorder(x=m1, df1$mean_index), y=value, fill=m1)) + geom_bar(stat='identity') + 
  labs(title = "10 Countries with Lowest Women Index Score", x = "Country", y = "Score")

# Plot women index score by country in descending order:
df2 <- women_index[order(-women_index$mean_index),]
View(df2)
df2 <- df2[1:10,]
m2 <- df2$Country.Code
df <- data.frame(df2$mean_index, m2)
df <- melt(df, id.vars='m2')
ggplot(df, aes(reorder(x=m2, -df2$mean_index), y=value, fill=m2)) + geom_bar(stat='identity') + 
  labs(title = "10 Countries with Highest Women Index Score", x = "Country", y = "Score")

## ------------- Import the World Bank SME dataset ----------------------##
sme <- read.csv(file="SME_Number_Per capita.csv",header = TRUE,sep = ",")
View(sme)

# Plot histogram:
h <- hist(sme$Amount, Plot = FALSE)
plot(h, xaxt = "n", xlab = "Number Per Capita", ylab = "Frequency", main = "SME Number Per Capita", col = "orange")
axis(1, h$mids, tick = FALSE, padj= -1.5)
# Plot boxplot:
boxplot(sme$Amount, col = "yellow")
# Plot qqlot:
qqnorm(sme$Amount, col="light blue")
qqline(sme$Amount, col="purple")

# Plot SME number per capita by country in ascending order:
df1 <- sme[order(sme$Amount),]
View(df2)
df1 <- df1[1:10,]
m1 <- df1$Country
df <- data.frame(df1$Amount, m1)
df <- melt(df, id.vars='m1')
ggplot(df, aes(reorder(x=m1, df1$Amount), y=value, fill=m1)) + geom_bar(stat='identity') + 
  labs(title = "10 Countries with Loweset SME Number Per Capita", x = "Country", y = "Number")

# Plot SME number per capita by country in descending order:
df2 <- sme[order(-sme$Amount),]
View(df2)
df2 <- df2[1:11,]
m2 <- df2$Country
df <- data.frame(df2$Amount, m2)
df <- melt(df, id.vars='m2')
ggplot(df, aes(reorder(x=m2, -df2$Amount), y=value, fill=m2)) + geom_bar(stat='identity') + 
  labs(title = "11 Countries with Highest SME Number Per Capita", x = "Country", y = "Number")

## --------------- Combine the women index dataset and SME dataset ----------------##
country_name <- women_index$Country.Name
country_code <- women_index$Country.Code
mean_women_score <- women_index$mean_index
df1 <- data.frame(country_name, country_code, mean_women_score)
#View(df1)
country_name <- sme$Country
sme_number <- sme$Amount
df2 <- data.frame(country_name, sme_number)
#View(df2)
data2 <- merge(df1, df2, by="country_name", all=TRUE)
data2 <- data2 %>% drop_na()
View(data2)

## -------------------- Model 1: Regression -------------------------##
# Apply univariate regression for data1:
lm_gender1 <- lm(data1$ratio ~ data1$mean_sb)
coef_gender1 <- coef(lm_gender1)
print(coef_gender1)
## Regression validation:
summary(lm_gender1) 

# Apply univariate regression for data2:
lm_gender2 <- lm(data2$mean_women_score ~ data2$sme_number)
coef_gender2 <- coef(lm_gender2)
print(coef_gender2)
## Multiple regression validation:
summary(lm_gender2) 
# Plot the regression:
ggplotRegression <- function (fit) {
  require(ggplot2)
  ggplot(fit$model, aes_string(x = names(fit$model)[2], y = names(fit$model)[1])) + 
    geom_point() + stat_smooth(method = "lm", col = "red") +
    labs(title = paste("Adj R2 = ",signif(summary(fit)$adj.r.squared, 5),
                       "Intercept =",signif(fit$coef[[1]],5 ),
                       " Slope =",signif(fit$coef[[2]], 5),
                       " P =",signif(summary(fit)$coef[2,4], 5)))
}
ggplotRegression(lm_gender2)


## -------------------- Model 2: KMeans -------------------------##
# Re-organize data2 and use country_code as index
mean_gender_score <- data2$mean_women_score
number_sme <- data2$sme_number
data <- data.frame(mean_gender_score, number_sme)
rownames(data) <- data2$country_name
View(data)
# Compute the optimal number of clusters:
fviz_nbclust(data, kmeans, method = "wss") + geom_vline(xintercept = 4, linetype = 2)
# Compute the clusters:
km_result <- kmeans(data, 4, nstart = 5)
cluster <- data.frame(km_result$cluster)
cluster_size <- km_result$size
print(cluster)
print(cluster_size)

# Visualization of KM clustering:
fviz_cluster(km_result, data = data,
             palette = c("#E7B800", 
                 "#2E9FDF", "#00AFBB", 
                 "#FC4E07"),
             ellipse.type = "euclid",
             star.plot = TRUE, 
             repel = FALSE,
             ggtheme = theme_minimal(),
             xlab = "Women Index Score",
             ylab = "SME Number Per Capita")

## -------------------- Model 3: RandomForest -------------------------##
score_randomforest <- randomForest(mean_gender_score ~.,
                                    data = data,
                                    ntree =80,
                                    mtry=3,
                                    importance=TRUE ,
                                    proximity=TRUE)
pred_score <- predict(score_randomforest,newdata=data)
pred_score <- as.numeric(pred_score)

# Compute the difference between predict data and true data:
table <- data.frame(data$mean_gender_score, pred_score)
View(table)

# Plot predict score and true score:
plot(xaxt= 'n', data$mean_gender_score,type="l", col="red", main="Predicted Score 
     (Green) vs. True Score (Red)")
lines(xaxt = 'n', pred_score, col="green")
     
# Compute mean squared error:
mse <- sqrt(sum((table$pred_score-table$data.mean_gender_score)^2) / nrow(table))
mse
