## libraries
library("dplyr")
library("ggplot2")
library("reshape2")
library("magrittr")
library("ggpubr")

## reading data from dataset
## df <- read.csv("data/bank-additional-full.csv", header=TRUE, sep=";")
df <- read.csv("data/wine.data", header=TRUE, sep=",")
summary(df)
print(head(df, n=3))

## check there are missed value or not
sum(is.na(df))

## boxplot only numeric data
boxplot(df)

## select one var and work with it
hist(df$ash)
boxplot(df$ash)

## removing outliers
outliers <- boxplot(df$ash, plot=FALSE)$out
df <- df[-which(df$ash %in% outliers),]


ggqqplot(df$ash)
ggdensity(df$ash, 
          main = "Density plot of ash",
          xlab = "Alcohol %")


shapiro.test(df$ash)
ks.test(df$ash, "pnorm", mean(df$ash), sd(df$ash))


## data transformation
df$new <- exp(df$ash)
hist(df$new)
boxplot(df$new)
ggqqplot(df$new)
ggdensity(df$new, main = "Density plot of Alcohol",xlab = "Alcohol %")

shapiro.test(df$new)
ks.test(df$new, "pnorm", mean(df$new), sd(df$new))