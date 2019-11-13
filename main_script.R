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
ggdensity(df$ash, main = "Density plot of ash",xlab = "Alcohol %")


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






## other codes
df1$new <- log(df1$fixed.acidity)
hist(df1)

ggplot(df1, aes(new)) +
  geom_histogram(aes(y = ..density..), colour = "black", fill = "white") +
  stat_function(fun = dnorm, args = list(mean = mean(df1$new, na.rm = T), sd = sd(df1$new, na.rm = T))) + xlab("new (%)")

boxplot(df1$new)
qqnorm(df1$new)
ggqqplot(df1$new)
ggdensity(df1$new, main = "Density plot of Alcohol",xlab = "Alcohol %")



model <- glm(formula = quality ~ fixed.acidity + volatile.acidity + citric.acid + residual.sugar + chlorides + free.sulfur.dioxide + total.sulfur.dioxide + density + pH + sulphates + alcohol, data = df1)
model2 <- glm(formula = quality ~ alcohol, data = df1)
plot(model)


