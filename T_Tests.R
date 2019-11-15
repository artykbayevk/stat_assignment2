## libraries
library("dplyr")
library("ggplot2")
library("reshape2")
library("magrittr")
library("ggpubr")
library("car")
library("corrplot")
library("psych")
library("MVN") ## only on windows
library("perturb")
library("caret")
library("olsrr")
library("tidyverse")
library("broom")


#%% reading datasets
df1 <- read.csv("data/winequality-red.csv", header = TRUE, sep = ";")
df2 <- read.csv("data/winequality-white.csv", header = TRUE, sep= ';')
df <- rbind(df1, df2)
df$class <- c(rep("red", nrow(df1)), rep("white", nrow(df2)))

summary(df1)
summary(df2)
summary(df)

data <- data.frame("quality"=df$quality, "class"=df$class)

summary(data)
print(head(data, n=3))



## checking normality for red and white wines


boxplot(df1$quality)
qqnorm(df1$quality)
ggqqplot(df1$quality)
ggdensity(df1$quality, main = "Density plot of Alcohol",xlab = "Alcohol %")
hist(df1$quality)

shapiro.test(df1$quality)
n <- rnorm(10, mean=5, sd=2)
shapiro.test(n)

boxplot(df2$quality)
qqnorm(df2$quality)
ggqqplot(df2$quality)
ggdensity(df2$quality, main = "Density plot of Alcohol",xlab = "Alcohol %")
hist(df2$quality)

shapiro.test(df2$quality)
n <- rnorm(10, mean=5, sd=2)
shapiro.test(n)


df1$transformed1 <- sqrt(df1$quality)
df2$transformed2 <- sqrt(df2$quality)
hist(df$quality)
hist(df1$transformed1)
hist(df2$quality)
hist(df2$transformed2)




## and we can assume that variables are not normally distributed.
## so, we need to use non-parametric test

fligner.test(quality~class, data = df)
# p value = 0.4319 > 0.05, it means that we reject H0, 

# now we can easily test T-test
t.test(df1$quality, df2$quality, var.equal = TRUE, paired = FALSE)
t.test(quality~class, data=df, var.equal = TRUE, paired=FALSE)

# mann whitney U test
wilcox.test(quality~class, data=df)

head(df)
