#%% libraries
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

## creating simple dataset for linear regression
data <- data.frame("alcohol" = df1$alcohol, "quality" = df1$quality)

## checking normality between this variables
boxplot(data$alcohol)
boxplot(data$quality)

# checking normality in alcohol
qqnorm(data$alcohol)
ggqqplot(data$alcohol)
ggdensity(data$alcohol, main = "Density plot of Alcohol",xlab = "Alcohol %")
hist(data$alcohol)

# checking normality in quality
qqnorm(data$quality)
ggqqplot(data$quality)
ggdensity(data$quality, main = "Density plot of Quality",xlab = "Alcohol %")
hist(data$quality)

# transformation data
tr_data <- sqrt(data)

## checking normality between this variables on transformed data
boxplot(tr_data$alcohol)
boxplot(tr_data$quality)

# checking normality in alcohol on transformed data
qqnorm(tr_data$alcohol)
ggqqplot(tr_data$alcohol)
ggdensity(tr_data$alcohol, main = "Density plot of Alcohol",xlab = "Alcohol %")
hist(tr_data$alcohol)

# checking normality in quality on transformed data
qqnorm(tr_data$quality)
ggqqplot(tr_data$quality)
ggdensity(tr_data$quality, main = "Density plot of Quality",xlab = "Alcohol %")
hist(tr_data$quality)

### quality doesnt need to transform
tr_data$quality <- data$quality
tr_data$pH <- data$pH

ks.test(tr_data$alcohol, rnorm(nrow(tr_data)))
ks.test(tr_data$quality, rnorm(nrow(tr_data)))

## correlation test
res <- cor.test(tr_data$quality, tr_data$alcohol, method = "spearman", exact = F)
res
res$estimate

res_squared <- res$estimate^2
res_squared

## data see that this correlation not so good
plot(tr_data$quality~tr_data$alcohol)

# working with the model
model <- lm(formula = quality ~ alcohol, data = tr_data)
summary(model)


model.diag.metrics <- augment(model)
head(model.diag.metrics)


# plot residuals
ggplot(model.diag.metrics, aes(alcohol, quality)) +
  geom_point() +
  stat_smooth(method = lm, se = FALSE) +
  geom_segment(aes(xend = alcohol, yend = .fitted), color = "red", size = 0.3)

#%% plotting model

par(mfrow = c(2, 2))
plot(model)

# running anova
anova(model)

#alternative anova testing
summary(aov(model))

# SSM 236
# SSR 805
# SST = 236 + 805

#anothw
Anova(model, type = "II")
acf(model$residuals)


