#%% libraries
library("dplyr")
library("ggplot2")
library("reshape2")
library("magrittr")
library("ggpubr")
library("car")
library("corrplot")
library("psych")
library("MVN")

#%% reading datasets
df1 <- read.csv("data/winequality-red.csv", header = TRUE, sep = ";")
df2 <- read.csv("data/winequality-white.csv", header = TRUE, sep= ';')
df1$class <- rep("red", nrow(df1))
df2$class <- rep("white", nrow(df2))
df <- rbind(df1, df2)
df$class <- c(rep("red", nrow(df1)), rep("white", nrow(df2)))

summary(df1)
summary(df2)
summary(df)

### we know that there are multiple linear regression. 
# we need to check many assumptions, that have to be done for MLR analysis
# 1st - Linear Relation ship between variables
# 2nd - Multivariate Normality
# 3rd - No Multicollinearity
# 4th - Homoscedasticity
corrplot(cor(df1,method = "spearman"))

df1$new <- log(df1$fixed.acidity)
hist(df1)

ggplot(df1, aes(new)) +
  geom_histogram(aes(y = ..density..), colour = "black", fill = "white") +
  stat_function(fun = dnorm, args = list(mean = mean(df1$new, na.rm = T), 
                                         sd = sd(df1$new, na.rm = T))) + xlab("new (%)")

boxplot(df1$new)
qqnorm(df1$new)
ggqqplot(df1$new)
ggdensity(df1$new, main = "Density plot of Alcohol",xlab = "Alcohol %")


res<- mvn(df1, univariateTest = "SW", univariatePlot = "histogram")

model <- glm(formula = quality ~ fixed.acidity + volatile.acidity + citric.acid + residual.sugar + chlorides + free.sulfur.dioxide + total.sulfur.dioxide + density + pH + sulphates + alcohol, data = df1)
model2 <- glm(formula = quality ~ alcohol, data = df1)
plot(model)
