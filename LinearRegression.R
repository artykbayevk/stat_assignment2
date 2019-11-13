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
library("perturb")
library("caret")
library("olsrr")

#%% reading datasets
df1 <- read.csv("data/winequality-red.csv", header = TRUE, sep = ";")
df2 <- read.csv("data/winequality-white.csv", header = TRUE, sep= ';')
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

## normality checking
res<- mvn(df1, univariateTest = "SW", univariatePlot = "histogram")
res<- mvn(df1, univariateTest = "SW", univariatePlot = "qqplot")
res<- mvn(df1, univariateTest = "SW", univariatePlot = "scatter")

## transforming whole dataset
tr_df1 <- sqrt(df1)



new_res <- mvn(tr_df1, univariateTest = "SW", univariatePlot = "histogram")



## all data not normally distr. by the reason of not normal distr of data, we choose spearman analysis
corrplot(cor(df1,method = "spearman"))
pairs.panels(df1)

corrplot(cor(tr_df1,method = "spearman"))
pairs.panels(tr_df1)

# removing correlated independent variables for avoiding multicollinearity
tr_df1 <-sqrt(df1)
tr_df1 <-select(tr_df1, -fixed.acidity)
tr_df1 <-select(tr_df1, -citric.acid)
tr_df1 <-select(tr_df1, -total.sulfur.dioxide)
tr_df1 <-select(tr_df1, -density)
tr_df1 <-select(tr_df1, -sulphates)
corrplot(cor(tr_df1,method = "spearman


corrplot.mixed(cor(tr_df1,method = "spearman"), lower = "number")


# other assumption for multicollinearity
colldiag(full)
colldiag(reduced1)
vif(reduced1)
vif(full)
ols_vif_tol(reduced1)
ols_test_correlation(reduced1)
ols_coll_diag(reduced1)
## constructing multiple linear regression



full <- lm(formula = quality~alcohol+pH+free.sulfur.dioxide+chlorides+residual.sugar+volatile.acidity, data = tr_df1)
reduced1 <- lm(formula = quality~alcohol+chlorides+volatile.acidity, data=tr_df1)

summary(full)
summary(reduced1)

confint(full, conf.level=0.95)

anova(reduced1, full)

RSS <- c(crossprod(model1$residuals))
MSE <- RSS / length(model1$residuals)
RMSE <- sqrt(MSE)
RMSE
res_ <- model$residualse


















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
