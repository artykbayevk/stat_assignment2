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
corrplot(cor(tr_df1,method = "spearman"))


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
reduced2 <- lm(formula = quality~alcohol+volatile.acidity, data=tr_df1)
# summary for two models
summary(full)
summary(reduced1)
summary(reduced2)

# coefficients of models
summary(full)$coefficient
summary(reduced1)$coefficient
summary(reduced2)$coefficient

# estimating error rate
err <- sigma(full)/mean(tr_df1$quality)
err
err <- sigma(reduced1)/mean(tr_df1$quality)
err
err <- sigma(reduced2)/mean(tr_df1$quality)
err

# anova for comparison two models
anova(reduced1, full)

# stepwise model selection
model.full <- lm(formula = quality~alcohol+pH+free.sulfur.dioxide+chlorides+residual.sugar+volatile.acidity, data = tr_df1)
model.null <- lm(formula = quality~1, data = tr_df1)

# characteristics of models
summary(model.null)
summary(model.full)

step(model.null,
     scope = list(upper=model.full),
     direction="both",
     data=tr_df1)    

model.final <- lm(formula = quality ~ alcohol + volatile.acidity + pH + chlorides, data = tr_df1)
summary(model.final)

# checking significance of model
Anova(model.final, type = "II")

## actual value and predicted values
tr_df1$pred <- predict(model.final)
plot(pred ~ quality,data=tr_df1,pch = 16,xlab="Actual response value",
     ylab="Predicted response value")
abline(0,1, col="blue", lwd=2)

## checking distribution of residuals.
resid_ <- model.final$residuals
hist(resid_)
plot(fitted(model.final),residuals(model.final))
plot(model.final)
##A plot of residuals vs. predicted values.  The residuals should be unbiased and homoscedastic.














