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
library("e1071")

#%% reading datasets
df1 <- read.csv("data/winequality-red.csv", header = TRUE, sep = ";")
df2 <- read.csv("data/winequality-white.csv", header = TRUE, sep= ';')
df <- rbind(df1, df2)
df$class <- c(rep("red", nrow(df1)), rep("white", nrow(df2)))
df$num_class <- c(rep(0, nrow(df1)), rep(1, nrow(df2)))

summary(df1)
summary(df2)
summary(df)

tr_df <-sqrt(select(df, -class,-num_class))
tr_df <-select(tr_df, -fixed.acidity)
tr_df <-select(tr_df, -citric.acid)
tr_df <-select(tr_df, -total.sulfur.dioxide)
tr_df <-select(tr_df, -density)
tr_df <-select(tr_df, -sulphates)

corrplot(cor(tr_df,method = "spearman"))
tr_df$num_class <- df$num_class

model.full <- glm(num_class~., data = tr_df,family = binomial(link = 'logit'))
summary(model.full)

model.reduced <- glm(num_class ~ quality + volatile.acidity,data=tr_df,  family = binomial(link='logit'))
summary(model.reduced)

colldiag(model.full)
vif(model.full)

colldiag(model.reduced)
vif(model.reduced)


model.final <- model.reduced
summary(model.final)
summary(model.final)$coefficient

## Computing OR
exp(coef(model.final))

model.final.probs <- predict(model.final, type="response")
model.final.probs[1:5]

#plotting model
plot(model.final.probs,df$num_class, xlab = "predicted probs", ylab="actual")

model.final.pred <- ifelse(model.final.probs>0.5, "white", "red")
model.final.pred

model.final.tab <- table(predicted=model.final.pred, actual=df$class)
model.final.tab

confusionMatrix(model.final.tab, positive="white")
