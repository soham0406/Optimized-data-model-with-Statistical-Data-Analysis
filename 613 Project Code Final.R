library(ISLR)
library(boot)
library(randomForest)
library(gbm)
library(readxl)
train <- read_excel("C:/Users/soham/OneDrive/Desktop/Soham MS/ISEN SEM 1/613/Project/train.xlsx")
View(train)
test <- read_excel("C:/Users/soham/OneDrive/Desktop/Soham MS/ISEN SEM 1/613/Project/test.xlsx")
View(test)

#Boosting
set.seed(1)
boost <- gbm(Y1 ~ X1 + X4 + X5 + X7 + X8 + X1:X3, data = train, distribution = "gaussian", n.trees = 1800, interaction.depth = 4)
summary(boost)

set.seed(1)
yhat.boost <- predict(boost, newdata = train)
## Using 1800 trees...
mean((yhat.boost - train$Y1)^2)
#Validation set for boosting
set.seed(1)
tr=sample(1:nrow(train),275)
te=train[-tr,]
ts= train[tr,]
set.seed(1)
bs <- gbm(Y1 ~ X1 + X4 + X5 + X7 + X8 + X1:X3, data = ts, distribution = "gaussian", n.trees = 1800, interaction.depth = 4)
summary(bs)
set.seed(1)
bs2 <- predict(bs, newdata = te)
mean((bs2 - te$Y1)^2)













#linear
library(ISLR)
library(readxl)
set.seed(1)
tr=sample(1:nrow(train),275)
te=train[-tr,]
ts= train[tr,]

linear_model <- lm(log(Y1) ~ X1 + X4+ X1*X3 + X5  + X7 + X8, data = train)
summary(linear_model)
lm2 <- predict(linear_model, data = train)
mean((exp(lm2) - train$Y1)^2)

lm_cv <- lm(log(Y1) ~ X1 + X4+ X1*X3 + X5  + X7 + X8, data = ts)
lm_cv_test <- predict(lm_cv, newdata = te)
mean((exp(lm_cv_test) - te$Y1)^2)

par(mfrow=c(2,2))
plot(linear_model)





#random forest
set.seed(1)
randomforest <- randomForest(Y1 ~ X1 + X4 + X5 + X7 + X8 + X1:X3, data = train, ntree=1000, mtry = 3, importance = TRUE)
randomforest
set.seed(1)
yhat.rf <- predict(randomforest, newdata = train)
mean((yhat.rf - train$Y1)^2)
set.seed(1)
rf <- randomForest(Y1 ~ X1 + X4 + X5 + X7 + X8 + X1:X3, data = ts, mtry = 3, ntree=1000, importance = TRUE)
rf
set.seed(1)
yhat.rf2 <- predict(rf, newdata = te)
mean((yhat.rf2 - te$Y1)^2)
varImpPlot(randomforest)

#Fitting test data on the best model
set.seed(1)
boost <- gbm(Y1 ~ X1 + X4 + X5 + X7 + X8 + X1:X3, data = train, distribution = "gaussian", n.trees = 1800, interaction.depth = 4)
summary(boost)

set.seed(1)
yhat.boost <- predict(boost, newdata = test)
## Using 1800 trees...
mean((yhat.boost - test$Y1)^2)


#Making modifications for the best model
set.seed(1)
boost <- gbm(Y1 ~X1 + X4 + X5 + X6 + X7 + X8 + X1:X3, data = train, distribution = "gaussian", n.trees = 2500, interaction.depth = 4)
summary(boost)

set.seed(1)
yhat.boost <- predict(boost, newdata = test)
## Using 2500 trees...
mean((yhat.boost - test$Y1)^2)
pairs(train)
cor(train)

