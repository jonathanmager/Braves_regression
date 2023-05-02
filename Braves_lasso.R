
#install.packages("tidyverse")
library(tidyverse)
braves <- read.csv("braves_data.csv")
library(lars)

#combining predictors
predictors = cbind(braves$pitchERA,braves$pitchBB,braves$pitchSO, braves$pitch1B,braves$pitch2B, braves$pitch3B, braves$pitchHR, braves$bat1B, braves$bat2B, braves$bat3B, braves$HR, braves$BB, braves$SO, braves$OBP, braves$SLG, braves$OPS)
colnames(predictors) = c("pitchERA","pitchBB","pitchSO", "pitch1B","pitch2B", "pitch3B", "pitchHR", "bat1B", "bat2B", "bat3B", "HR", "BB", "SO", "OBP", "SLG", "OPS")
library(glmnet)

#Using cross validation with alpha = 1 lasso regression
rd.cv = cv.glmnet(predictors, braves$RD, alpha = 1, nfolds = 10)
rdmodel = glmnet(predictors, braves$RD, alpha = 1, nlambda = 100)
coef(rdmodel, s = rd.cv$lambda.min)

#plotting how to coefficients are brought down
plot(rdmodel, xvar="lambda", lwd=2, label = TRUE)
abline(v=log(rd.cv$lambda.min), col='black', lty=2, lwd=2)



yhat = predict(rdmodel, predictors,type = "response")
y = braves$RD



yhat
res = yhat -y
plot(res)
braves23 <- read.csv("braves_data - 2023.csv")



#Used for predictions
predictors23 = cbind(braves23$pitchERA,braves23$pitchBB,braves23$pitchSO, braves23$pitch1B,braves23$pitch2B, braves23$pitch3B, braves23$pitchHR, braves23$bat1B, braves23$bat2B, braves23$bat3B, braves23$HR, braves23$BB, braves23$SO, braves23$OBP, braves23$SLG, braves23$OPS)



predict(rdmodel, rd.cv, newx = as.matrix(predictors23))
predictors23
colnames(predictors23) = c("pitchERA","pitchBB","pitchSO", "pitch1B","pitch2B", "pitch3B", "pitchHR", "bat1B", "bat2B", "bat3B", "HR", "BB", "SO", "OBP", "SLG", "OPS")
