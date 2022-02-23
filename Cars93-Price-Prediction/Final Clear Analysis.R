library(MASS)
library(plotrix)
library(ISLR)
library(ggplot2)
library(cowplot)
library(lattice)
library(moments)
library(ineq)
library(MASS)
library(car)
library(tidyverse)
library( leaps )
library( qpcR )
library(faraway)
library(depth)
library(aplpack)
library(ddalpha)
library(GGally)
library(glmnet)
library(boot)
library(ggfortify)
library(ISLR)
library(GGally)


## Import the data set
df = MASS::Cars93
attach(df)
head(df)

###############################
########### DATA CLEANING######
###############################

## Replacing with median
df$Luggage.room[is.na(df$Luggage.room)] <- median(df$Luggage.room, na.rm = TRUE)  # Replace NA in one column
#Replacing with 0
df$Rear.seat.room[is.na(df$Rear.seat.room)] <- 0  # Replace NA in one column
#Check for no more values
colSums(is.na(df))

## Removing the variables that were decided to be removed
df <- df[ c(1, 3, 5, 7:26) ]
df <- df[ c(1:4,7:23)]
names(df)

ggcorr(df, label=T)

####### TRAIN TEST ###########

set.seed(100) 
index = sample(1:nrow(df), 0.7*nrow(df)) 
train = df[index,] # Create the training data 
test = df[-index,] # Create the test data

dim(train)
dim(test)

###############################
########LINEAR REGRESSION######
###############################

lr = lm(Price~., data=train)
summary(lr)

  ## EVALUATION ##
  eval_metrics = function(model, df, predictions, target){
  resids = df[,target] - predictions
  resids2 = resids**2
  N = length(predictions)
  r2 = as.character(round(summary(model)$r.squared, 2))
  adj_r2 = as.character(round(summary(model)$adj.r.squared, 2))
  print(adj_r2) #Adjusted R-squared
  print(as.character(round(sqrt(sum(resids2)/N), 2))) #RMSE
  }

  ## PREDICTION on TRAIN##
  predictions = predict(lr, newdata=train)
  eval_metrics(lr, train, predictions, target='Price')

  ## PREDICTION on TEST##
  predictions = predict(lr, newdata = test)

  ## PLOTING ##
  par(mfrow = c(2,2))
  plot(lr)
  
###############################
######### REGULARIZATION ######
###############################  
library(caret)
dummies = dummyVars(Price~. , data=df)
train_dummies = predict(dummies, newdata = train)
test_dummies = predict(dummies, newdata=test)
print(dim(train_dummies))
print(dim(test_dummies))  


###############################
#########RIDGE REGRESSION######
###############################

x = as.matrix(train_dummies)
y_train = train$Price

x_test = as.matrix(test_dummies)
y_test = test$Price

lambdas = c(10^seq(10,-2,length.out = 100) ,0)

ridge_reg = glmnet(x, y_train, nlambda=25, alpha = 0 , family = 'gaussian', lambda=lambdas)
summary(ridge_reg)

cv_ridge = cv.glmnet(x, y_train, alpha=0, lambda=lambdas)

optimal_lambda = cv_ridge$lambda.min
optimal_lambda

  ## EVALUATION ##
  eval_results <- function(true, predicted, df) {
  SSE <- sum((predicted - true)^2)
  SST <- sum((true - mean(true))^2)
  R_square <- 1 - SSE / SST
  RMSE = sqrt(SSE/nrow(df))
  
  # Model performance metrics
  data.frame(
    RMSE = RMSE,
    Rsquare = R_square
  )
  }
  
  ## PREDICTION ##
  predictions_train = predict(ridge_reg, s=optimal_lambda, newx = x)
  eval_results(y_train, predictions_train, train)  
  
  predictions_test = predict(ridge_reg, s=optimal_lambda, newx = x_test)
  eval_results(y_test, predictions_test, test)  
  
  ## PLOTTING BEST LAMBDA ##
  library(plotmo)
  graphics.off()
  plot_glmnet(ridge_reg,xvar = "lambda",xlim = c(-10,10))
  plot_glmnet(ridge_reg,xvar = "dev")
  
  x11()
  autoplot(cv_ridge, ylim=c(0, 100))
  ?autoplot
###############################
#########RIDGE REGRESSION######
###############################

lambdas = c(10^seq(10,-2,length.out = 100) ,0)

lasso_reg = cv.glmnet(x, y_train, alpha = 1, lambda = lambdas, standarize = TRUE, nfolds=5)
?cv.glmnet
lambda_best = lasso_reg$lambda.min
lambda_best

lasso_model = glmnet(x, y_train, alpha = 1, lambda = lambda_best, standardize = TRUE)

autoplot(lasso_reg, ylim=c(0,100))

prediction_train = predict(lasso_model, s=lambda_best, newx = x)
eval_results(y_train, predictions_train, train)

###############################
######   CONCLUSIONS  ########
###############################

# As our model was better with Linear:

## LINEAR 
    #R2: 0.979
    #AdjustedR2: 0.83
    #RMSE: 1.25

## RIDGE: 
  # R2: 0.901
  # RMSE: 2.682

## LASSO:
  # R2: 0.912
  # RMSE: 2.42

# But we tried to do another linear regressios to see if our model could be discarded
# Our model didn't improve.
#What we want to do now is 

stepAIC(lr, direction = "both" )

lr_step = lm(Price ~ Manufacturer + Type + MPG.city + Cylinders + Fuel.tank.capacity + Width + Rear.seat.room + Luggage.room + Weight, data = train)
plot(lr_step)

summary(lr)
summary(lr_step)

names(df)
df_step = df[c(1, 2, 4, 6, 12, 16, 18, 19, 20, 3)]

ggcorr(df_step, label = T)

prediction_train1 = predict(lr_step)
prediction_train

eval_results(y_train, prediction_train1, train)

