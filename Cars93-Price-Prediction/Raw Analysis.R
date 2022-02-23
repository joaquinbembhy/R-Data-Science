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
library(rgl)
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

#import the data,attach and check the 5 first rows
df = MASS::Cars93
attach(df)
head(df)


##### DESCRIPTIVE ANALYSIS ######
### Divide into numerical and categorical

?Cars93

str(df)
names(df)
length(df)
#we have 93 observations and a total of 27 variables (including target Price)

#check NULL values
colSums(is.na(df))
# Luggage.room = 11
# Rear.seat.room = 2

#Rear seat room  has missing values for cars with only 2 seats
table(df$Rear.seat.room)

#we replaced the nulls of Rear seat room for 0 because it doesn't have cubic
df$Rear.seat.room[is.na(df$Rear.seat.room)] <- 0  # Replace NA in one column
hist(df$Rear.seat.room)

#Luggage room has 11 null values
table(df$Luggage.room)
#we check the histogram to see the distribution
hist(df$Luggage.room)
#we can see that it is symetric, therefore we can replace null values by the median.
df$Luggage.room[is.na(df$Luggage.room)] <- median(df$Luggage.room, na.rm = TRUE)  # Replace NA in one column

#Analyze the target variable: price
hist(Price)
boxplot(Price)
median(Price)
summary(Price)

kurtosis(Price) #6.1, we can say that we probably have a large tail on the right side of the dataset
skewness(Price) #Right skewed 1.5
#We seem to have very large values for the price. According to the boxplot we have 3 outliers (check).
#Also, the kurtosis index if of 6.1 which is very high.


# We have observed that some of are variables are redundant to the dataset.
# MinPrice MaxPrice

df_min_max = df[,c(5, 4,6)]
#ggcorr(df_min_max)
ggcorr(df_min_max, label=TRUE, label_round = 3)
?ggcorr
#All the variables are highly correlated between each othr.
#This is because the Price is the average of the Min and Max Prices
#Therefore, we can just drop this columns.

#We check how many different models we have
unique(df$Model)
#as it is 93, we know that there is a model for each car and they don't repeat.
#We can also drop this variable.
df <- df[ c(1, 3, 5, 7:26) ]
str(df)

#The variable 'Make' was alto dropped because it was a concatenation of Maufacturer and Model.
#Now we have 24 variables

#Analyze MPG.City MPG.Highway
ggplot(df, aes(x=MPG.highway, y=MPG.city)) + geom_smooth(method='lm', se=FALSE, color='red') + geom_point(color='black')
#We can see that they are extremely highly correlated. We can see their correlation index too.
cor(df$MPG.city, df$MPG.highway)
# It's correlation is of 0.94. Therefore, we can just keep the one that is more highly corelated to price.
ggcorr(df[c(3, 4, 5)], label = TRUE,  label_round = 3)
#MPG city is more highly correlated, therefore we keep that variable
df <- df[ c(1:4,7:23)]

names(df)
df_num = df[c(3, 4, 7:10, 12)]

str(df)

cor(df)

barplot(df$DriveTrain, df$Price)

?barplot

####Partition data
set.seed(100) 
index = sample(1:nrow(df), 0.7*nrow(df)) 
train = df[index,] # Create the training data 
test = df[-index,] # Create the test data

dim(train)
dim(test)


######## RIDGE #############

#split train test
split1<- sample(c(rep(0, 0.7 * nrow(df)), rep(1, 0.3 * nrow(df))))
split1

table(split1)

train <- df[split1 == 0, ]   
test <- df[split1== 1, ]    

y_train = train[3]
y_test = test$Price

y_train
dim(train)
dim(test)

##Model

model = lm(Price ~. , data = df)

t = model.matrix(Price~. , data = df)[,-1]

lambdas = c(10^seq(10,-2,length.out = 100) ,0)

model_ridge = lm.ridge(df$Price ~ ., data=df, lambda = bestlam, model = FALSE) 
model_ridge = glmnet(t,df$Price,lambda = lambdas,alpha = 0, thresh = 1e-10)

cv_ridge = cv.glmnet(t, df$Price, alpha=0, lambda = lambdas)
optimal = cv_ridge$lambda.min
optimal

summary(model_ridge)


library(plotmo)
plot_glmnet(model_ridge,xvar = "lambda",xlim = c(-10,10))
plot_glmnet(model_ridge,xvar = "dev")

#choose lambda
cv.model_ridge = cv.glmnet(t,df$Price,lambda = lambdas,alpha = 0,nfolds = 10)

x11()
autoplot(cv.model_ridge)

bestlam = cv.model_ridge$lambda.min
bestlam

indbest = which(cv.model_ridge$lambda == bestlam)

reglam = cv.model_ridge$lambda.1se   
indreg = which(cv.model_ridge$lambda == reglam)

coef(model_ridge)[,c(indbest,indreg)]  
cv.model_ridge$cvm[c(indbest,indreg)]

cv.model_ridge$cvm[101] ### praticamente (o quasi) uguale al seguente
cv.glm(df, glm(Price~., data = df), K = 10)$delta[1]

  ##### Predict
predictions_train = predict(model_ridge, s=optimal, newx = t)
predictions_train
eval_results(y_train, predictions_train, train)

  ##### Errors
eval_results <- function(true, predicted, df) {
  SSE <- sum((predicted - true)^2)
  SST <- sum((true - mean(true))^2)
  R_square <- 1 - SSE / SST
  RMSE = sqrt(SSE/nrow(df))


df_num = df[,c(5,7,8,12,13,14,15,17,18, 19, 20, 21, 22, 23, 24, 25)]
df_cat = df[,c(5,1,3,9, 10, 11, 16, 26)]

##### LASSO ######
library(lattice)
library(caret)

model_lasso <- train(t, df$Price, method = 'lasso')
model_lasso
plot(model_lasso)
plot(varImp(model_lasso))


#
boxplot(df$Luggage.room)
hist(df$Luggage.room)



?ggcorr

df_num = df[,c(4,6,7,8,12,13,14,15,17,18, 19, 20, 21, 22, 25)]
plot(df_cat)


plot(df_num)

# ARRANCAMOS
df = df[,c(5, 7, 12, 13, 17, 19, 21, 25, 1, 9, 10, 11, 26)]
attach(df)
df_num = df[,c(2, 3, 4, 5, 6, 7, 8)]
df_cat = df[,c(9, 10, 11, 12, 13)]
names(df_num)
names(df)

ggcorr(df, label = T)

#####
par(mfrow=c(2,3))
boxplot(df$MPG.city, main='MPG')
boxplot(df$EngineSize, main='EngineSize')
boxplot(df$Horsepower, main= 'Horsepower')
boxplot(df$Fuel.tank.capacity, main= 'Fuel Tank Capacity')
boxplot(df$Length, main='Length')
boxplot(df$Width, main='Width')
#####

## Kurtosis & Skewness ##
apply(df_num, 2, skewness)
apply(df_num, 2, kurtosis)

#correlation between variables
ggcorr(df[,c(1, 2, 3, 4, 5, 6, 7, 8)], label = T)

plot(df)

#make a model
model = lm(Price~., data=df)
summary(model)

#make variabe selection
stepAIC( model, direction = "both" )

model$coefficients

model1 = lm(Price ~ Horsepower + Fuel.tank.capacity + Width + Weight + Manufacturer + AirBags + DriveTrain + Cylinders, data = df)
summary(model1)

str(df_cat)

AIC(model)
AIC(model1)


x11()
par(mfrow = c(2,2))
plot(model)

shapiro.test(model$residuals)

train <- df[split1 == 0, ]            
train

test <- df[split1== 1, ]    
test

train_model = lm(Price ~ Horsepower + Fuel.tank.capacity + Width + Weight + Manufacturer + AirBags + DriveTrain + Cylinders, data = train)
Ypred_train = predict(train_model) 
Ypred_test = predict(train_model)

MSE_train = sum((train$Price - Ypred_train )^2)/(dim(train)[1])
MSE_train

MSE_test = sum((test$Price - Ypred_test)^2)/(dim(test)[1])
MSE_test

plot(Ypred_test, train$Price, col='red') 

summary(train_model)

?plot

###Categorical###
length(unique(df$Model))           #93, doesn't make sense to keep it
length(unique(df$Manufacturer))    #32
length(unique(df$Type))            #6
length(unique(df$AirBags))         #3
length(unique(df$DriveTrain))      #3
length(unique(df$Passengers))      #6
length(unique(df$Make))            #93, doesn't make sense to keep it
length(unique(df$Origin))          #2

df_num = df[,c(1,3,9, 10, 16, 18, 26)]
names(df_num)
length(df_num) #we kept 7 numerical variables

#statistic summary
summary(df_num)
summary(df_cat)
#Boxplots and Histogram

length(df_cat$Manufacturer)

par(mfrow=c(2,3))
boxplot(df$Min.Price, main='Min Price')
boxplot(df$Max.Price, main='Max Price')
boxplot(df$MPG.city, main= 'MPG.city')
boxplot(df$MPG.highway, main= 'MPG Highway')
boxplot(df$Horsepower, main='Horsepower')
boxplot(df$RPM, main='RPM')


par(mfrow=c(2,3))
hist(df$Min.Price, main='Min Price')
hist(df$Max.Price, main='Max Price')
hist(df$MPG.city, main= 'MPG.city')
hist(df$MPG.highway, main= 'MPG Highway')
hist(df$Horsepower, main='Horsepower')
hist(df$RPM, main='RPM')

apply(df_num, 2, skewness)
apply(df_num, 2, kurtosis)

#from the boxplots and the histograms we can say that:
  # Min Price: right skewed, aparent outliers on the right side
  # Max price:  right skewed, normal dist, big outliers on the right side
  # MPG.city: right skewed, exponential distribution, aparent outliers on the right
  # MPG Highway: right skewed, apparent outliers on the right
  # Horsepower: slight right skew, some outliers on the right
  # RPM: almost symetrical dist, outliers on the right


####### Model

ggcorr(df_num, label = T)
ggcorr(df_cat, label = T)

model = lm(Price~., data=df)
stepAIC( model, direction = "both" )

new_model = lm(Price ~ Min.Price + Max.Price + MPG.city + MPG.highway + 
  EngineSize + RPM + Rev.per.mile + Passengers + Length + Wheelbase + 
  Width + Turn.circle + Weight + Manufacturer + Type + AirBags + 
  Cylinders, data = df)

summary(new_model)
