# clear plots
if(!is.null(dev.list())) dev.off()

# clear console
cat("\014")

# clean workspace
rm(list=ls())

setwd("C:/Users/Checo/Desktop/projectum/github_repos/2021/tinnitus")
getwd()

#install.packages("RColorBrewer")
library(RColorBrewer)
tin.rdata = read.csv("data.csv",header=T)  
fix(tin.rdata)

#### Multiple Linear Regression Analysis
## 1 - Descriptive Analysis of the data

# Data cleaning - Combining levels (the categories) in the "Group" predictor  
levels(tin.rdata$Group) = c("Control","Control", "Treatment")

# Pie chart
pie.GROUP = pie(table(tin.rdata$Group), labels=c('Control', 'Treatment'))
pie.GENDER = pie(table(tin.rdata$Gender), labels=c('Male','Female'))

#Histograms  
hist.GROUP= plot(as.factor(tin.rdata$Group),xlab = names(tin.rdata)[2], main = paste("Histogram of ",names(tin.rdata)[2]), ylim=c(0,80) )
hist.HHI  = hist(as.numeric(tin.rdata[,3]), xlab = names(tin.rdata)[3], main = paste("Histogram of ",names(tin.rdata)[3]) )
hist.GAD = hist(as.numeric(tin.rdata[,4]), xlab = names(tin.rdata)[4], main = paste("Histogram of ",names(tin.rdata)[4]) )
hist.PHQ= hist(as.numeric(tin.rdata[,5]), xlab = names(tin.rdata)[5], main = paste("Histogram of ",names(tin.rdata)[5]),ylim=c(0,60))
hist.ISI= hist(as.numeric(tin.rdata[,6]), xlab = names(tin.rdata)[6], main = paste("Histogram of ",names(tin.rdata)[6]))
hist.SWLS= hist(as.numeric(tin.rdata[,7]), xlab = names(tin.rdata)[7], main = paste("Histogram of ",names(tin.rdata)[7]),ylim=c(0,40))
hist.HYP= hist(as.numeric(tin.rdata[,8]), xlab = names(tin.rdata)[8], main = paste("Histogram of ",names(tin.rdata)[8]),ylim=c(0,40))
hist.CFQ= hist(as.numeric(tin.rdata[,9]), xlab = names(tin.rdata)[9], main = paste("Histogram of ",names(tin.rdata)[9]),ylim=c(0,40))
hist.GENDER= plot(as.factor(tin.rdata$Gender),xlab = paste(names(tin.rdata)[10],"(1 = Man , 2 = Female)"), main = paste("Histogram of ",names(tin.rdata)[10]) )
hist.AGE= hist(as.numeric(tin.rdata[,11]), xlab = names(tin.rdata)[11], main = paste("Histogram of ",names(tin.rdata)[11]),ylim=c(0,35))
hist.DURATION= hist(as.numeric(tin.rdata[,12]), xlab = names(tin.rdata)[12], main = paste("Histogram of ",names(tin.rdata)[12]))
hist.PRE= hist(as.numeric(tin.rdata[,13]), xlab = names(tin.rdata)[13], main = paste("Histogram of ",names(tin.rdata)[13]))

#Summary
summary(tin.rdata)

## 2 - Data cleaning

# Data imputation with mean for Pre and Post TFI scores
tin.rdata$Post_TFI_Score[is.na(tin.rdata$Post_TFI_Score)] = mean(tin.rdata$Post_TFI_Score, na.rm = TRUE)

# Create response variable "TFI_Reduction" 
TFI_Reduction = tin.rdata$Post_TFI_Score - tin.rdata$Pre_TFI_Score
tin.rdata$TFI_Reduction = TFI_Reduction
View(tin.rdata)

# Histogram of Post_TFI_Score and TFI_Reduction
hist.POST= hist(as.numeric(tin.rdata[,14]), xlab = names(tin.rdata)[14], main = paste("Histogram of ",names(tin.rdata)[14]),ylim=c(0,100), xlim=c(0,100))
hist.RED= hist(as.numeric(tin.rdata[,15]), xlab = names(tin.rdata)[15], main = paste("Histogram of ",names(tin.rdata)[15]))
par(mfrow=c(1,1))

## 3
# Data imputation for numerical values using mean and data imputation for categorical values using mode

## 4 - Partition the data set 

# Mix Data Set
set.seed(123)
mix.data = runif(nrow(tin.rdata)) # Randomly genertes 142 values 
tin.rdata_mix = tin.rdata[order(mix.data),]
sset.data1 = subset(tin.rdata_mix,select = -c(Pre_TFI_Score,Post_TFI_Score))   #Removing Pre and Post TFI variables

#Partition the data set into a trainning set (80%)  and test set (20%)
#install.packages("caret")
#install.packages('ggplot2')
library(ggplot2)
library(caret)
set.seed(123)
train.index =createDataPartition(sset.data1$TFI_Reduction,p=.8,list = F, times=1)
train.data = sset.data1[train.index,]
test.data = sset.data1[-train.index,]
names(sset.data1)

## 5 - Multiple Regression Analysis
# Remove the "Subject ID" indicator column and perform Multiple regression analysis 
data.fit = lm(TFI_Reduction ~ . -Subject_ID  ,data = train.data)
data.fit
options(scipen="100",digits = "4")
summary(data.fit)
  
par(mfrow=c(2,2))
plot(data.fit)

# Forward selection model
# Forward Selection (Approach A)
# install.packages("olsrr")
library(olsrr)
fws_a.aic = ols_step_forward_aic(data.fit,details=T) 
plot(fws_a.aic)

#Forward Selection (Approach B)
# fit.start = lm(TFI_Reduction ~ 1 , data = train.data)  # Specify Starting list of coefficients
# fws_b = step(fit.start, direction ="forward", scope=formula(data.fit))
# summary(fws_b)

# Backward selection model 
# Backward selection (Approach A)
bws_a.aic = ols_step_backward_aic(data.fit,details=T)
bws_a.aic
plot(bws_a.aic)
betas = bws_a.aic$model$coefficients
betas

# Backward Selection (Approach B)
# bws_b = step(data.fit, direction ="backward")
# summary(bws_b)

## 6 - Model diagnostic and correcttion of the model assumptions (if needed) 
## 7 - Factors that influence the reduction in TFI

train.response = lm(TFI_Reduction ~ ISI+GAD+SWLS , data = train.data ) 
summary(train.response)
plot(train.response)
summary(influence.measures(train.response))


## 8 - Make predictions on the test data set and calculate the mean square error
test.response = predict(train.response,newdata = test.data)
test.response
summary(test.response)
plot(test.response)

# Train Model Mean square error
sum((train.data$TFI_Reduction - train.response$fitted)^2)/nrow(train.data)

#Test Model Mean Square Error
sum((test.data$TFI_Reduction - test.response)^2)/nrow(test.data)


#### K-mean Regression
## 9,10,11,12 - K-mean regression to train several regression models (k= 2,4,6,8,10)
set.seed(123)
model.knn = train(train.data[,3:12],train.data[,13], method ="knn" , 
                  trControl=trainControl(method="cv", number =5), 
                  preProcess = c("center","scale"),
                  tuneGrid = expand.grid(k=seq(2,10, by =2)))
model.knn
plot(model.knn)
model.knn$bestTune


# Make prediction on the testing data set and find the Mean Square Error
prediction.knn = predict(object=model.knn ,newdata = test.data[,2:12])
prediction.knn
summary(prediction.knn)
if(!is.null(dev.list())) dev.off()
plot(prediction.knn) 


# Calculate RMSE and MSE
library(Metrics)
rmse = model.knn$results$RMSE
rmse.train = min(rmse)
rmse.train
mse.train = (rmse.train)^2
mse.train
  
rmse.test = rmse(test.data[,13],prediction.knn) 
rmse.test
mse.test = (rmse.test)^2
mse.test
