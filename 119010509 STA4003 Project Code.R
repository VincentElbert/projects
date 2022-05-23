# STA4003 APPdata forecast
# 119010509 Vincent Elbert

library(fpp2)
library(ggplot2)
library(ggfortify)

#Loading the data.project file from the workspace given
setwd("C:/Users/vince/Desktop/task/STA4003/project")
load("AppData.rdata")

#Split into train and test data set
data.project[,2:10] <- scale(data.project[,2:10])
index <- which(data.project$date == "2020-03-01") #Setting the bound to 1st March 2020
train <- data.project[1:(index-1), ]
test <- data.project[index:length(date), ]

#Extract all dates for later use
date <- unique(data.project$date)
#Convert train data set from to time series
data.project <- ts(data.project[, c(2:10)], 
            start = c(format(date, "%Y")[1], format(date, "%j")[1]), 
            frequency = 365)

#Extract all dates for later use
traindate <- unique(train$date)
#Convert train data set from to time series
train <- ts(train[, c(2:10)], 
                 start = c(format(traindate, "%Y")[1], format(traindate, "%j")[1]), 
                 frequency = 365)

#Extract all dates for later use
testdate <- unique(test$date)
#Convert test data set from to time series
test <- ts(test[, c(2:10)], 
            start = c(format(testdate, "%Y")[1], format(testdate, "%j")[1]), 
            frequency = 365)

#Plot function
plot.ts <- function(i){
  autoplot(train[, as.numeric(i)]) + ggtitle("Time Series Data of AppData", i) + 
    xlab("Time") + ylab("Usage in MB")
}  
plot.ts(1)
plot.ts(2)
plot.ts(3)
plot.ts(4)
plot.ts(5)
plot.ts(6)
plot.ts(7)
plot.ts(8)
plot.ts(9)

plotdiff <- function(i){
  diffdata <- diff(train[, i])
  autoplot(diffdata) + ggtitle("Time Series by differencing", i) + 
    xlab("Time") + ylab("Changes in MB")
}
plotdiff(1)
plotdiff(2)
plotdiff(3)
plotdiff(4)
plotdiff(5)
plotdiff(6)
plotdiff(7)
plotdiff(8)
plotdiff(9)

#Use Arima model to choose which order is the best
forecast <- c()
for (i in 1:ncol(train)){
  fit.arima <- auto.arima(train[, i], approximation = FALSE, 
                          stepwise = FALSE, trace = TRUE, stationary = TRUE)
  predict <- forecast(fit.arima, h = 60)
  forecast <- cbind(forecast, predict[["mean"]])
}

#Calculate MSE
MSE <- c()
for (i in 1:ncol(forecast)){
  diff_squared <- (forecast[, i] - test[, i])^2
  MSE <- cbind(MSE, diff_squared)
}

sum(difference_total)/(9*(30+30))