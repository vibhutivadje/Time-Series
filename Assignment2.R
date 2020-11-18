## USE FORECAST LIBRARY.

install.packages("forecast")
library(forecast)
library(zoo)
#**************************************************Q1:*************************************************************#
##a)
## CREATE DATA FRAME. 
walmart.data <- read.csv(file.choose(), header = TRUE)
# See the first 6 records of the file.
head(walmart.data)

## USE ts() FUNCTION TO CREATE TIME SERIES DATA SET.
revenue.ts <- ts(walmart.data$Revenue, 
               start = c(2005, 1), end = c(2020, 2), freq = 4)

revenue.ts

##b):
## Use plot() to plot time series data  
plot(revenue.ts, 
     xlab = "Time", ylab = "Revenue (in million $) ", 
     ylim = c(70000, 150000), main = "Walmart Quaterly Revenue", col = "blue")


# Use stl() function to plot times series components of the original data. 
# The plot includes original data, trend, seasonal, and reminder 
# (level and noise component).
revenue.stl <- stl(revenue.ts, s.window = "periodic")
revenue.stl
autoplot(revenue.stl, main = "Walmart Revenue Time Series Components")

#***************************************************Q2:*************************************************************#
##a)
#### CREATE DATA PARTITION.
#Data Partiton with VAlidation partition of 16 periods and training partition of 46 periods
nValid <- 16
nTrain <- length(revenue.ts) - nValid
train.ts <- window(revenue.ts, start = c(2005, 1), end = c(2005, nTrain))
valid.ts <- window(revenue.ts, start = c(2005, nTrain + 1), 
                   end = c(2005, nTrain + nValid))

##b)
##FIT REGRESSION MODEL WITH LINEAR TREND: MODEL 1
# create regression model with linear trend.
train.lin <- tslm(train.ts ~ trend) #Trend mean function B0+B1t+e
#trend parameter means simply linear trend.

# See summary of linear trend model and asociated parameters.
summary(train.lin)

# Apply forecast() function to make forecast for validation period.
train.lin.pred <- forecast(train.lin, h = nValid, level = 0)
train.lin.pred

##FIT REGRESSION MODEL WITH Quadratic(Polynominal) TREND: MODEL 2.
# Use tslm() function to create quadratic (polynomial) trend model.
train.quad <- tslm(train.ts ~ trend + I(trend^2))

# See summary of quadratic trend model and asociated parameters.
summary(train.quad)

# Apply forecast() function to make predictions for ts data in
# validation set.  
train.quad.pred <- forecast(train.quad, h = nValid, level = 0)
train.quad.pred

##FIT REGRESSION MODEL WITH Seasonality: MODEL 3.
#tslm() function to create seasonal model.
train.season <- tslm(train.ts ~ season)

# See summary of seasonal model and asociated parameters.
summary(train.season)

# If necessary, run the following code to identify seasons
train.season$data 

# Apply forecast() function to make predictions for ts with 
# seasonality data in validation set.  
train.season.pred <- forecast(train.season, h = nValid, level = 0)
train.season.pred


## FIT REGRESSION MODEL WITH LINEAR TREND AND SEASONALITY: MODEL 4. 
# create linear trend and seasonal model.
train.trend.season <- tslm(train.ts ~ trend  + season)

# See summary of quadratic trend and seasonality model and asociated parameters.
summary(train.trend.season)

# Apply forecast() function to make predictions for ts with 
# trend and seasonality data in validation set.  
train.trend.season.pred <- forecast(train.trend.season, h = nValid, level = 0)
train.trend.season.pred

## FIT REGRESSION MODEL WITH QUADRATIC TREND AND SEASONALITY: MODEL 5

# create Quadratic trend and seasonal model.
train.quad.trend.season <- tslm(train.ts ~ trend  +I(trend^2)+ season)

# See summary of quadratic trend and seasonality model and asociated parameters.
summary(train.quad.trend.season)

# Apply forecast() function to make predictions for ts with 
# trend and seasonality data in validation set.  
train.quad.trend.season.pred <- forecast(train.quad.trend.season, h = nValid, level = 0)
train.quad.trend.season.pred

##c)
## Apply Accuracy function
###ACCURACY OF REGRESSION MODEL WITH LINEAR TREND: MODEL 1. 
round(accuracy(train.lin.pred, valid.ts), 3)
## ACCURACY OF REGRESSION MODEL WITH Quadratic(Polynominal) TREND: MODEL 2. 
round(accuracy(train.quad.pred, valid.ts), 3)
## ACCURACY OF REGRESSION MODEL WITH Seasonality: MODEL 3.
round(accuracy(train.season.pred, valid.ts), 3)
## ACCURACY OF REGRESSION MODEL WITH LINEAR TREND AND SEASONALITY: MODEL 4
round(accuracy(train.trend.season.pred, valid.ts), 3)
## ACCURACY OF REGRESSION MODEL WITH QUADRATIC TREND AND SEASONALITY: MODEL 5
round(accuracy(train.quad.trend.season.pred, valid.ts), 3)

#***************************************************Q3:*************************************************************#

##a)
#Using Best Model (Model 5 and Model 4 ) 
#for entire dataset to Forecast and Predict Walmarts revenue in the 4 quarters of 2020 & 2021

## REGRESSION MODEL WITH QUADRATIC TREND AND SEASONALITY Model 5 

# create quadratic trend and seasonality model.

# create quadratic trend and seasonality model.
revenue.quad.trend.season <- tslm(revenue.ts ~ trend + I(trend^2) + season)

# see summary of linear trend equation and asociated parameters.
summary(revenue.quad.trend.season)


# Apply forecast() function to make predictions for ts with 
# trend and seasonality data in validation set.  
revenue.quad.trend.season.pred <- forecast(revenue.quad.trend.season, h = 4, level = 0)
revenue.quad.trend.season.pred



## REGRESSION MODEL WITH LINEAR TREND AND SEASONALITY Model 4
revenue.lin.trend.season <- tslm(revenue.ts ~ trend + season)
# summary of linear trend equation and asociated parameters.
summary(revenue.lin.trend.season)

# Apply forecast() function to make predictions for ts with 
# trend and seasonality data in validation set.  
revenue.lin.trend.season.pred <- forecast(revenue.lin.trend.season, h = 4, level = 0)
revenue.lin.trend.season.pred

##b)
## PERFORMANCE MEASURE OF REGRESSION MODEL WITH QUADRATIC TREND AND SEASONALITY Model 5 
# for naive model, seasonal naive, and regression model with quadratic trend and seasonality.
round(accuracy(revenue.quad.trend.season.pred$fitted, revenue.ts),3)
round(accuracy((naive(revenue.ts))$fitted, revenue.ts), 3)
round(accuracy((snaive(revenue.ts))$fitted, revenue.ts), 3)

## PERFORMANCE MEASURE OF REGRESSION MODEL WITH linear TREND AND SEASONALITY Model 4
# for naive model, seasonal naive, and regression model with linear trend and seasonality.
round(accuracy(revenue.lin.trend.season.pred$fitted, revenue.ts),3)
round(accuracy((naive(revenue.ts))$fitted, revenue.ts), 3)
round(accuracy((snaive(revenue.ts))$fitted, revenue.ts), 3)


