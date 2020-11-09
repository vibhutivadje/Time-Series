## USE FORECAST LIBRARY.

install.packages("forecast")
library(forecast)
library(zoo)
##Q1:
##a)
## CREATE DATA FRAME. 
#Grocery.data <- read.csv("673_case1.csv")
Grocery.data <- read.csv(file.choose(), header = TRUE)
# See the first 6 records of the file.
head(Grocery.data)

## USE ts() FUNCTION TO CREATE TIME SERIES DATA SET.
sales.ts <- ts(Grocery.data$Sales, 
                   start = c(2015, 1), end = c(2019, 12), freq = 12)


sales.ts

##b):
## Use plot() to plot time series data  
plot(sales.ts, 
     xlab = "Time", ylab = "Sales (in million $) ", 
     ylim = c(100, 500), main = "Grocery Store Sales", col = "blue")

# Use stl() function to plot times series components of the original data. 
# The plot includes original data, trend, seasonal, and reminder 
# (level and noise component).
sales.stl <- stl(sales.ts, s.window = "periodic")
sales.stl
autoplot(sales.stl, main = "Gorcery Store Time Series Components")


##c):
# Use Acf() function to identify autocorrelation and plot autocorrelation
# for different lags (up to maximum of 12).
autocor <- Acf(sales.ts, lag.max = 12, main = "Autocorrelation for Grocery Store Sales")

##Q2
##a) Trailing MAs
## Create trailing moving average with window (number of periods) k = 2,6,12.
## In rollmean(), use argument align = "right" to calculate a trailing MA.
ma.trailing_2 <- rollmean(sales.ts, k = 2, align = "right")
ma.trailing_6 <- rollmean(sales.ts, k = 6, align = "right")
ma.trailing_12 <- rollmean(sales.ts, k = 12, align = "right")

##b)
##Window K = 2:
## Create forecast for residuals for the 12 periods into the future.
ma.trailing_2.pred <- forecast(ma.trailing_2, h = 12, level = 0)
ma.trailing_2.pred

##Window K =6:
## Create forecast for residuals for the 12 periods into the future.
ma.trailing_6.pred <- forecast(ma.trailing_6, h = 12, level = 0)
ma.trailing_6.pred

##Window K =12:
## Create forecast for residuals for the 12 periods into the future.
ma.trailing_12.pred <- forecast(ma.trailing_12, h = 12, level = 0)
ma.trailing_12.pred

##c)
# Use accuracy() function to identify common accuracy measures.
# Use round() function to round accuracy measures to three decimal digits.
round(accuracy((snaive(sales.ts))$fitted, sales.ts), 3)
round(accuracy(ma.trailing_2, sales.ts), 3)
round(accuracy(ma.trailing_6, sales.ts), 3)
round(accuracy(ma.trailing_12, sales.ts), 3)

##Q3
## a)
## DE-TRENDING and DE-SEASONALIZING TIME SERIES USING REGRESSION
## CREATE TRAILING MA USING RESIDUALS.
## FORECAST USING REGRESSION MODEL WITH LINEAR TREND AND TRAILING MA INTO FUTURE PERIODS.
# Fit a regression model with trend and seasonality.
reg.trend.seas <- tslm(sales.ts ~ trend + season)
summary(reg.trend.seas)

# Create forecast for the 12 periods into the future.
reg.trend.seas.pred <- forecast(reg.trend.seas, h = 12, level = 0)
reg.trend.seas.pred

## b)
# Identify and display residulas for time series based on the regression
# (differences between actual and regression values in the same periods)
reg.trend.seas.res <- reg.trend.seas$residuals
reg.trend.seas.res

# Apply trailing MA with 12 periods in the window to residuals.
ma.trailing.res_2 <- rollmean(reg.trend.seas.res, k = 2, align = "right")
ma.trailing.res_2

# Create forecast for residuals for the 12 periods into the future.
ma.trailing.res_2.pred <- forecast(ma.trailing.res_2, h = 12, level = 0)
ma.trailing.res_2.pred

# To develop real forecast for 12 periods into the future, 
# combine regression forecast and trailing MA forecast for residuals.
ts.forecast.12 <- reg.trend.seas.pred$mean + ma.trailing.res_2.pred$mean
ts.forecast.12

# Create a table with regression forecast, trailing MA for residuals
# and total forecast for 12 months into the future
total.reg.ma.pred <- data.frame(reg.trend.seas.pred$mean, 
                                ma.trailing.res_2.pred$mean, ts.forecast.12)

names(total.reg.ma.pred) <- c("Regression Forecast", "Residuals Forecast",
                              "Combined Forecast")
total.reg.ma.pred

## c)
# Use accuracy() function to identify common accuracy measures.
# Use round() function to round accuracy measures to three decimal digits.
round(accuracy(snaive(sales.ts)$fitted, sales.ts), 3)
round(accuracy(reg.trend.seas.pred$fitted, sales.ts), 3)
round(accuracy(reg.trend.seas.pred$fitted+ma.trailing.res_2, sales.ts), 3)


## Q4
## a)
## CREATE DATA PARTITION.
## PLOT DATA PARTITION.

# Define the numbers of months in the training and validation sets,
# nTrain and nValid, respectively.
nValid <- 12
nTrain <- length(sales.ts) - nValid
train.ts <- window(sales.ts, start = c(2015, 1), end = c(2015, nTrain))
valid.ts <- window(sales.ts, start = c(2015, nTrain + 1), 
                   end = c(2015, nTrain + nValid))
train.ts
valid.ts


## b)
# Use ets() function with model = "ZZZ", i.e., automated selection of
# error, trend, and seasonality options.
# Use optimal alpha, beta, & gamma to fit HW over the training period.

