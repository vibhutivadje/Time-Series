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



