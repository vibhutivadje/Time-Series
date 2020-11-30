## LOAD LIBRARIES AND INITIAL DATA INPUT

# Use forecast and zoo libraries
library(forecast)
library(zoo)

##***************************************From Case2****************************************************##
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

#### CREATE DATA PARTITION.
#Data Partiton with VAlidation partition of 16 periods and training partition of 46 periods
nValid <- 16
nTrain <- length(revenue.ts) - nValid
train.ts <- window(revenue.ts, start = c(2005, 1), end = c(2005, nTrain))
valid.ts <- window(revenue.ts, start = c(2005, nTrain + 1), 
                   end = c(2005, nTrain + nValid))


#**************************************************Q1:*************************************************************#
##a)
# AR(1) model for regression residulas.
revenue.ts.ar1<- Arima(revenue.ts, order = c(1,0,0))
summary(revenue.ts.ar1)

revenue.ts.ar1.pred <- forecast(revenue.ts.ar1, h = 12, level = 0)
revenue.ts.ar1.pred

##b)
# Create differenced Walmart data using (lag-1)
diff.revenue.ts <- diff(revenue.ts, lag = 1)
diff.revenue.ts

#plot autocorrrelation for different lags (up to maximum of 8).
Acf(diff.revenue.ts, lag.max = 8, 
    main = "Autocorrelation for Walmart revenue")

#**************************************************Q2:*************************************************************#
## Two-level forecast with regression model and AR model for residuals
##a) Regression model with quadratic trend and seasonality

# create Quadratic trend and seasonal model.
train.quad.trend.season <- tslm(train.ts ~ trend  +I(trend^2)+ season)

# See summary of quadratic trend and seasonality model and asociated parameters.
summary(train.quad.trend.season)

# Apply forecast() function to make predictions for ts with 
# trend and seasonality data in validation set.  
train.quad.trend.season.pred <- forecast(train.quad.trend.season, h = nValid, level = 0)
train.quad.trend.season.pred

##b)
# identify autocorrealtion for the model residuals (training sets), and plot autocorrrelation for different 
# lags (up to maximum of 8).
Acf(train.quad.trend.season.pred$residuals, lag.max = 8, main = "Autocorrelation for regression models Residuals")

##c)
##AR(1) model for the regression residuals 
res.ar1 <- Arima(train.quad.trend.season.pred$residuals, order = c(1,0,0))
summary(res.ar1)
Acf(res.ar1$residuals, lag.max = 8, 
    main = "Autocorrelation for AR1 Residuals Walmart revenue Data")

res.ar1.pred <-  forecast(res.ar1, h = nValid, level = 0)
res.ar1.pred

##d)
##two-level forecasting model (regression model with quadratic trend and seasonality + 
##AR(1) model for residuals) for the validation period
valid.two.level.pred <- train.quad.trend.season.pred$mean + res.ar1.pred$mean

valid.df <- data.frame(valid.ts, train.trend.season.pred$mean, 
                       res.ar1.pred$mean, valid.two.level.pred)
names(valid.df) <- c("Ridership", "Reg.Forecast", 
                     "AR(1)Forecast", "Combined.Forecast")
valid.df
