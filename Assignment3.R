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

valid.df <- data.frame(valid.ts, train.quad.trend.season.pred$mean, 
                       res.ar1.pred$mean, valid.two.level.pred)
names(valid.df) <- c("Revenue data", "Reg.Forecast", 
                     "AR(1)Forecast", "Combined.Forecast")
valid.df

##e)
## Develop a two-level forecast (regression model 
#with quadratic  trend and seasonality and AR(1) model for residuals)

# Use tslm() function to create quadratic trend and seasonality model.
trend.season <- tslm(revenue.ts ~ trend + I(trend^2) + season)

# See summary of linear trend equation and asociated parameters.
summary(trend.season)

# Apply forecast() function to make predictions with quadratic trend and seasonal 
# model into the future 12 months.  
qua.trend.season.pred <- forecast(trend.season, h = 4, level = 0)
qua.trend.season.pred
qud.residuals.ar1 <- Arima(trend.season$residuals, order = c(1,0,0))
summary(qud.residuals.ar1 )

qud.residuals.ar1.pred <- forecast(qud.residuals.ar1, h = 4, level = 0)
qud.residuals.ar1.pred

Acf(qud.residuals.ar1$residuals, lag.max = 8,
    main = "Autocorrelation for Residuals of Walmart revenue Data")

two.level.pred <- qua.trend.season.pred$mean +qud.residuals.ar1.pred$mean

Total.df <- data.frame(qua.trend.season.pred$mean, 
                       qud.residuals.ar1.pred$mean, two.level.pred)
names(Total.df) <- c("Reg.Forecast", 
                     "AR(1)Forecast", "Combined.Forecast")
Total.df

#*****************************************Q3:****************************************************#
##a)
## Arima(1,1,1)(1,1,1) model for training data set.
## summary report
train.arma1.seas <- Arima(train.ts, order = c(1,1,1), seasonal = c(1,1,1))
summary(train.arma1.seas)

# Apply forecast() function to make predictions for ts with 
# ARIMA model in validation set.    
train.arima1.seas.pred <- forecast(train.arma1.seas, h = nValid, level = 0)

##b)
## FIT AUTO ARIMA MODEL.
# Use auto.arima() function to fit ARIMA model.
# Use summary() to show auto ARIMA model and its parameters.
train.auto.arima <- auto.arima(train.ts)
summary(train.auto.arima)

# Apply forecast() function to make predictions for ts with 
# auto ARIMA model in validation set.  
train.auto.arima.pred <- forecast(train.auto.arima, h = nValid, level = 0)
train.auto.arima.pred

##c)
# Use accuracy() function to identify common accuracy measures 
# for validation period forecast:

round(accuracy(train.arima1.seas.pred, valid.ts), 3)
round(accuracy(train.auto.arima.pred, valid.ts), 3)

##d)
## Arima(1,1,1)(1,1,1) model for entire revenue data set.
## summary report
arma1.seas <- Arima(revenue.ts, order = c(1,1,1), seasonal = c(1,1,1))
summary(arma1.seas)

# Apply forecast() function to make predictions for ts with 
# seasonal ARIMA model for the future 12 periods. 
arima1.seas.pred <- forecast(arma1.seas, h = 4, level = 0)
arima1.seas.pred


## FIT AUTO ARIMA MODELS FOR ENTIRE DATA SET. 

# Use auto.arima() function to fit ARIMA model for entire data set.
# use summary() to show auto ARIMA model and its parameters for entire data set.
auto.arima <- auto.arima(revenue.ts)
summary(auto.arima)

# Apply forecast() function to make predictions for ts with 
# auto ARIMA model for the future 12 periods. 
auto.arima.pred <- forecast(revenue.ts, h = 4, level = 0)
auto.arima.pred


# MEASURE FORECAST ACCURACY FOR ENTIRE DATA SET.

# Use accuracy() function to identify common accuracy measures for:
# (1) regression model with quadratic trend and seasonality
# (2) two level model with (with AR(1) model for residuals)
# (1) Seasonal ARIMA (1,1,1)(1,1,1) Model,
# (1) Auto ARIMA Model,
# (2) Seasonal naive forecast, and
# (3) Naive forecast.
round(accuracy(arima.seas.pred$fitted, ridership.ts), 3)
round(accuracy(auto.arima.pred$fitted, ridership.ts), 3)
round(accuracy((snaive(ridership.ts))$fitted, ridership.ts), 3)
round(accuracy((naive(ridership.ts))$fitted, ridership.ts), 3)