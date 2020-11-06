## USE FORECAST LIBRARY.

install.packages("forecast")
library(forecast)

##Q1:
## CREATE DATA FRAME. 
#Grocery.data <- read.csv("673_case1.csv")
Grocery.data <- read.csv(file.choose(), header = TRUE)
# See the first 6 records of the file.
head(Grocery.data)

## USE ts() FUNCTION TO CREATE TIME SERIES DATA SET.
sales.ts <- ts(Grocery.data$Sales, 
                   start = c(2015, 1), end = c(2019, 12), freq = 12)


sales.ts

##Q2:
## Use plot() to plot time series data  
plot(sales.ts, 
     xlab = "Time", ylab = "Sales", 
     ylim = c(100, 500), main = "Grocery Store Sales", col = "blue")

# Use stl() function to plot times series components of the original data. 
# The plot includes original data, trend, seasonal, and reminder 
# (level and noise component).
sales.stl <- stl(sales.ts, s.window = "periodic")
sales.stl
autoplot(sales.stl, main = "Gorcery Store Time Series Components")


##Q3:
# Use Acf() function to identify autocorrelation and plot autocorrelation
# for different lags (up to maximum of 12).
autocor <- Acf(sales.ts, lag.max = 12, main = "Autocorrelation for Amtrak Ridership")

# Display autocorrelation coefficients for various lags.
Lag <- round(autocor$lag, 0)
ACF <- round(autocor$acf, 3)
data.frame(Lag, ACF)