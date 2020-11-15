## USE FORECAST LIBRARY.

install.packages("forecast")
library(forecast)
library(zoo)
##Q1:
##a)
## CREATE DATA FRAME. 
walmart.data <- read.csv(file.choose(), header = TRUE)
# See the first 6 records of the file.
head(walmart.data)

## USE ts() FUNCTION TO CREATE TIME SERIES DATA SET.
revenue.ts <- ts(walmart.data$Revenue, 
               start = c(2005, 1), end = c(2020, 2), freq = 4)

revenue.ts