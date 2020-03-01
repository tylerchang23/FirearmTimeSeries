# STA 137- FINAL PROJECT
# TYLER CHANG

library(astsa)

setwd('~/Downloads')
data = read.delim('GD.dat.txt', header = FALSE, sep = "")

names(data)[1] = "sales"
names(data)[2] = "deaths"

# Initial Data Inspection
ncol(data)
nrow(data)
summary(data)

# Time series plot: Raw Data
par(mfrow=2:1)
tsplot(data$sales, main="Sales", ylab = 'Sales')
tsplot(data$deaths, main="Deaths", ylab = 'Deaths')

# Differencing to eliminate primary stationarity
par(mfrow=2:1)
tsplot(diff(data$sales), main="Sales", ylab = 'Sales')
tsplot(diff(data$deaths), main="Deaths", ylab = 'Deaths')

# Differencing to eliminate seasonality
par(mfrow=2:1)
tsplot(diff(diff(data$sales,12)), main="Sales", ylab = 'Sales')
tsplot(diff(diff(data$deaths,12)), main="Deaths", ylab = 'Deaths')

# Determine model for SALES
sales_acf = acf2(diff(diff(data$sales,12)), max.lag = 73)

sales_model = sarima(data$sales, p = 0, d = 1, q =2, P = 3, D = 1, Q = 0, S = 12)

# Determine model for DEATHS
deaths_acf = acf2(diff(diff(data$deaths,12)), max.lag = 73)

deaths_model = sarima(data$deaths, p = 0, d = 1, q =1, P = 0, D = 1, Q = 1, S = 12)

sarima(data$deaths, p = 0, d = 1, q =1, P = 0, D = 1, Q = 1, S = 12)

# CCF
par(mfrow=c(1,1))
ccf2(data$sales, data$deaths, max.lag = 100, main = "CCF of Sales and Deaths")

# Regression
reg = lm(data$deaths~data$sales, data = data) 
summary(reg)

# Time series plot of residual
tsplot(reg$residuals, main = "Time Series of OLS Residuals", ylab = "Residuals")

# Differencing to make residuals stationary.
tsplot(diff(diff(reg$residuals,12)), main = "Time Series of OLS Residuals After Differencing", ylab = "Residuals")


# Differencing to remove trend & seasonality
acf2(diff(diff(reg$residuals,12)),  max.lag = 61)

# ACF looks like this SARIMA model
sarima(reg$residuals, p=0, d=1, q=1, P=2, D=1, Q=0, S=12)

# Find new linear model coefficient (xreg)
sarima(data$deaths, p=0, d=1, q=1, P=2, D=1, Q=0, S=12, xreg = data$sales)
