#install.packages(c("tseries","zoo","forecast","FinTS","rugarch"))
#install.packages("multicool")
library(tseries)
library(zoo)
library(forecast)
library(FinTS)
library(rugarch)
library(lattice)
library(datasets)
library(moments)
require('xts')

setwd("C:/Users/clement.liu/Desktop/DV Project/forecasting-value")
#setwd("C:/Users/User/Desktop/DV")
rm(list = ls())
days_back = 365
raw_data = read.csv("dv test.csv", header = TRUE)
target_table = "PJM_ES_RT_DAILY_ENERGY_TXN"
#NYMEX_OPTIONS
#MRTU_PRC_LMP_RUC
#PJM_EDF_INSTANT_LOAD_ZONE
#PJM_ES_RT_DAILY_ENERGY_TXN

x = raw_data$TABLE_NAME == target_table
target_data = raw_data[x,]

good_date = as.Date(target_data$DATE_COL,format = "%m/%d/%Y")

if (days_back > 0)
{
    date = tail(good_date, days_back)
    daily_count = tail(target_data$RESULT_SET, days_back)
} else
{
    date = good_date
    daily_count = target_data$RESULT_SET
}

#plot(date, daily_count, col = "black", type = 1, main = "Daily Count")
# head(date)
#ts = xts(daily_count, date)
daily_count_ts = ts(daily_count, frequency = 7)
#daily_count_ts_root = ts(sqrt(daily_count), frequency = 7)

daily_count_d = diff(daily_count_ts)
fit1 = auto.arima(daily_count_ts, trace = TRUE, test = "kpss", ic = "aic")
Box.test(fit1$residuals^2,lag = 7, type = "Ljung-Box")

p = fit1$arma[1]
q = fit1$arma[2]
P = fit1$arma[3]
Q = fit1$arma[4]
seasonal = fit1$arma[5]
d = fit1$arma[6]
D = fit1$arma[7]



help(auto.arima)
# a=adf.test(daily_count_ts_d, alternative = "stationary")
# a

plot(decompose(daily_count_ts))


decompose(daily_count_ts_root)$trend + decompose(daily_count_ts_root)$seasonal + decompose(daily_count_ts_root)$random
acf(decompose(daily_count_ts_root)$random, na.action = na.pass)

daily_count_ts_diff = diff(daily_count_ts,7)
temp = arima(daily_count_ts_diff,order = c(1,0,7))
temp

pacf(daily_count_ts_diff)

acf(daily_count_d)


# cycle(daily_count_ts)
# end(daily_count_ts)
# # windows()
#  acf(diff(daily_count_ts))
#  pacf(daily_count_ts)
# # #daily_count_ts
#  daily_count_ts.hw = HoltWinters(daily_count_ts)
#  windows()
#  plot(daily_count_ts.hw)
#  daily_count_ts_predict = predict(daily_count_ts.hw, n =7*10, prediction.interval = FALSE)
#  ts.plot(daily_count_ts,daily_count_ts_predict, lty = 1:4)
#  
help(ugarchspec)
