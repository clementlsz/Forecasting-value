library(lattice)
library(datasets)
library(moments)
require('xts')

setwd("C:/Users/clement.liu/Desktop/DV Project/forecasting-value")
#setwd("C:/Users/User/Desktop/DV")
rm(list = ls())
days_back = 1000
raw_data = read.csv("dv test.csv", header = TRUE)
target_table = "MRTU_PRC_LMP_RUC"
#NYMEX_OPTIONS
#MRTU_PRC_LMP_RUC
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
ts1 = ts(daily_count, start = c(2009,4), frequency = 365)
windows()
plot(decompose(ts1))
#ts1
ts1.hw = HoltWinters(ts1)
windows()
plot(ts1.hw)

cycle(ts1)
