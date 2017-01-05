#### Notes ####
#### system reads Date_Col as character, need to convert to date first, Otherwise plotting is incorrect ####
#windows()
#xyplot(tail(target_set$RESULT_SET,1000) ~ tail(target_set$DATE_COL,1000), data = target_set, pch = 1, xlab = "Date", ylab = "Count")
#sort(as.Date(target_set$DATE_COL,format = "%m/%d/%Y"))

#### code ####

library(lattice)
library(datasets)
#library(moments)

#### Reading the data ####

raw_data = read.csv("dv test.csv", header = TRUE)

#### isoldate the values with table name and date ####

target_table = "NOAA_WATER_SUPPLY_FORECAST"
x = raw_data$TABLE_NAME == target_table
target_set = raw_data[x,]

# setting the rolling back date period
# set the most recent number of days, 0 means for all days
days_back = 300
good_date = as.Date(target_set$DATE_COL,format = "%m/%d/%Y")

if (days_back > 0)
{
    recent_days = good_date >= max(good_date) - days_back
} else 
{
    recent_days = good_date == good_date
}

daily_count = target_set$RESULT_SET[recent_days]
date = good_date[recent_days]


#### Analysis ####

# kurtosis(daily_count)
# create index vector, for split
index = length(daily_count) : 1
# set it to every 7 data points, program runs once per week
rollback_period = 30
interval = ceiling(index/rollback_period)
split(daily_count,interval)
split(date,interval)


#### ploting graphs ####

## plot divided into intervals ##
temp_dataset = data.frame(date, daily_count, interval = factor(interval))
windows()
xyplot(daily_count ~ date | interval, data = temp_dataset, pch = 1, xlab = "Date", ylab = "Count")
## plotting chart ##
windows()
plot(date, daily_count, col = "blue", pch = 20)
## plotting linear line ##
fit = lm(daily_count ~ date)
abline(fit, col = "red")
## boxplot ##
windows()
boxplot.stats(daily_count)$out
boxplot(daily_count)
## histogram ##
windows()
hist(daily_count)

