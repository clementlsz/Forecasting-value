#### Notes ####
#### system reads Date_Col as character, need to convert to date first, Otherwise plotting is incorrect ####
#windows()
#xyplot(tail(target_set$RESULT_SET,1000) ~ tail(target_set$DATE_COL,1000), data = target_set, pch = 1, xlab = "Date", ylab = "Count")
#sort(as.Date(target_set$DATE_COL,format = "%m/%d/%Y"))

## work directory ##
#setwd("C:/Users/clement.liu/Desktop/DV Project/forecasting-value")

#### code ####

library(lattice)
library(datasets)

source("consistency_rate.R")


#### Reading the data ####

raw_data = read.csv("dv test.csv", header = TRUE)


#### isoldate the values with table name and date ####

target_table = "NOAA_WATER_SUPPLY_FORECAST"
x = raw_data$TABLE_NAME == target_table
target_set = raw_data[x,]

# setting the rolling back date period
# set the most recent number of days, 0 means for all days
days_back = 0
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

## We are dividing the table into small intervals, find out the pattern in each interval and consolidate all the results to check the overall pattern ##
## Our primary goal is to check whether the distribution is a flat line or not ##

# spliting the data set into intervals #
# create index vector, for splitting
index = length(daily_count) : 1
# set it to every 7 data points, program runs once per week
rollback_period = 7
interval = factor(ceiling(index/rollback_period))
# finding the consistency rate for daily row count in each interval
daily_count_s = split(daily_count,interval)
daily_count_l = sapply(daily_count_s, consistency_rate)
# finding the middle date for each interval
date_s = split(date,interval)
date_l = as.Date(sapply(date_s, median),origin = "1970-01-01")

#### ploting graphs for daily row count ####

# plotting count vs time #
windows()
plot(date, daily_count, col = "blue", pch = 20, main = "Daily Count")
# plotting linear line #
fit = lm(daily_count ~ date)
abline(fit, col = "red")

# boxplot and histogram #
windows()
par(mfrow = c(2,1))
boxplot.stats(daily_count)$out
boxplot(daily_count, main = "Daily Count")
hist(daily_count, main="", prob = TRUE, col = "grey", lwd = 2)
lines(density(daily_count), main = "Daily Count", col = "blue", lwd = 2)

#### ploting graphs for interval analysis ###

# plot divided into intervals #
#splitted_dataset = data.frame(date, daily_count, interval)
#windows()
#xyplot(daily_count ~ date | interval, data = splitted_dataset, pch = 1, xlab = "Date", ylab = "Count")

# histogram and scatter#
windows()
par(mfrow = c(2,1))
hist(daily_count_l[2,], main = "Interval Row Count", fre = FALSE, col = "grey", lwd = 2, xlab = "Consistency Rate")
lines(density(daily_count_l[2,]), col = "blue", lwd = 2)

plot(date_l, daily_count_l[2,], col = "blue", pch = 20, ylab = "Consistency Rate", xlab = "date")
