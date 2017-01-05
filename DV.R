#### Notes ####
#### system reads Date_Col as character, need to convert to date first, Otherwise plotting is incorrect ####
#windows()
#xyplot(tail(target_set$RESULT_SET,1000) ~ tail(target_set$DATE_COL,1000), data = target_set, pch = 1, xlab = "Date", ylab = "Count")
#sort(as.Date(target_set$DATE_COL,format = "%m/%d/%Y"))

#### code ####
library(lattice)
library(datasets)
library(moments)

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

#### ploting graphs ####
windows()
# xyplot(daily_count ~ date, data = target_set, pch = 1, xlab = "Date", ylab = "Count")

## ploting chart ##
plot(date, daily_count, col = "blue", pch = 20)
## ploting linear line ##
fit = lm(daily_count ~ date)
abline(fit, col = "red")
## boxplot ##
windows()
boxplot.stats(daily_count)$out
boxplot(daily_count)
## histogram ##
windows()
hist(daily_count)


#### Analysis ####
#kurtosis(daily_count)
rollback_period = 7 #set it to every 7 data points, program runs once per week
df = data.frame(date, daily_count)