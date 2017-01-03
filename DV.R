#### Notes ####
#### system reads Date_Col as character, need to convert to date first, Otherwise plotting is incorrect ####
#windows()
#xyplot(tail(target_set$RESULT_SET,1000) ~ tail(target_set$DATE_COL,1000), data = target_set, pch = 1, xlab = "Date", ylab = "Count")
#sort(as.Date(target_set$DATE_COL,format = "%m/%d/%Y"))

#### code ####
library(lattice)
library(datasets)
# Reading the data
raw_data = read.csv("dv test.csv", header = TRUE)

# isoldate the value with the desire table name
target_table = "NOAA_WATER_SUPPLY_FORECAST"
x = raw_data$TABLE_NAME == target_table
target_set = raw_data[x,]

# setting the rolling back date period
# set the most recent number of days, 0 means for all days
days_back = 160
good_date = as.Date(target_set$DATE_COL,format = "%m/%d/%Y")

if (days_back > 0)
{
    rollback_period = good_date >= max(good_date) - days_back
} else
{
    rollback_period = good_date == good_date
}

#rollback_period

daily_count = target_set$RESULT_SET[rollback_period]
date = good_date[rollback_period]

#ploting graphs
windows()
xyplot(daily_count ~ date, data = target_set, pch = 1, xlab = "Date", ylab = "Count")
windows()
hist(daily_count)