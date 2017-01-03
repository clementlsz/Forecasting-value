library(lattice)
library(datasets)
# Reading the data
data_set = read.csv("dv test.csv", header = TRUE)

# isoldate the value with the desire table name
target_table = "NOAA_WATER_SUPPLY_FORECAST"
x = data_set$TABLE_NAME == target_table
target_set = data_set[x,]

tail(target_set,100)

daily_count = target_set$RESULT_SET
date = as.Date(target_set$DATE_COL,format = "%m/%d/%Y")

date
summary(as.factor(daily_count))

#ploting graphs
windows()
xyplot(daily_count ~ date, data = target_set, pch = 1, xlab = "Date", ylab = "Count")
windows()
hist(daily_count)



