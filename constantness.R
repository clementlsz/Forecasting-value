#### We are dividing the table into small intervals, find out the pattern in each interval and consolidate all the results to check the overall pattern ####
#### Our primary goal is to check whether the distribution is a flat line or not ####
#### spliting the data set into intervals ####
constantness = function (input_list, rollback_period = 7, hist_scat = FALSE, interval_plot = FALSE) {
    source("consistency_rate.R")
    constant_value = FALSE
    message = ""
    
    daily_count = input_list$daily_count
    date = input_list$date
    # create index vector, for splitting
    index = length(daily_count):1
    # set it to every 7 data points, program runs once per week
    interval = factor(ceiling(index / rollback_period))
    # finding the consistency rate for daily row count in each interval
    daily_count_s = split(daily_count,interval)
    interval_count_r = sapply(daily_count_s, consistency_rate)
    # finding the middle date for each interval
    date_s = split(date,interval)
    interval_date_r = as.Date(sapply(date_s, median),origin = "1970-01-01")
    
    consistency_mean = mean(interval_count_r[2,])
    consistency_std = sd(interval_count_r[2,])
    consistency_median = median(interval_count_r[2,])
    consistency_skewness = skewness(interval_count_r[2,])

    # Making Decisions #
     if (is.nan(consistency_skewness)) {
         if (consistency_mean == 1) {
             constant_value = TRUE
         } else if (consistency_mean > 0.5) {
             constant_value = TRUE
             message = paste(consistency_mean * 100,"% of consistency, please check source", sep = "")
         } else {
             constant_value = FALSE
         }
     }

#### ploting graphs for interval analysis ###
    
    # histogram and scatter #
    if (isTRUE(hist_scat)) {
        windows()
        par(mfrow = c(2,1))
        hist(
            interval_count_r[2,], main = "Interval Rate", fre = FALSE, col = "grey", lwd = 2, xlab = "Consistency Rate"
        )
        lines(density(interval_count_r[2,]), col = "blue", lwd = 2)
        
        plot(
            interval_date_r, interval_count_r[2,], col = "blue", pch = 20, ylab = "Consistency Rate", xlab = "date"
        )
    }
    
    # plot divided into intervals #
    if (isTRUE(interval_plot)) {
        #splitted_dataset = data.frame(date, daily_count, interval)
        #windows()
        #xyplot(daily_count ~ date | interval, data = splitted_dataset, pch = 1, xlab = "Date", ylab = "Count")
    }
    
    result = c(consistency_mean, consistency_std, consistency_median, consistency_skewness)
    names(result) = c("mean","std", "median", "skewness")
    return(result)
}
