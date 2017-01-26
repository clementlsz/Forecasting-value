#### We are dividing the table into small intervals, find out the pattern in each interval and consolidate all the results to check the overall pattern ####
#### Our primary goal is to check whether the distribution is a flat line or not ####
#### spliting the data set into intervals ####
constantness = function (input_list, rollback_period = 7, hist_scat = FALSE, interval_plot = FALSE, show_stat = FALSE) {
    
    source("consistency_rate.R")
    is_constant = NA
    consistency_message = ""
    expected_constant_value = NA
    interval_num = NA
    
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
    
    consistency_min = min(interval_count_r[2,])
    consistency_max = max(interval_count_r[2,])
    consistency_mean = mean(interval_count_r[2,])
    consistency_std = sd(interval_count_r[2,])
    consistency_median = median(interval_count_r[2,])
    consistency_skewness = skewness(interval_count_r[2,])

    # Making Decisions #
     if (is.nan(consistency_skewness)) {
         # There is an unique consistency rate
         if (consistency_mean == 1) {
             is_constant = TRUE
         } else if (consistency_mean > 0.5) {
             is_constant = NA
             consistency_message = paste(consistency_mean * 100,"% of consistency, undetermined", sep = "")
         } else {
             is_constant = FALSE
         }
     } else {
         # There is more than one consistency rate
         if (consistency_skewness > 0) {
             if (consistency_min > 0.5 | (consistency_mean > 0.5 & consistency_median > 0.5)) {
                 is_constant = NA
                 consistency_message = "undetermined"
             } else {
                 is_constant = FALSE
             } 
         } else if (-0.1 < consistency_skewness && consistency_skewness < 0) {
                 is_constant = NA
                 consistency_message = "Table is likely to have a constantness pattern"
         } else {
                 if (consistency_max == 1) {
                     is_constant = TRUE
                 } else {
                     is_constant = NA
                     consistency_message = "Table is likely to have a constantness pattern"
                 }
             }
     }
    
    # Calculating the expected row count #
        if (isTRUE(is_constant) | is.na(is_constant)) {
            #print("in")
            # find the most recent repeated value
            for (n in 1:dim(interval_count_r)[2]) {
                if (interval_count_r[2,n] > 0.5) {
                    expected_constant_value = interval_count_r[[1,n]]
                    interval_num = n
                    break
                }
            }
        }
        #print(expected_constant_value)

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
        splitted_dataset = data.frame(date, daily_count, interval)
        windows()
        print(xyplot(daily_count ~ date | interval, data = splitted_dataset, pch = 1, xlab = "Date", ylab = "Count"))
    }
    
    stat = c(consistency_min, consistency_max, consistency_mean, consistency_std, consistency_median, consistency_skewness)
    names(stat) = c("min", "max", "mean", "std", "median", "skewness")
    if (isTRUE(show_stat)){
        print(stat)
    }
    
    result = c(expected_constant_value, interval_num, is_constant,consistency_message)
    names(result) = c("expected constant value","interval num","is constant?", "consistency message")
    return(result)
}
