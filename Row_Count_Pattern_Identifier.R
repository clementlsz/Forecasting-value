#### set work directory ####
setwd("C:/Users/clement.liu/Desktop/DV Project/forecasting-value")
#setwd("C:/Users/User/Desktop/DV")
rm(list=ls())

#### code ####

library(lattice)
library(datasets)
library(moments)

source("constantness.R")

#### Reading the data ####
raw_data = read.csv("dv test.csv", header = TRUE)

#### create control variables ####
const_para = list()
# if target table is not specified, it will loop through all the tables
target_table = "WSI_WC_ISO_AGGR_WIND_FORE" 
#NYMEX_OPTIONS
#WSI_WC_ISO_AGGR_WIND_FORE
#PJM_EDF_INSTANT_LOAD_ZONE

# setting the rolling back date period, 0 means all the days
days_back = 365

if (target_table != "") {
#### working on a single table ####
    
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
    
    input_list = list(date, daily_count)
    names(input_list) = c("date", "daily_count")
    
# Analysis #
    
    # identify whether a table displays constant row count pattern #
    const_para = constantness(input_list, hist_scat = FALSE, interval_plot = FALSE,show_stat = FALSE)
    const_para = list(const_para)
    names(const_para) = target_table
    
# ploting graphs for daily row count #
    
    # plotting count vs time
    windows()
    plot(date, daily_count, col = "blue", pch = 20, main = "Daily Count")
    # plotting linear line
    fit = lm(daily_count ~ date)
    abline(fit, col = "red")
    # plot the expected daily constant row count
    #abline(h = const_para[[1]][["expected constant value"]], col = "green")
    
    # boxplot and histogram
    windows()
    par(mfrow = c(2,1))
    boxplot(daily_count, main = "Daily Count")
    hist(
        daily_count, main = "", fre = FALSE, col = "grey", lwd = 2, ylim = c(0,max(density(daily_count)$y))
    )
    lines(
        density(daily_count), main = "Daily Count", col = "blue", lwd = 2
    )
} else {
#### working on all tables ####
    j = 1
    
    for (i in levels(raw_data$TABLE_NAME)) {
        x = raw_data$TABLE_NAME == i
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
        
        input_list = list(date, daily_count)
        names(input_list) = c("date", "daily_count")
        
# Analysis #
        
        # identify whether a table displays constant row count pattern #
        const_para[[j]] = constantness(input_list)
        names(const_para)[j] = i
        
        j = j + 1
        
        windows()
        plot(date, daily_count, col = "blue", pch = 20, main = i)
        abline(h = const_para[[i]][["expected constant value"]], col = "green")
    }
}
const_para

