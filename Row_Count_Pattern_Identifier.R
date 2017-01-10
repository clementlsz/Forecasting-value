#### set work directory ####
#setwd("C:/Users/clement.liu/Desktop/DV Project/forecasting-value")
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
target_table = "" # if target table is not specified, it will loop through all the tables
# setting the rolling back date period, 0 means all the days
days_back = 100
#NYMEX_OPTIONS
#WSI_WC_ISO_AGGR_WIND_FORE

if (target_table != "") {
    # working on a single table
    
    x = raw_data$TABLE_NAME == target_table
    target_data = raw_data[x,]
    
    # setting the rolling back date period
    # set the most recent number of days, 0 means for all days
    
    good_date = as.Date(target_data$DATE_COL,format = "%m/%d/%Y")
    
    if (days_back > 0)
    {
        recent_days = good_date >= max(good_date) - days_back
    } else
    {
        recent_days = good_date == good_date
    }
    
    daily_count = target_data$RESULT_SET[recent_days]
    date = good_date[recent_days]
    
    input_list = list(date, daily_count)
    names(input_list) = c("date", "daily_count")
    
    #### Analysis ####
    
    # identify whether a table displays constant row count pattern #
    const_para = constantness(input_list, hist_scat = TRUE)
    const_para = list(const_para)
    names(const_para) = target_table
    
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
    # finding outliers
    #boxplot.stats(daily_count)$out
    boxplot(daily_count, main = "Daily Count")
    hist(
        daily_count, main = "", fre = FALSE, col = "grey", lwd = 2, ylim = c(0,max(density(daily_count)$y))
    )
    lines(
        density(daily_count), main = "Daily Count", col = "blue", lwd = 2
    )
} else {
    # working on all tables
    j = 1
    
    for (i in levels(raw_data$TABLE_NAME)) {
        x = raw_data$TABLE_NAME == i
        target_data = raw_data[x,]
        
        # set the most recent number of days
        good_date = as.Date(target_data$DATE_COL,format = "%m/%d/%Y")
        
        if (days_back > 0)
        {
            recent_days = good_date >= max(good_date) - days_back
        } else
        {
            recent_days = good_date == good_date
        }
        
        daily_count = target_data$RESULT_SET[recent_days]
        date = good_date[recent_days]
        
        input_list = list(date, daily_count)
        names(input_list) = c("date", "daily_count")
        
        #### Analysis ####
        
        # identify whether a table displays constant row count pattern #
        const_para[[j]] = constantness(input_list)
        names(const_para)[j] = i
        
        j = j + 1
    }
}
const_para

