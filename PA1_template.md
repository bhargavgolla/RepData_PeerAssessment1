---
title: "Reproducible Research: Peer Assessment 1"
output:
  html_document:
    keep_md: true
---

# Reproducible Research: Peer Assessment 1

## Loading and preprocessing the data

We load data using `read.csv()` and convert the date column into dates.


```r
activity_data <- read.csv(unz('activity.zip', 'activity.csv'),
                          stringsAsFactor = FALSE)
# Convert Date to date types
activity_data[, 2] = as.Date(activity_data[, 2])
```

## What is mean total number of steps taken per day?

### Histogram of the total number of steps taken each day


```r
steps_by_days <- aggregate(steps ~ date, data = activity_data, sum,
                           na.rm = TRUE)

library(ggplot2)
qplot(steps, data = steps_by_days, geom = 'histogram',
    xlab = "Total Steps", ylab = "Number of days",
    main = "Total steps per day - Histogram", binwidth = 1000)
```

![plot of chunk histogram](figure/histogram-1.png) 

### Mean and median of total number of steps taken per day


```r
step_mean <- mean(steps_by_days$steps)
step_median <- median(steps_by_days$steps)
```
* The mean number of steps taken per day = 1.0766189 &times; 10<sup>4</sup>
* The median number of steps taken per day = 10765


## What is the average daily activity pattern?

### Time series plot of 5-minute interval and average number of steps taken


```r
steps_by_interval <- aggregate(steps ~ interval,
                               data = activity_data,
                               mean, na.rm = TRUE)
qplot(data = steps_by_interval, x = interval, y = steps,
    ylab = "Average steps", main = "Time series plot") + geom_line()
```

![plot of chunk timeseries](figure/timeseries-1.png) 

### 5-minute interval containing the maximum number of steps


```r
max_interval <- steps_by_interval[which.max(steps_by_interval$steps),]$interval
```
The 5 minute interval that contains the maximum number of steps is
835.



## Imputing missing values



## Are there differences in activity patterns between weekdays and weekends?
