# Reproducible Research: Peer Assessment 1




## Loading and preprocessing the data

Read the file and convert the date column to a Date class. Convert the interval column to a time class to display in a more readable 24-hour time format.


```r
activity <- read.csv("activity.csv", header = T, nrows = 17569, colClasses = c("numeric", 
    "character", "numeric"), na.strings = "NA")
activity$date <- as.Date(activity$date, format = "%Y-%m-%d")
interval.as.time <- strptime(formatC(activity$interval, width = 4, format = "d", 
    flag = "0"), "%H%M")
activity <- cbind(activity, interval.as.time)
```


Aggregate the number of steps by day, and the average number of steps taken by time interval. The aggregate function ignores missing values, which have been tagged as "NA".


```r
steps.by.day <- with(activity, aggregate(steps ~ date, activity, sum))
avg.steps.by.interval <- with(activity, aggregate(steps ~ interval.as.time, 
    activity, mean))
```


## What is mean total number of steps taken per day?

The following produces a histogram of the total number of steps taken each day.


```r
with(steps.by.day, hist(steps, breaks = 30, xlab = "Number of steps", main = "Histogram of steps taken per day"))
```

![plot of chunk histogram_steps_per_day](figure/histogram_steps_per_day.png) 



```r
steps.mean <- round(mean(steps.by.day$steps))
steps.median <- round(median(steps.by.day$steps))
```


The mean of the total number of steps taken per day (rounded to the nearest whole number) is 10,766, and the median is 10,765.

## What is the average daily activity pattern?

The following produces a time series plot by interval of the average number of steps taken across all days.


```r
with(avg.steps.by.interval, plot(interval.as.time, steps, type = "l", xlab = "Interval", 
    ylab = "Average steps"))
```

![plot of chunk average_daily_activity](figure/average_daily_activity.png) 

```r
interval.with.max.avg.steps <- format(avg.steps.by.interval[avg.steps.by.interval$steps == 
    max(avg.steps.by.interval$steps), ]$interval.as.time, "%H:%M")
```


The 5-minute interval, which contains the maximum number of steps on average across all the days in the data set, is: 08:35.

## Imputing missing values


```r
na.count <- sum(!complete.cases(activity))
ind <- match(activity[is.na(activity$steps), ]$interval.as.time, avg.steps.by.interval$interval.as.time)
activity.complete <- activity
activity.complete[is.na(activity$steps), ]$steps <- avg.steps.by.interval$steps[ind]
steps.by.day.complete <- with(activity.complete, aggregate(steps ~ date, activity.complete, 
    sum))
```


There are 2304 missing values (entries with no recorded steps for the day and interval) in the activity data set.

Missing values in the data set will be imputed by assigning the mean of the corresponding interval to create a new data set "activity.complete". The relevant summaries will be recompiled from this new data set.

The following produces a histogram of the total number of steps taken each day using imputed values for missing events.


```r
with(steps.by.day.complete, hist(steps, breaks = 30, xlab = "Number of steps", 
    main = "Histogram of steps taken per day"))
```

![plot of chunk hist_steps_per_day_complete](figure/hist_steps_per_day_complete.png) 

```r
steps.mean.complete <- round(mean(steps.by.day.complete$steps))
steps.median.complete <- round(median(steps.by.day.complete$steps))
```


The mean of the total number of steps taken per day is 10,766, and the median is 10,766. The mean is the same as that calculated from the earlier data set because the missing values have simply been replaced by the earlier mean. The median is slightly different.

The impact of imputing missing data on the estimates of the total daily number of steps has not been significant given the distribution of data.

## Are there differences in activity patterns between weekdays and weekends?

The following code splits the data by occurrence on weekdays vs weekends to determine any difference in activity pattern.


```r
activity.complete$date <- as.Date(activity.complete$date, format = "%Y-%m-%d")
day.of.week <- weekdays(activity.complete$date)
weekends <- day.of.week %in% c("Saturday", "Sunday")
activity.complete$time.of.week[weekends] <- "weekend"
activity.complete$time.of.week[!weekends] <- "weekday"
activity.complete$time.of.week <- as.factor(activity.complete$time.of.week)
avg.steps <- with(activity.complete, aggregate(steps ~ interval + time.of.week, 
    activity.complete, mean))
with(avg.steps, xyplot(steps ~ interval | time.of.week, layout = c(1, 2), type = "l", 
    ylab = "Number of steps"))
```

![plot of chunk average_activity_time_of_week](figure/average_activity_time_of_week.png) 


The plot shows a difference with a higher peak of activity in the morning during weekdays, compared to weekends, which have greater mean activity overall, more evenly spread across the daytime hours.
