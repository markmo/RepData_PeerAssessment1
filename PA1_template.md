# Reproducible Research: Peer Assessment 1




## Loading and preprocessing the data

Read the file and convert the date column to a Date class.


```r
activity <- read.csv("activity.csv", header = T, nrows = 17568, colClasses = c("numeric", 
    "character", "numeric"), na.strings = "NA")
data <- activity[complete.cases(activity), ]
data$date <- as.Date(data$date, format = "%Y-%m-%d")
steps.by.day <- with(data, aggregate(steps ~ date, data, sum))
avg.steps.by.interval <- with(data, aggregate(steps ~ interval, data, mean))
```


## What is mean total number of steps taken per day?


```r
steps.mean <- mean(steps.by.day$steps)
steps.median <- median(steps.by.day$steps)
with(steps.by.day, hist(steps, main = "Histogram of steps taken per day"))
```

![plot of chunk histogram_steps_per_day](figure/histogram_steps_per_day.png) 


The mean of the total number of steps taken per day is 1.0766 &times; 10<sup>4</sup>, and the median is 1.0765 &times; 10<sup>4</sup>.

## What is the average daily activity pattern?


```r
interval.with.max.avg.steps <- avg.steps.by.interval[avg.steps.by.interval$steps == 
    max(avg.steps.by.interval$steps), ]$interval
with(avg.steps.by.interval, plot(interval, steps, type = "l"))
```

![plot of chunk average_daily_activity](figure/average_daily_activity.png) 


The 5-minute interval which, on average across all the days in the dataset, contains the maximum number of steps is interval: 835.

## Imputing missing values


```r
na.count <- length(!complete.cases(activity))
activity.complete <- activity
ind <- match(activity.complete[is.na(activity.complete$steps), ]$interval, avg.steps.by.interval$interval)
activity.complete[is.na(activity.complete$steps), ]$steps <- avg.steps.by.interval$steps[ind]
steps.by.day.complete <- with(activity.complete, aggregate(steps ~ date, activity.complete, 
    sum))
steps.mean.complete <- mean(steps.by.day$steps)
steps.median.complete <- median(steps.by.day$steps)
with(steps.by.day, hist(steps, main = "Histogram of steps taken per day"))
```

![plot of chunk hist_steps_per_day_complete](figure/hist_steps_per_day_complete.png) 


The mean of the total number of steps taken per day is 1.0766 &times; 10<sup>4</sup>, and the median is 1.0765 &times; 10<sup>4</sup>.


## Are there differences in activity patterns between weekdays and weekends?


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

