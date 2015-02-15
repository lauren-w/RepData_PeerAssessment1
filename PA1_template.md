---
title: "Reproducible Research: Peer Assessment 1"
output: html_document
---

##Loading and preprocessing the data

```r
setwd("C:/Users/laurena/Desktop/Learning Materials/Reproducible Research/repdata_data_activity")
activity <- read.csv("activity.csv")
head(activity)
```

```
##   steps       date interval
## 1    NA 2012-10-01        0
## 2    NA 2012-10-01        5
## 3    NA 2012-10-01       10
## 4    NA 2012-10-01       15
## 5    NA 2012-10-01       20
## 6    NA 2012-10-01       25
```

##What is mean total number of steps taken per day?

```r
tot_steps <- tapply(activity$steps, activity$date, FUN=sum, na.rm=TRUE)
tot_steps
```

```
## 2012-10-01 2012-10-02 2012-10-03 2012-10-04 2012-10-05 2012-10-06 
##          0        126      11352      12116      13294      15420 
## 2012-10-07 2012-10-08 2012-10-09 2012-10-10 2012-10-11 2012-10-12 
##      11015          0      12811       9900      10304      17382 
## 2012-10-13 2012-10-14 2012-10-15 2012-10-16 2012-10-17 2012-10-18 
##      12426      15098      10139      15084      13452      10056 
## 2012-10-19 2012-10-20 2012-10-21 2012-10-22 2012-10-23 2012-10-24 
##      11829      10395       8821      13460       8918       8355 
## 2012-10-25 2012-10-26 2012-10-27 2012-10-28 2012-10-29 2012-10-30 
##       2492       6778      10119      11458       5018       9819 
## 2012-10-31 2012-11-01 2012-11-02 2012-11-03 2012-11-04 2012-11-05 
##      15414          0      10600      10571          0      10439 
## 2012-11-06 2012-11-07 2012-11-08 2012-11-09 2012-11-10 2012-11-11 
##       8334      12883       3219          0          0      12608 
## 2012-11-12 2012-11-13 2012-11-14 2012-11-15 2012-11-16 2012-11-17 
##      10765       7336          0         41       5441      14339 
## 2012-11-18 2012-11-19 2012-11-20 2012-11-21 2012-11-22 2012-11-23 
##      15110       8841       4472      12787      20427      21194 
## 2012-11-24 2012-11-25 2012-11-26 2012-11-27 2012-11-28 2012-11-29 
##      14478      11834      11162      13646      10183       7047 
## 2012-11-30 
##          0
```

```r
hist(tot_steps)
```

![plot of chunk unnamed-chunk-2](figure/unnamed-chunk-2-1.png) 

```r
mean_perday <- mean(tot_steps, na.rm=TRUE)
mean_perday
```

```
## [1] 9354.23
```

```r
median_perday <- median(tot_steps, na.rm=TRUE)
median_perday
```

```
## [1] 10395
```

##What is the average daily activity pattern?

```r
averages <- aggregate(x=list(steps=activity$steps), by=list(interval=activity$interval),
                         FUN=mean, na.rm=TRUE)

plot(averages$steps~averages$interval,
     type="l", ylab="Avg # of Steps", xlab="Time Interval", 
     main="Average # of steps across all days")
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3-1.png) 

```r
max_time <- which.max(averages$steps)
max_time
```

```
## [1] 104
```

##Imputing missing values

```r
missing <- is.na(activity$steps)
table(missing)
```

```
## missing
## FALSE  TRUE 
## 15264  2304
```

```r
##fill in missing values with the average for that time interval
fill_value <- function(steps, interval) {
     filled <- NA
     if (!is.na(steps))
          filled <- c(steps)
     else
          filled <- (averages[averages$interval==interval, "steps"])
     return(filled)
}
filled_activity <- activity
filled_activity$steps <- mapply(fill_value, filled_activity$steps, filled_activity$interval)
head(filled_activity)
```

```
##       steps       date interval
## 1 1.7169811 2012-10-01        0
## 2 0.3396226 2012-10-01        5
## 3 0.1320755 2012-10-01       10
## 4 0.1509434 2012-10-01       15
## 5 0.0754717 2012-10-01       20
## 6 2.0943396 2012-10-01       25
```

```r
tot_steps_filled <- tapply(filled_activity$steps, filled_activity$date, FUN=sum, na.rm=TRUE)
tot_steps_filled
```

```
## 2012-10-01 2012-10-02 2012-10-03 2012-10-04 2012-10-05 2012-10-06 
##   10766.19     126.00   11352.00   12116.00   13294.00   15420.00 
## 2012-10-07 2012-10-08 2012-10-09 2012-10-10 2012-10-11 2012-10-12 
##   11015.00   10766.19   12811.00    9900.00   10304.00   17382.00 
## 2012-10-13 2012-10-14 2012-10-15 2012-10-16 2012-10-17 2012-10-18 
##   12426.00   15098.00   10139.00   15084.00   13452.00   10056.00 
## 2012-10-19 2012-10-20 2012-10-21 2012-10-22 2012-10-23 2012-10-24 
##   11829.00   10395.00    8821.00   13460.00    8918.00    8355.00 
## 2012-10-25 2012-10-26 2012-10-27 2012-10-28 2012-10-29 2012-10-30 
##    2492.00    6778.00   10119.00   11458.00    5018.00    9819.00 
## 2012-10-31 2012-11-01 2012-11-02 2012-11-03 2012-11-04 2012-11-05 
##   15414.00   10766.19   10600.00   10571.00   10766.19   10439.00 
## 2012-11-06 2012-11-07 2012-11-08 2012-11-09 2012-11-10 2012-11-11 
##    8334.00   12883.00    3219.00   10766.19   10766.19   12608.00 
## 2012-11-12 2012-11-13 2012-11-14 2012-11-15 2012-11-16 2012-11-17 
##   10765.00    7336.00   10766.19      41.00    5441.00   14339.00 
## 2012-11-18 2012-11-19 2012-11-20 2012-11-21 2012-11-22 2012-11-23 
##   15110.00    8841.00    4472.00   12787.00   20427.00   21194.00 
## 2012-11-24 2012-11-25 2012-11-26 2012-11-27 2012-11-28 2012-11-29 
##   14478.00   11834.00   11162.00   13646.00   10183.00    7047.00 
## 2012-11-30 
##   10766.19
```

```r
hist(tot_steps_filled)
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1.png) 

```r
mean_perday_filled <- mean(tot_steps_filled)
mean_perday_filled
```

```
## [1] 10766.19
```

```r
median_perday_filled <- median(tot_steps_filled)
median_perday_filled
```

```
## [1] 10766.19
```


##Are there differences in activity patterns between weekdays and weekends?

```r
weekend_or_weekday <- function(date) {
     day <- weekdays(date)
     if (day %in% c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday"))
          return("weekday")
     else if (day %in% c("Saturday", "Sunday"))
          return("weekend")
}
filled_activity$date <- as.Date(filled_activity$date)
filled_activity$day <- sapply(filled_activity$date, FUN=weekend_or_weekday)
head(filled_activity)
```

```
##       steps       date interval     day
## 1 1.7169811 2012-10-01        0 weekday
## 2 0.3396226 2012-10-01        5 weekday
## 3 0.1320755 2012-10-01       10 weekday
## 4 0.1509434 2012-10-01       15 weekday
## 5 0.0754717 2012-10-01       20 weekday
## 6 2.0943396 2012-10-01       25 weekday
```

```r
averages <- aggregate(steps ~ interval + day, data=filled_activity, mean)
with(averages, {
     plot(averages$interval[averages$day=="weekday"], averages$steps[averages$day=="weekday"],
                     type="l", ylab="Avg # of Steps", xlab="Time Interval")
     lines(averages$interval[averages$day=="weekend"], averages$steps[averages$day=="weekend"], 
     col=2)
})
legend("topright", col=c("Black","Red"), lty=1, lwd=2,
       legend=c("Weekday","Weekend"))
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-1.png) 
      
