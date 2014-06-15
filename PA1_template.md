# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data

```r
unzip("activity.zip")
activity <- read.csv("activity.csv")
activity <- transform(activity, date = as.Date(date))
```


## What is mean total number of steps taken per day?
- Histogram

```r
steps.by.date <- aggregate( steps ~ date, data = activity, FUN = sum, na.action = na.omit )
par(mfrow=c(2,1))
plot(steps.by.date$date,steps.by.date$steps,type="h")
hist(steps.by.date$steps,breaks=20,main="")
```

![plot of chunk unnamed-chunk-2](figure/unnamed-chunk-2.png) 

- Mean

```r
mean(steps.by.date$steps)
```

```
## [1] 10766
```

- Median

```r
median(steps.by.date$steps)
```

```
## [1] 10765
```

## What is the average daily activity pattern?
- Time series plot

```r
steps.by.interval <- aggregate( steps ~ interval, data = activity, FUN = mean, na.action = na.omit )
plot(steps.by.interval$interval,steps.by.interval$steps,type="l")
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5.png) 

- Interval containing max average nb of steps (across all days)

```r
steps.by.interval$interval[which.max(steps.by.interval$steps)]
```

```
## [1] 835
```

## Imputing missing values
- Number of missing values

```r
sum(is.na(activity$steps))
```

```
## [1] 2304
```

- Imputation

*Filling NA values by the mean of values available for that interval across all days*

```r
activity.imputed <- activity
for (i in 1:nrow(activity) ) {
  if (is.na(activity$steps[i])){
    activity.imputed$steps[i] <- mean(activity$steps[activity$interval==activity$interval[i]],na.rm=TRUE)
  } 
}
```

- Impact

```r
steps.by.date2 <- aggregate( steps ~ date, data = activity.imputed, FUN = sum, na.action = na.omit )
par(mfrow=c(2,1))
hist(steps.by.date2$steps,breaks=20,main="with imputation")
hist(steps.by.date$steps,breaks=20,main="without imputation")
```

![plot of chunk unnamed-chunk-9](figure/unnamed-chunk-9.png) 


```r
mean(steps.by.date2$steps)
```

```
## [1] 10766
```

```r
median(steps.by.date2$steps)
```

```
## [1] 10766
```

*Conclusion: with this imputation strategy, mean and median are (almost) the same but the distribution of total number of steps taken per day is more narrowly distributed around its mean.*

## Are there differences in activity patterns between weekdays and weekends?

```r
library(lattice)
activity.weekend = subset(activity.imputed,weekdays(activity.imputed$date) %in% c("Saturday","Sunday"))
activity.weekday = subset(activity.imputed,!(weekdays(activity.imputed$date) %in% c("Saturday","Sunday")))
steps.by.interval2a <- aggregate( steps ~ interval, data = activity.weekday, FUN = mean, na.action = na.omit )
steps.by.interval2a$day = as.factor("weekday")
steps.by.interval2b <- aggregate( steps ~ interval, data = activity.weekend, FUN = mean, na.action = na.omit )
steps.by.interval2b$day = as.factor("weekend")
steps.by.interval2 = rbind(steps.by.interval2a,steps.by.interval2b)
f <- factor(steps.by.interval2$day,labels = c("weekday","weekend"))
xyplot(steps.by.interval2$steps ~ steps.by.interval2$interval | f, layout=c(1,2),type="l",xlab="Interval",ylab="Number of steps")
```

![plot of chunk unnamed-chunk-11](figure/unnamed-chunk-11.png) 

*Conclusion: there are indeed differences in activity patterns between weekdays and weekends - for example, weekend activity is slower in early morning (as expected).*
