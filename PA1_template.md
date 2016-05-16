# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data

```r
unzip("activity.zip")
activity <- read.csv("activity.csv", stringsAsFactors = FALSE)
activity$date <- as.Date(activity$date)
```

## What is mean total number of steps taken per day?

```r
#total number of Steps per day
totalSteps <- tapply(activity$steps, activity$date, FUN=sum, na.rm=TRUE) 
library(ggplot2)
qplot(totalSteps, binwidth=1000, xlab="total number of steps taken each day")
```

![](PA1_template_files/figure-html/mean steps per day-1.png) 

```r
#mean of steps taken per day
mean(totalSteps, na.rm=TRUE)
```

```
## [1] 9354.23
```

```r
#median of stpes taken per day
median(totalSteps, na.rm=TRUE)
```

```
## [1] 10395
```

## What is the average daily activity pattern?

```r
library(ggplot2)
#calculate mean per interval
avgActitivyPattern <- aggregate(x=list(steps=activity$steps), by=list(interval=activity$interval),
                      FUN=mean, na.rm=TRUE)
head(avgActitivyPattern)
```

```
##   interval     steps
## 1        0 1.7169811
## 2        5 0.3396226
## 3       10 0.1320755
## 4       15 0.1509434
## 5       20 0.0754717
## 6       25 2.0943396
```

```r
ggplot(data=avgActitivyPattern, aes(x=interval, y=steps)) +
    geom_line() +
    xlab("5-minute interval") +
    ylab("Average number of steps taken")
```

![](PA1_template_files/figure-html/average activity Pattern-1.png) 

```r
# 5 min interval with max steps
avgActitivyPattern[which.max(avgActitivyPattern$steps),]
```

```
##     interval    steps
## 104      835 206.1698
```

## Imputing missing values

```r
# number of rows with NA
activityNA <- sum(is.na(activity$steps))
print(activityNA)
```

```
## [1] 2304
```

```r
#filling up missing values with the mean of the interval to create new dataset
activityNew <- activity
for (i in 1:nrow(activityNew)){
  if (is.na(activityNew$steps[i])){
    intervalValue <- activityNew$interval[i]
    rowIndex <- which(avgActitivyPattern$interval == intervalValue)
    stepValue <- avgActitivyPattern$steps[rowIndex]
    activityNew$steps[i] <- stepValue
  }
}

totalStepsNew <- tapply(activityNew$steps, activityNew$date, FUN=sum)
qplot(totalStepsNew, binwidth=1000, xlab="total number of steps taken each day")
```

![](PA1_template_files/figure-html/missing values-1.png) 

```r
mean(totalStepsNew)
```

```
## [1] 10766.19
```

```r
median(totalStepsNew)
```

```
## [1] 10766.19
```


## Are there differences in activity patterns between weekdays and weekends?

```r
activityNew$date <- as.Date(activityNew$date, "%Y-%m-%d")
# add a new column day in dataset
activityNew$day <- weekdays(activityNew$date)
# add a new column called day type and initialize to weekday
activityNew$type <- c("weekday")

# If Saturday or Sunday then make type as weekend
for (i in 1:nrow(activityNew)){
  if (activityNew$day[i] == "Saturday" || activityNew$day[i] == "Sunday"){
    activityNew$type[i] <- "weekend"
  }
}

# convert type from character to factor
activityNew$type <- as.factor(activityNew$type)

# get Average stpes per interval and day type
avgSteps <- aggregate(steps ~ interval+type, activityNew, mean)
ggplot(avgSteps, aes(interval, steps)) + geom_line() + facet_grid(type ~ .) +
    xlab("5-minute interval") + ylab("Number of steps")
```

![](PA1_template_files/figure-html/weekends vs weekdays -1.png) 
