---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---


## Loading and preprocessing the data


```r
library(dplyr) 
```

```
## Warning: package 'dplyr' was built under R version 3.6.3
```

```
## 
## Attaching package: 'dplyr'
```

```
## The following objects are masked from 'package:stats':
## 
##     filter, lag
```

```
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

```r
if (file.exists("activity.zip"))
  unzip("activity.zip")

activities<-read.csv("activity.csv")

str(activities)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : Factor w/ 61 levels "2012-10-01","2012-10-02",..: 1 1 1 1 1 1 1 1 1 1 ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```

## What are the mean and median number of steps taken per day?
  + Calculate the total number of steps taken per day
  + If you do not understand the difference between a histogram and a barplot, research the difference between them. Make a histogram of the total number of steps taken each day
  + Calculate and report the mean and median of the total number of steps taken per day

```r
day_steps<-activities %>% group_by(date) %>% summarize(steps=sum(steps))
```

```
## `summarise()` ungrouping output (override with `.groups` argument)
```

```r
library(ggplot2)
```

```
## Warning: package 'ggplot2' was built under R version 3.6.3
```

```r
ggplot(day_steps, aes(x=date, y=steps)) +geom_bar(stat="identity")+labs(x="Date", y="Number of Steps")+ggtitle("Total Steps per Day")
```

```
## Warning: Removed 8 rows containing missing values (position_stack).
```

![](PA1_files/figure-html/unnamed-chunk-2-1.png)<!-- -->

```r
day_steps_mean<- mean(day_steps$steps, na.rm=TRUE)
day_steps_mean
```

```
## [1] 10766.19
```

```r
day_steps_median<-median(day_steps$steps, na.rm=TRUE)
day_steps_median
```

```
## [1] 10765
```

## What is the average daily activity pattern?
  + Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)



```r
mean_by_interval<-aggregate(steps ~ interval, activities, mean)
plot(mean_by_interval$interval, mean_by_interval$steps, type="l", xlab="5 Minute Intervals", ylab="Average Number of Steps")
```

![](PA1_files/figure-html/unnamed-chunk-3-1.png)<!-- -->
  + Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?
 
 ```r
 mean_by_interval[which.max(mean_by_interval$steps),]$interval
 ```
 
 ```
 ## [1] 835
 ```

## Imputing missing values

  +  Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with \color{red}{\verb|NA|}NAs)
  
  ```r
  missing_steps <- sum(is.na(activities$steps))
  missing_steps
  ```
  
  ```
  ## [1] 2304
  ```
  + Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.
    + Create a new dataset that is equal to the original dataset but with the missing data filled in.
  
  ```r
    activities$imputed_steps<-mean_by_interval$steps[match(activities$interval, mean_by_interval$interval)]
    imputed_activities<-activities
    imputed_activities$steps <- with(imputed_activities, ifelse(is.na(steps), imputed_steps, steps))
  ```

  + Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?
  
  
  ```r
  imputed_day_steps<-imputed_activities %>% group_by(date) %>% summarize(steps=sum(steps))
  ```
  
  ```
  ## `summarise()` ungrouping output (override with `.groups` argument)
  ```
  
  ```r
  ggplot(imputed_day_steps, aes(x=date, y=steps)) +geom_bar(stat="identity")+labs(x="Date", y="Number of Steps")+ggtitle("Total Steps per Day - Imputed missing values")
  ```
  
  ![](PA1_files/figure-html/unnamed-chunk-7-1.png)<!-- -->
  
  ```r
  day_steps_mean<- mean(day_steps$steps, na.rm=TRUE)
  day_steps_mean
  ```
  
  ```
  ## [1] 10766.19
  ```
  
  ```r
  day_steps_median<-median(day_steps$steps, na.rm=TRUE)
  day_steps_median
  ```
  
  ```
  ## [1] 10765
  ```
  
  ```r
  imputed_day_steps_mean<- mean(imputed_day_steps$steps)
  imputed_day_steps_mean
  ```
  
  ```
  ## [1] 10766.19
  ```
  
  ```r
  imputed_day_steps_median<-median(imputed_day_steps$steps)
  imputed_day_steps_median 
  ```
  
  ```
  ## [1] 10766.19
  ```


## Are there differences in activity patterns between weekdays and weekends?

  + Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.
  
  ```r
  activities$date <- as.Date(activities$date, format="%Y-%m-%d")
  activities$day<- weekdays(activities$date)
  activities$dayType <- with(activities, ifelse(day == "Saturday" | day == "Sunday", "weekend", "weekday"))
  ```
  + Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.
  
  
  ```r
  aggregate_type<-aggregate(steps ~ interval + dayType, activities, mean, na.rm=TRUE)
  ggplot(aggregate_type, aes(x=interval, y=steps,)) + geom_line() + 
  labs(title = "Average Steps by Day Type", x = "Interval", y ="Number of Steps") +
  facet_wrap(~dayType, ncol=1)
  ```
  
  ![](PA1_files/figure-html/unnamed-chunk-9-1.png)<!-- -->
