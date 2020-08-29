---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---


## Loading and preprocessing the data
Unzip data to obtain a csv file.


```r
library(ggplot2)
library(lubridate)
```

```
## 
## Attaching package: 'lubridate'
```

```
## The following objects are masked from 'package:base':
## 
##     date, intersect, setdiff, union
```

```r
unzip("activity.zip",exdir = "data")
```
Load Data

```r
activity <- read.csv("data/activity.csv",stringsAsFactors=FALSE)
```
Change type of date column to Date

```r
activity$date<-ymd(activity$date)
```

## What is mean total number of steps taken per day?

1. Calculate the total number of steps taken per day


```r
stepsPerDay <- aggregate(activity$steps, list(activity$date), FUN=sum)
colnames(stepsPerDay) <- c("Date", "Steps")
head(stepsPerDay)
```

```
##         Date Steps
## 1 2012-10-01    NA
## 2 2012-10-02   126
## 3 2012-10-03 11352
## 4 2012-10-04 12116
## 5 2012-10-05 13294
## 6 2012-10-06 15420
```

2. Histogram of the total number of steps taken each day

```r
# draw the histogram
hPlot <- ggplot(stepsPerDay, aes(Steps))
hPlot+
  geom_histogram(fill="blue",col="green",binwidth=2000)+
  ggtitle("Steps per day")+
  xlab("Steps")+
  ylab("Frequency")+
  scale_x_continuous(breaks=seq(0,24000,4000))+
  scale_y_continuous(breaks=seq(0,14,2))
```

```
## Warning: Removed 8 rows containing non-finite values (stat_bin).
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png)<!-- -->

3. Mean and median number of steps taken each day

Mean

```r
mean(stepsPerDay$Steps, na.rm=TRUE)
```

```
## [1] 10766.19
```
Median


```r
median(stepsPerDay$Steps, na.rm=TRUE)
```

```
## [1] 10765
```

## What is the average daily activity pattern?
1.Time series plot of the average number of steps taken

```r
# create steps/time
stepsPerTime <- aggregate(steps~interval,data=activity,FUN=mean,na.action=na.omit)
# line plot
lPlot <- ggplot(stepsPerTime, aes(interval, steps))
lPlot+geom_line(col="blue")+
  ggtitle("Average steps per interval")+
  xlab("Time")+
  ylab("Steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-8-1.png)<!-- -->

## Imputing missing values



## Are there differences in activity patterns between weekdays and weekends?
