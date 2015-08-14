---
title: "activity"
output: html_document
---

```r
library(data.table)
library(ggplot2)
```
##load data

```r
setwd('/Users/Nan/Documents/Mooc/Reproducible_research')
data<-read.csv('activity.csv')
data<-data.table(data)
```
##What is mean total number of steps taken per day?
1 Calculate the total number of steps taken per day

```r
dsum<-aggregate(data$steps, by=list(date=data$date),FUN=sum,na.rm= T)
names(dsum)<-c('date','steps')
```
2 Make a histogram of the total number of steps taken each day

```r
hist(dsum$steps,main='histogram of the total number of steps taken each day',xlab='total number of steps per day')
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1.png) 
  
3 Calculate and report the mean and median of the total number of steps taken per day

```r
dsum<-data.table(dsum)
dsum[,list(mean(steps),median(steps))]
```

```
##         V1    V2
## 1: 9354.23 10395
```
The mean of the total number of steps taken per day is 9354.23 and median is 10395.  

##What is the average daily activity pattern?
1 Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

```r
iave<-aggregate(data$steps, by=list(interval=data$interval),FUN=mean,na.rm= T)
names(iave)<-c('interval','steps')
with(iave,plot(interval,steps,type='l'))
```

![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-1.png) 
  
2 Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```r
iave<-iave[with(iave,order(-steps)),]
iave[1,]
```

```
##     interval    steps
## 104      835 206.1698
```
Interval 835 contains the maximum number of steps.  

##Imputing missing values  
1 Calculate and report the total number of missing values in the dataset

```r
summary(data)
```

```
##      steps                date          interval     
##  Min.   :  0.00   2012-10-01:  288   Min.   :   0.0  
##  1st Qu.:  0.00   2012-10-02:  288   1st Qu.: 588.8  
##  Median :  0.00   2012-10-03:  288   Median :1177.5  
##  Mean   : 37.38   2012-10-04:  288   Mean   :1177.5  
##  3rd Qu.: 12.00   2012-10-05:  288   3rd Qu.:1766.2  
##  Max.   :806.00   2012-10-06:  288   Max.   :2355.0  
##  NA's   :2304     (Other)   :15840
```
There are 2304 missing values in the dataset.  

2 Create a new dataset that is equal to the original dataset but with the missing data filled in with mean for that interval.

```r
noNAdata<-data
for (i in 1:nrow(noNAdata)){
  if (is.na(noNAdata[i,steps])){
    intv<-noNAdata[i,interval]
    noNAdata[i,]$steps<-iave[iave$interval==intv,]$steps
  } 
}
```
3 Make a histogram of the total number of steps taken each day.

```r
dsum<-aggregate(noNAdata$steps, by=list(date=noNAdata$date),FUN=sum,na.rm= T)
names(dsum)<-c('date','steps')
hist(dsum$steps,main='histogram of the total number of steps taken each day',xlab='total number of steps per day')
```

![plot of chunk unnamed-chunk-10](figure/unnamed-chunk-10-1.png) 
  
4 Calculate and report the mean and median total number of steps taken per day. 

```r
dsum<-data.table(dsum)
dsum[,list(mean(steps),median(steps))]
```

```
##          V1    V2
## 1: 10749.77 10641
```
The mean of the total number of steps taken per day is 10749.77 and median is 10641.  
Do these values differ from the estimates from the first part of the assignment?  
What is the impact of imputing missing data on the estimates of the total daily number of steps?  
Yes, these values are higher after imputing missing data.  

##Are there differences in activity patterns between weekdays and weekends?
1 Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.

```r
noNAdata[,date:=as.POSIXct(date)]
```

```
##        steps       date interval
##     1:     1 2012-10-01        0
##     2:     0 2012-10-01        5
##     3:     0 2012-10-01       10
##     4:     0 2012-10-01       15
##     5:     0 2012-10-01       20
##    ---                          
## 17564:     4 2012-11-30     2335
## 17565:     3 2012-11-30     2340
## 17566:     0 2012-11-30     2345
## 17567:     0 2012-11-30     2350
## 17568:     1 2012-11-30     2355
```

```r
noNAdata$week<-ifelse(weekdays(noNAdata$date) %in% c('Sunday','Saturday'),'weekend','weekday')
```
2 Make a panel plot containing a time series plot of the 5-minute interval and the average number of steps taken, averaged across all weekday days or weekend days.

```r
g<-ggplot(noNAdata,aes(x=interval,y=steps))
g+geom_line()+facet_wrap(~week)+theme_bw()
```

![plot of chunk unnamed-chunk-13](figure/unnamed-chunk-13-1.png) 


