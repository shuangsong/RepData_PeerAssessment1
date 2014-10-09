Assignment 1
==================
*Loading and preprocessing the data


```r
setwd("C:/Users/stephanie song/Desktop")
data<-read.csv("C:/Users/stephanie song/Desktop/activity.csv",colClasses=c("numeric","Date","factor"))
head(data,10)
```

```
##    steps       date interval
## 1     NA 2012-10-01        0
## 2     NA 2012-10-01        5
## 3     NA 2012-10-01       10
## 4     NA 2012-10-01       15
## 5     NA 2012-10-01       20
## 6     NA 2012-10-01       25
## 7     NA 2012-10-01       30
## 8     NA 2012-10-01       35
## 9     NA 2012-10-01       40
## 10    NA 2012-10-01       45
```

what is mean total number of steps taken per day?

*Make a histogram of the total number of steps taken each day

```r
library(ggplot2)
subdata<-data[,1:2]
step_sum<-aggregate(subdata$steps, list(subdata$date), sum)
class(step_sum)
```

```
## [1] "data.frame"
```

```r
names(step_sum)<-c("date","steps")
clean_sum<-step_sum[complete.cases(step_sum),]
clean_sum$month<-as.numeric(format(clean_sum$date, "%m"))
head(clean_sum)
```

```
##         date steps month
## 2 2012-10-02   126    10
## 3 2012-10-03 11352    10
## 4 2012-10-04 12116    10
## 5 2012-10-05 13294    10
## 6 2012-10-06 15420    10
## 7 2012-10-07 11015    10
```

```r
ggplot(clean_sum, aes(date, steps)) +
        geom_bar(stat="identity", colour="gray", fill="gray", width=0.8)+
        facet_grid(.~month, scales="free")+
        labs(title=" Total number of steps taken each day", x="date",y="total number of steps")
```

![plot of chunk 2cdchunk](figure/2cdchunk.png) 

* Calculate and report the mean and median total number of steps taken per day



```r
mean1<-mean(clean_sum$steps)
mean1
```

```
## [1] 10766
```

```r
median1<-median(clean_sum$steps)
median1
```

```
## [1] 10765
```
What is the average daily activity pattern?

*Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis).


```r
setwd("C:/Users/stephanie song/Desktop")
data<-read.csv("C:/Users/stephanie song/Desktop/activity.csv",colClasses=c("numeric","Date","numeric"))

noNA<-data[complete.cases(data),]

step_mean<-aggregate(noNA$steps, list(noNA$interval), mean)

names(step_mean)<-c("interval_group","steps_mean")

head(step_mean)
```

```
##   interval_group steps_mean
## 1              0    1.71698
## 2              5    0.33962
## 3             10    0.13208
## 4             15    0.15094
## 5             20    0.07547
## 6             25    2.09434
```

```r
ggplot(step_mean, aes(interval_group, steps_mean)) +
        geom_line(color="gray",  size=0.9)+
        labs(title=" Time series plot", x="5-minute interval", y="average number of steps taken")
```

![plot of chunk 4thchunk](figure/4thchunk.png) 

Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```r
group<-which.max(step_mean[,2])
step_mean[104, ]
```

```
##     interval_group steps_mean
## 104            835      206.2
```
Imputing missing values
*Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)

```r
setwd("C:/Users/stephanie song/Desktop")
data<-read.csv("C:/Users/stephanie song/Desktop/activity.csv",colClasses=c("numeric","Date","numeric"))
sum(is.na(data))
```

```
## [1] 2304
```

*Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.



```r
setwd("C:/Users/stephanie song/Desktop")
data<-read.csv("C:/Users/stephanie song/Desktop/activity.csv",colClasses=c("numeric","Date","numeric"))
new<-data
step_mean<-aggregate(new$steps, list(new$date), mean)
names(step_mean)<-c("date","steps")
head(step_mean)
```

```
##         date   steps
## 1 2012-10-01      NA
## 2 2012-10-02  0.4375
## 3 2012-10-03 39.4167
## 4 2012-10-04 42.0694
## 5 2012-10-05 46.1597
## 6 2012-10-06 53.5417
```

```r
step_mean[is.na(step_mean)]<-mean(step_mean$steps, na.rm=TRUE)
head(step_mean)
```

```
##         date   steps
## 1 2012-10-01 37.3826
## 2 2012-10-02  0.4375
## 3 2012-10-03 39.4167
## 4 2012-10-04 42.0694
## 5 2012-10-05 46.1597
## 6 2012-10-06 53.5417
```

*Make a histogram of the total number of steps taken each day.


```r
setwd("C:/Users/stephanie song/Desktop")
data<-read.csv("C:/Users/stephanie song/Desktop/activity.csv",colClasses=c("numeric","Date","numeric"))
new<-data
new[is.na(new)]<-mean(new$steps, na.rm=TRUE)
head(new)
```

```
##   steps       date interval
## 1 37.38 2012-10-01        0
## 2 37.38 2012-10-01        5
## 3 37.38 2012-10-01       10
## 4 37.38 2012-10-01       15
## 5 37.38 2012-10-01       20
## 6 37.38 2012-10-01       25
```

```r
new$month<-as.numeric(format(new$date, "%m"))
head(new)
```

```
##   steps       date interval month
## 1 37.38 2012-10-01        0    10
## 2 37.38 2012-10-01        5    10
## 3 37.38 2012-10-01       10    10
## 4 37.38 2012-10-01       15    10
## 5 37.38 2012-10-01       20    10
## 6 37.38 2012-10-01       25    10
```

```r
clean<-new[complete.cases(new),]
ggplot(clean, aes(date, steps)) +
        geom_bar(stat="identity", colour="gray", fill="gray", width=0.8)+
        facet_grid(.~month, scales="free")+
        labs(title=" Total number of steps taken each day", x="date",y="total number of steps")
```

![plot of chunk 8thchunk](figure/8thchunk.png) 


*Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?


```r
step_sum<-aggregate(new$steps, list(new$date), sum)
head(step_sum)
```

```
##      Group.1     x
## 1 2012-10-01 10766
## 2 2012-10-02   126
## 3 2012-10-03 11352
## 4 2012-10-04 12116
## 5 2012-10-05 13294
## 6 2012-10-06 15420
```

```r
names(step_sum)<-c("date","steps")
mean2<-mean(step_sum$steps)
```



```r
step_sum<-aggregate(new$steps, list(new$date), sum)
head(step_sum)
```

```
##      Group.1     x
## 1 2012-10-01 10766
## 2 2012-10-02   126
## 3 2012-10-03 11352
## 4 2012-10-04 12116
## 5 2012-10-05 13294
## 6 2012-10-06 15420
```

```r
names(step_sum)<-c("date","steps")
median2<-median(step_sum$steps)
```
Conclusion: 

```r
mean1
```

```
## [1] 10766
```

```r
mean2
```

```
## [1] 10766
```

```r
median1
```

```
## [1] 10765
```

```r
median2
```

```
## [1] 10766
```

*After imputing missing value, mean are same, while median in case 1 is smaller than median in case 2.

Are there differences in activity patterns between weekdays and weekends?

*Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.

```r
setwd("C:/Users/stephanie song/Desktop")
data<-read.csv("C:/Users/stephanie song/Desktop/activity.csv",colClasses=c("numeric","Date","numeric"))
newdata<-data
newdata<-newdata[complete.cases(newdata),]
head(newdata)
```

```
##     steps       date interval
## 289     0 2012-10-02        0
## 290     0 2012-10-02        5
## 291     0 2012-10-02       10
## 292     0 2012-10-02       15
## 293     0 2012-10-02       20
## 294     0 2012-10-02       25
```

```r
newdata$day <- factor(format(newdata$date, "%A"))
levels(newdata$day) <- list(weekday = c("Monday", "Tuesday","Wednesday", "Thursday", "Friday"),weekend = c("Saturday", "Sunday"))
levels(newdata$day)
```

```
## [1] "weekday" "weekend"
```

```r
head(newdata)
```

```
##     steps       date interval     day
## 289     0 2012-10-02        0 weekday
## 290     0 2012-10-02        5 weekday
## 291     0 2012-10-02       10 weekday
## 292     0 2012-10-02       15 weekday
## 293     0 2012-10-02       20 weekday
## 294     0 2012-10-02       25 weekday
```

*Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). 


```r
noNA<-newdata[complete.cases(newdata),]
new<-aggregate(noNA$steps, list(noNA$interval), mean)
names(new)<-c("interval","steps")
head(new)
```

```
##   interval   steps
## 1        0 1.71698
## 2        5 0.33962
## 3       10 0.13208
## 4       15 0.15094
## 5       20 0.07547
## 6       25 2.09434
```

```r
newdata$mean<-new$steps
head(newdata)
```

```
##     steps       date interval     day    mean
## 289     0 2012-10-02        0 weekday 1.71698
## 290     0 2012-10-02        5 weekday 0.33962
## 291     0 2012-10-02       10 weekday 0.13208
## 292     0 2012-10-02       15 weekday 0.15094
## 293     0 2012-10-02       20 weekday 0.07547
## 294     0 2012-10-02       25 weekday 2.09434
```

```r
ggplot(newdata, aes(interval, steps)) +
        geom_line(color="gray",  size=0.9)+
        facet_grid(.~day, scales="free")+
        labs(title=" Time series plot", x="5-minute interval", y="average number of steps taken")
```

![plot of chunk 13thchunk](figure/13thchunk.png) 

















