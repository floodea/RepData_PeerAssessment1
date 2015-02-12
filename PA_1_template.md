Reproducible Research Peer Assessment 1
========================



This is the markdown file for Peer Assessment 1 of the Reproducible Research Course

First we load the data required and coerce the date column to a date item.


```r
data<-read.csv("activity.csv")
data$date<-as.Date(data$date)
```

Calculate the total number of steps per day. NA values are taken as zero so that
the sum function can work. Name the columns


```r
Steps<-aggregate(data$steps,by=list(data$date),FUN=sum,na.rm=TRUE)
colnames(Steps)<-c("date","steps")
```

Histogram of total number of steps taken per day


```r
hist(Steps$steps,breaks=61,main="Total Steps",xlab="Number of Steps",ylab="Frequency",col="Red",xlim=range(0,22000),ylim=range(0,11))
```

![plot of chunk Histogram1](figure/Histogram1-1.png) 

Calculate the mean and median steps per day:


```r
print(paste("Mean=",mean(Steps$steps)))
```

```
## [1] "Mean= 9354.22950819672"
```

```r
print(paste("Median=",median(Steps$steps)))
```

```
## [1] "Median= 10395"
```

Time series plot of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)


```r
Interval<-aggregate(data$steps,by=list(data$interval),FUN=mean,na.rm=TRUE)
  colnames(Interval)<-c("Interval","MeanSteps")
  plot(Interval$Interval,Interval$MeanSteps,type="l",xlab="Time Interval",ylab="Mean Steps Per Interval")
```

![plot of chunk Plot1](figure/Plot1-1.png) 

Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?


```r
Max<-max(Interval$MeanSteps,na.rm=TRUE)
IntMax<-Interval$Interval[Interval$MeanSteps==Max]
print(IntMax)
```

```
## [1] 835
```

Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)


```r
length(data[is.na(data$steps),1])
```

```
## [1] 2304
```

Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.


```r
index<-which(is.na(data))
  for (i in 1:length(index)){
  data[index,"steps"][i]<- Interval[Interval$Interval==data[index,"interval"][i],"MeanSteps"]
  }
```

Create a new dataset that is equal to the original dataset but with the missing data filled in.


```r
dataClean<-data #New dataset
rm(data)
```

Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?


```r
Steps2<-aggregate(dataClean$steps,by=list(dataClean$date),FUN=sum,na.rm=TRUE)
  colnames(Steps2)<-c("date","steps")
  hist(Steps2$steps,breaks=61,main="Total Steps",xlab="Number of Steps",ylab="Frequency",col="Red",xlim=range(0,22000),ylim=range(0,11))
```

![plot of chunk Histogram2](figure/Histogram2-1.png) 

Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?


```r
print(paste("Mean=",mean(Steps2$steps)))
```

```
## [1] "Mean= 10766.1886792453"
```

```r
print(paste("Median=",median(Steps2$steps)))
```

```
## [1] "Median= 10766.1886792453"
```
Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.


```r
dataClean$date<-weekdays(dataClean$date)
dataClean$date[dataClean$date=="Saturday"]<-"Weekend"
dataClean$date[dataClean$date=="Sunday"]<-"Weekend"
dataClean$date[dataClean$date!="Weekend"]<-"Weekday"
dataClean$date<-as.factor(dataClean$date)
levels(dataClean$date)<-c("Weekday","Weekend")
```

Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis).


```r
  library(lattice)
  Interval<-aggregate(dataClean$steps,by=list(dataClean$interval,dataClean$date),FUN=mean,na.rm=TRUE)
  colnames(Interval)<-c("Interval","DayType","MeanSteps")  
  xyplot(MeanSteps~Interval|DayType,data=Interval,layout=c(1,2),xlab="Interval",ylab="Number of Steps",type="l")
```

![plot of chunk Plot2](figure/Plot2-1.png) 

