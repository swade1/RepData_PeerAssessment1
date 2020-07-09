---
title: "Reproducible Research: Peer Assessment 1"
author: Susan Wade
date: July 9, 2020
output: 
  html_document:
    keep_md: true
---
This assignment makes use of data from a personal activity monitoring device. This device collects data at 5 minute intervals through out the day. The data consists of two months of data from an anonymous individual collected during the months of October and November, 2012 and include the number of steps taken in 5 minute intervals each day. There are a total of 17,568 observations in this dataset.

The variables included in this dataset are:

* **steps**: Number of steps taking in a 5-minute interval (missing values are coded as NA)
* **date**: The date on which the measurement was taken in YYYY-MM-DD format
* **interval**: Identifier for the 5-minute interval in which measurement was taken


## Loading and preprocessing the data




## What is the mean total number of steps taken per day?
This histogram shows the number of steps taken per day on the x-axis. The y-axis indicates the number of times particular step counts occur. This graph shows a more-or-less normal distribution. For this plot, all NA values have been removed from the dataset.   

The mean and median values for this dataset are shown below the graph. 

```r
  df.hist <- na.omit(subset(df, select= -c(interval)))  
  histData <- tapply(df.hist$steps, df.hist$date, FUN=sum)
  xname <- "Steps per Day"
  hist(histData, main=paste("Histogram of", xname), xlab = xname)
```

![](figure/histogram-1.png)<!-- -->

```r
  summary_stats <- summary(histData)
  summary_stats["Median"]
```

```
## Median 
##  10765
```

```r
  summary_stats["Mean"]
```

```
##     Mean 
## 10766.19
```

## What is the average daily activity pattern?
The graph below shows the average daily activity pattern. Each 5 minute period for all days is averaged together before being plotted. For example, if the interval is 15, all interval 15 values across all days are averaged together. Each 24 hours contains 288 five minute periods which are shown on the x-axis. The y-axis is the average number of steps for each 5 minute period. There is a clear peak during the interval corresponding to 8:35-8:40 am (see code snippet and results below). Most likely this reflects the time period when the individual was in the process of getting up, getting ready for work, or leaving for work.   

```r
df$date <- as.Date(df$date)  
df <- na.omit(df)
avg_steps <- tapply(df$steps, df$interval, mean)
plot(x=avg_steps,type="l",lwd=2,ylab="Average number of steps throughout the day", xlab="5 minute intervals over 24 hours")
```

![](figure/timeseries-1.png)<!-- -->


```r
avg_steps <- sort(avg_steps,decreasing=TRUE)
n <-names(avg_steps)
print(c("The five minute period with the highest average number of steps was between",paste(n[1], "am and ", n[2], "am")))
```

```
## [1] "The five minute period with the highest average number of steps was between"
## [2] "835 am and  840 am"
```
  
## Imputing missing values  
All NA values have been replaced with the average for the corresponding 5 minute period. For example, if a line in the original csv contains 'NA	10/1/12	0' and the average steps for all 0-5 time periods is 0.1756, the NA is replaced with this value.  

```r
df <- read.csv("activity.csv", header=TRUE)

#check for NAs in rows
new_df <- df[rowSums(is.na(df)) > 0,]
print(paste("Number of NAs is: ", nrow(new_df)))
```

```
## [1] "Number of NAs is:  2304"
```

```r
df_no_na <- na.omit(df)
avg_steps <- tapply(df_no_na$steps, df_no_na$interval, mean)

for (row in 1:nrow(df)) {
    if (is.na(df$steps[row])) {
      idx <- df$interval[row] %% 288
      if (idx > 0) {
        df$steps[row] = avg_steps[[(idx/5) + 1]]
      } else {
        df$steps[row] = avg_steps[[1]]
      }
    }
}
```

The following histogram shows the distribution of steps with all NA values replaced by the averaged values for that time interval. The first two bins in the graph are the most affected by this change, reflecting higher values than when the NAs were simply removed.  The mean and median of this set are indicated below. Previously, the median was 10765 and the mean was 10766.19. Here, the median is 10395 and the mean is 9377.596. This is intuitively correct since more, generally lower, values have been added to the dataset.

```r
df.hist <- subset(df, select= -c(interval))
histData <- tapply(df.hist$steps, df.hist$date, FUN=sum)
xname <- "Steps per Day"
hist(histData, main=paste("Histogram of", xname), xlab = xname)
```

![](figure/unnamed-chunk-3-1.png)<!-- -->

```r
summary_stats <- summary(histData)
summary_stats["Median"]
```

```
## Median 
##  10395
```

```r
summary_stats["Mean"]
```

```
##     Mean 
## 9377.596
```
## Are there differences in activity patterns between weekdays and weekends?  
Clear differences can be observed between weekend and weekday activity.  During the weekdays, there is a peak at the beginning of the day with lower activity for the rest of the day, presumably during the time when a person is at work. There is a slight peak in the evening, most likely indicating the commute home and evening activity at home.   

The weeekends show a similar peak of activity in the morning the then the rest of the day shows generally higher activity associated with weekend activities that don't correspond to sitting at a desk at work.  


```r
df$date <- as.Date(df$date)
df$day <- chron::is.weekend(df$date)
df$day <- as.numeric(df$day)
df$day <- as.factor(ifelse(df$day > 0, "weekend","weekday"))
```



```r
weekday <- subset(df, df$day == "weekday")
weekend <- subset(df, df$day == "weekend")

wkday_avg_steps <- tapply(weekday$steps, weekday$interval, mean)
wkend_avg_steps <- tapply(weekend$steps, weekend$interval, mean)

par(mfrow=c(2,1))
plot(x=wkday_avg_steps,type="l",lwd=2, main="Weekdays",xlab='',ylim=c(0,200),ylab='')
plot(x=wkend_avg_steps,type="l",lwd=2, main="Weekends", ylim=c(0,200), xlab="5 minute intervals over 24 hours",ylab='')
```

![](figure/unnamed-chunk-5-1.png)<!-- -->
