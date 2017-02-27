# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data

```r
download.file("https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip",
    destfile="activity.zip",)
unzip("activity.zip", exdir="activity")
data <- read.csv("./activity/activity.csv"
                 , colClasses = c("numeric", "character", "numeric"))
```

## What is mean total number of steps taken per day?  
###The following is a histogram of total number of steps taken per day:

```r
total_steps_per_day <- tapply(data$steps, data$date, sum, na.rm=TRUE)
library(ggplot2)
ggplot(NULL, aes(total_steps_per_day)) + xlab("Total steps taken per day") + geom_histogram(binwidth = 2000)
```

![](PA1_template_files/figure-html/histogram of total number of steps taken per day-1.png)<!-- -->

###Mean of total number of steps taken per day is:

```r
mean_original <- mean(total_steps_per_day)
mean_original
```

```
## [1] 9354.23
```

###Median of total number of steps taken per day is:

```r
median_original <- median(total_steps_per_day)
median_original
```

```
## [1] 10395
```

## What is the average daily activity pattern?
###The following is a plot for average number of steps of the 5-minute interval, averaged across all days (y-axis)

```r
avg_accross_days <- tapply(data$steps, data$interval, mean, na.rm=TRUE)
plot(x=names(avg_accross_days),
     y=avg_accross_days, type="l", ylab="Average steps of 5-min interval accross all days", xlab="Interval")
```

![](PA1_template_files/figure-html/average number of steps of the 5-minute interval-1.png)<!-- -->


###The interval which has the maximum average number of steps accross all days is:

```r
names(which.max(avg_accross_days))
```

```
## [1] "835"
```


## Imputing missing values

###Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)

```r
nrow(data) - sum(!is.na(data$steps))
```

```
## [1] 2304
```

###Create a new dataset that is equal to the original dataset but with the missing data filled in. The missing values in the new dataset are filled in with the mean of steps accross all intervals and all days.


```r
mean_of_all_intervals <- mean(data$steps, na.rm = TRUE)
data2 <- data
data2$steps[is.na(data2$steps)] <- mean_of_all_intervals
```

###Compare the histogram of the total number of steps taken each day for both the original dataset and the new dataset. And Calculate and report the mean and median total number of steps taken per day.

```r
total_steps_per_day <- cbind(total_steps_per_day, 'Original Dataset')
total_steps_per_day2 <- tapply(data2$steps, data2$date, sum, na.rm=TRUE)
total_steps_per_day2 <- cbind(total_steps_per_day2, 'New Dataset')
rownames(total_steps_per_day) <- NULL
rownames(total_steps_per_day2) <- NULL
all <- data.frame(rbind(total_steps_per_day, total_steps_per_day2), stringsAsFactors=FALSE)
colnames(all)  <-  c('steps','flag')
all$steps <- as.numeric(all$steps)
ggplot(all, aes(steps)) + geom_histogram(binwidth = 2000) + facet_grid(flag ~ .)
```

![](PA1_template_files/figure-html/histogram for both the original dataset and the new dataset-1.png)<!-- -->

###Mean and Median number difference between original dataset and the new dataset is:

####Mean of original dataset:

```r
mean_original
```

```
## [1] 9354.23
```
####Mean of new dataset:

```r
mean_new <- mean(as.numeric(total_steps_per_day2[,1]))
mean_new
```

```
## [1] 10766.19
```

####Difference of mean is:

```r
mean_original - mean_new
```

```
## [1] -1411.959
```

####Median of original dataset:

```r
median_original
```

```
## [1] 10395
```

####Median of new dataset:

```r
median_new <- median(as.numeric(total_steps_per_day2[,1]))
median_new
```

```
## [1] 10766.19
```

####Difference of median is:

```r
median_original - median_new
```

```
## [1] -371.1887
```

## Are there differences in activity patterns between weekdays and weekends?

####Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day. And then make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). 

```r
data3 <-  transform(data, weekdayflag = ifelse(as.POSIXlt(data$date)$wday %in% c(0,6), 'weekend', 'weekday'))
library(plyr)
data3 <- ddply(data3, .(interval, weekdayflag), summarise, mean_steps = mean(steps, na.rm = TRUE))
ggplot(data3, aes(interval,mean_steps)) + ylab("Number of steps") +geom_line() + facet_grid(weekdayflag~.)
```

![](PA1_template_files/figure-html/average steps across all weekday days or weekend days-1.png)<!-- -->

