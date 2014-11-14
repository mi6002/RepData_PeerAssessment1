# Reproducible Research: Peer Assessment 1

#Assumptions
It is assumed that the knitr package has been installed.
It is further assumed that the data file has been downloaded and unzipped in the working directory.

## Loading and preprocessing the data

```r
dir = ("C:/Users/Public")
setwd(dir)
 
activitydata <- read.csv("activity.csv")
```

## What is mean total number of steps taken per day?
1.  Make a histogram of the total number of steps taken each day

```r
  dailysteps <- aggregate(steps ~ date, activitydata, sum)  
  barplot(dailysteps$steps, names.arg = dailysteps$date, xlab = "date", ylab = "steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-2-1.png) 

2.  Calculate and report the mean and median total number of steps taken per day

```r
mean(dailysteps$steps)  
```

```
## [1] 10766.19
```

```r
median(dailysteps$steps)
```

```
## [1] 10765
```


## What is the average daily activity pattern?

1.  Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)


```r
plot(aggregate(steps ~ interval, activitydata, mean, type = "l"))
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png) 

2.  Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?


```r
int <- aggregate(steps ~ interval,  activitydata, mean) 
int$interval[which.max(int$steps)]
```

```
## [1] 835
```

## Imputing missing values
Note that there are a number of days/intervals where there are missing values (coded as NA). The presence of missing days may introduce bias into some calculations or summaries of the data.

1.  Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)

```r
sum(is.na(activitydata))  
```

```
## [1] 2304
```

2.  Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

  
Strategy: using the mean for that 5-minute interval seems appropriate. 



3.  Create a new dataset that is equal to the original dataset but with the missing data filled in.


```r
  activitydata <- merge(activitydata, int, by = "interval", suffixes = c("", ".y"))  
  fillna <- is.na(activitydata$steps)  
  activitydata$steps[fillna] <- activitydata$steps.y[fillna]
  newactivitydata <- activitydata[, c(1:3)]  
```


4.  Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?


```r
  newdailysteps <- aggregate(steps ~ date, newactivitydata, sum)  
  barplot(newdailysteps$steps, names.arg = newdailysteps$date, xlab = "date", ylab = "steps")  
```

![](PA1_template_files/figure-html/unnamed-chunk-8-1.png) 

The new mean and median values are:

```r
mean(newdailysteps$steps)  
```

```
## [1] 10766.19
```

```r
median(newdailysteps$steps)
```

```
## [1] 10766.19
```
  
  
  These values are essentially the same as the original values with missing values. So, we can conclude that the missing values have an almost negligible impact on the results.

## Are there differences in activity patterns between weekdays and weekends?
For this part the weekdays() function may be of some help here. Use the dataset with the filled-in missing values for this part.

1.  Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.

```r
newdayfact <- function(date) {
  if (weekdays(as.Date(date)) %in% c("Saturday", "Sunday")) 
    {"weekend"}
  else 
    {"weekday"}
  }
activitydata$newdayfact <- as.factor(sapply(activitydata$date, newdayfact))
```



2.  Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.


```r
par(mfrow=c(2,1))
for (type in c("weekend", "weekday")) {
    
    plot(aggregate(steps ~ interval,
                            data=activitydata,
                            subset=activitydata$newdayfact==type,
                            FUN=mean), type="l", main=type)
}
```

![](PA1_template_files/figure-html/unnamed-chunk-11-1.png) 


