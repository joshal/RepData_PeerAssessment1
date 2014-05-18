# Reproducible Research: Peer Assessment 1




## Loading and preprocessing the data

For loading the data, we assume that the data is available in the `data` directory at the same level as the markdown file. If it is not, the data should be [downloaded](https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip) and unzipped in the `data` directory. The unzipped file is named `activity.csv`



```r
# Path to data
data_file_path <- "data/activity.csv"
if (!file.exists(data_file_path)) {
    stop("Data file not available. Please place the data in `data/activity.csv`")
}
data <- read.csv(data_file_path)
data$date <- as.Date(data$date)
# Check how the data looks
head(data, n = 5)
```

```
##   steps       date interval
## 1    NA 2012-10-01        0
## 2    NA 2012-10-01        5
## 3    NA 2012-10-01       10
## 4    NA 2012-10-01       15
## 5    NA 2012-10-01       20
```


## What is mean total number of steps taken per day?

1. The histogram of the data can be plotted using the following piece of code


```r
with(data, hist(steps))
```

![plot of chunk histogram_steps_raw_data](figure/histogram_steps_raw_data.png) 


2. The mean and meadian of the total number of steps can be calculated as


```r
mean_steps <- mean(data$steps, na.rm = TRUE)
mean_steps
```

```
## [1] 37.38
```

```r
median_steps <- median(data$steps, na.rm = TRUE)
median_steps
```

```
## [1] 0
```


## What is the average daily activity pattern?

1. Time series plot of the 5-minute interval and the average number of steps taken, averaged across all days


```r
library(data.table)
dt <- data.table(data)
mean_across_days <- dt[, list(mean = mean(steps, na.rm = TRUE)), by = interval]
with(mean_across_days, plot(interval, mean, type = "l"))
```

![plot of chunk average_daily_activity_pattern](figure/average_daily_activity_pattern.png) 


2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?


```r
mean_across_days[which.max(mean_across_days$mean), ][["interval"]]
```

```
## [1] 835
```


## Imputing missing values

1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)


```r
sum(is.na(data))
```

```
## [1] 2304
```


2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

3. Create a new dataset that is equal to the original dataset but with the missing data filled in.


```r
imputed_data <- data
na_data <- is.na(imputed_data)
na_intervals <- imputed_data$interval[na_data]
imputed_data[na_data] <- sapply(na_intervals, function(x) mean_across_days[interval == 
    x, ][["mean"]])
sum(is.na(imputed_data))
```

```
## [1] 0
```



```r
with(imputed_data, hist(steps))
```

![plot of chunk histogram_steps_imputed_data](figure/histogram_steps_imputed_data.png) 

```r
mean(imputed_data$steps)
```

```
## [1] 37.38
```

```r
median(imputed_data$steps)
```

```
## [1] 0
```


As you can see, the mean and median of the raw and imputted data remains the same.
Now let's look at the number of steps:


```r
sum(data$steps, na.rm = TRUE)
```

```
## [1] 570608
```

```r
sum(imputed_data$steps)
```

```
## [1] 656738
```


As expected, the number of steps for the imputed data is more than the raw data.

## Are there differences in activity patterns between weekdays and weekends?


```r
imputed_data$weekdays <- weekdays(imputed_data$date) %in% c("Saturday", "Sunday")
imputed_data$weekfactors <- factor(imputed_data$weekdays, labels = c("weekday", 
    "weekend"), levels = c(FALSE, TRUE))
```



```r
dt_imputed <- data.table(imputed_data)

data_to_plot <- dt_imputed[dt_imputed$weekfactors == "weekday", list(steps = mean(steps), 
    weekfactors = "weekday"), by = interval]

data_to_plot <- rbind(data_to_plot, dt_imputed[dt_imputed$weekfactors == "weekend", 
    list(steps = mean(steps), weekfactors = "weekend"), by = interval])

head(data_to_plot)
```

```
##    interval   steps weekfactors
## 1:        0 2.25115     weekday
## 2:        5 0.44528     weekday
## 3:       10 0.17317     weekday
## 4:       15 0.19790     weekday
## 5:       20 0.09895     weekday
## 6:       25 1.59036     weekday
```

```r

library(lattice)
xyplot(steps ~ interval | weekfactors, data = data_to_plot, type = "l", horizontal = TRUE, 
    layout = c(1, 2))
```

![plot of chunk weekday_weekend_pattern](figure/weekday_weekend_pattern.png) 

