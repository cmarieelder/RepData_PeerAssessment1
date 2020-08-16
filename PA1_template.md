---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---


## Loading and preprocessing the data

### 1. Load the data (i.e. read.csv())
### 2. Process/transform the data (if necessary) into a format suitable for your analysis

```r
list_of_packages <- c("data.table", "lubridate", "dplyr", "knitr")
lapply(list_of_packages, library, character.only = TRUE)

opts_chunk$set(echo = TRUE)
```


```r
data_dir = "activity"
if (!dir.exists(data_dir)) {
    unzip(paste0(data_dir, ".zip"), overwrite = FALSE, exdir = data_dir)
}

activity <- data.table(fread(file = paste0(data_dir, "/activity.csv"))) %>%
    mutate(date = ymd(date))
```


## What is mean total number of steps taken per day?

### 1. Calculate the total number of steps taken per day

```r
daily_steps = with(activity, tapply(steps, date, sum, na.rm = TRUE))
```

### 2. Make a histogram of the total number of steps taken each day

```r
hist(daily_steps, main = "Total Number of Steps Taken Each Day",
     xlab = "Daily total steps", col = "cyan3")
```

![](PA1_template_files/figure-html/steps_hist-1.png)<!-- -->

### 3. Calculate and report the mean and median of the total number of steps taken per day

```r
mean_daily = mean(daily_steps)
print(mean_daily)
```

```
## [1] 9354.23
```

```r
median_daily = median(daily_steps)
print(median_daily)
```

```
## [1] 10395
```


## What is the average daily activity pattern?

### 1. Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

```r
avg_steps = with(activity, tapply(steps, interval, mean, na.rm = TRUE))
avg_steps_df <- data.frame(interval = as.numeric(names(avg_steps)), avg_steps=avg_steps)
with(avg_steps_df, plot(interval, avg_steps, type = "l", col = "blue",
                        xlab = "Interval", ylab = "Number of steps",
                        main = "Average Steps Per 5-min Interval (Across All Days)"))
```

![](PA1_template_files/figure-html/time_series-1.png)<!-- -->

### 2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```r
print(avg_steps_df$interval[which.max(avg_steps_df$avg_steps)])
```

```
## [1] 835
```


## Imputing missing values

### 1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)

```r
print(sum(!complete.cases(activity)))
```

```
## [1] 2304
```

### 2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.
Missing values will be imputed with the mean for the 5-minute interval.

### 3. Create a new dataset that is equal to the original dataset but with the missing data filled in.

```r
activity_imputed <- data.frame(activity)
activity_imputed <- merge(activity_imputed, avg_steps_df)

activity_imputed$steps <- ifelse(test = is.na(activity_imputed$steps),
                                 yes = activity_imputed$avg_steps,
                                 no = activity_imputed$steps)
activity_imputed$avg_steps = NULL
```

### 4. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

```r
daily_steps_imputed = with(activity_imputed, tapply(steps, date, sum, na.rm = TRUE))
hist(daily_steps_imputed, main = "Total Number of Steps Taken Each Day (Imputed)", xlab = "Daily total steps", col = "cyan3")
```

![](PA1_template_files/figure-html/imputed_comparison-1.png)<!-- -->

```r
mean_daily_imputed = mean(daily_steps_imputed)
print(mean_daily_imputed)
```

```
## [1] 10766.19
```

```r
print(paste("Difference in mean is:", 
            round(mean_daily - mean_daily_imputed, 2)))
```

```
## [1] "Difference in mean is: -1411.96"
```

```r
median_daily_imputed = median(daily_steps_imputed)
print(median_daily_imputed)
```

```
## [1] 10766.19
```

```r
print(paste("Difference in median is:", 
            round(median_daily - median_daily_imputed, 2)))
```

```
## [1] "Difference in median is: -371.19"
```


## Are there differences in activity patterns between weekdays and weekends?

### 1. Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.

```r
activity_imputed$weekday <- factor(
    ifelse(weekdays(activity_imputed$date, abbreviate = TRUE) %in% 
               c("Sun", "Sat"), "weekend", "weekday"))
```

### 2. Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.

```r
weekend_steps = with(subset(activity_imputed, weekday == "weekend"), 
                     tapply(steps, interval, mean, na.rm = TRUE))
weekday_steps = with(subset(activity_imputed, weekday == "weekday"), 
                     tapply(steps, interval, mean, na.rm = TRUE))
week_steps_df <- data.frame(interval = as.numeric(names(weekday_steps)),
                            weekday_steps=weekday_steps,
                            weekend_steps=weekend_steps)

par(mfcol = c(2, 1), mar = c(4, 4, 4, 1), oma = c(0, 0, 2, 0))
with(week_steps_df, {
     plot(interval, weekend_steps, ylim = c(0,250), type = "l", col = "blue",
          xlab = "Interval", ylab = "Number of steps", main = "weekend")
     plot(interval, weekday_steps, ylim = c(0,250), type = "l", col = "blue", 
          xlab = "Interval", ylab = "Number of steps", main = "weekday")
     mtext("Average Steps Per 5-min Interval (Across All Days)", outer = TRUE,
           adj = 0.6)
     })
```

![](PA1_template_files/figure-html/weekday_plots-1.png)<!-- -->
