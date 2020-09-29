### Step 1

### Code for reading in the dataset and/or processing the data

    library(data.table)
    library(ggplot2)
    library(dplyr)

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:data.table':
    ## 
    ##     between, first, last

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

    activity <- fread("C:/Users/Ben/Downloads/repdata_data_activity/activity.csv")
    activity <- activity[, date := as.Date(date)]

### What is mean total number of steps taken per day?

    mean_steps <- mean(activity[,steps], na.rm = TRUE)
    #activity <- activity[,day_week := weekdays(activity[,date])]
    steps_per_day <- activity[, .(steps_day = sum(steps)),by = date]
    ggplot(data = steps_per_day, aes(x=steps_day )) +
          geom_histogram(boundary=0, binwidth = 2500, fill="green4",col="black") +
          labs(title = "Total number of steps taken per day", x = "Total steps taken per day") 

    ## Warning: Removed 8 rows containing non-finite values (stat_bin).

![](PA1_template--md-_files/figure-markdown_strict/unnamed-chunk-1-1.png)

    mean_steps_per_day <- mean(steps_per_day[,steps_day], na.rm = TRUE)
    print(mean_steps_per_day)

    ## [1] 10766.19

    median_steps_per_day <- median(steps_per_day[,steps_day],na.rm = TRUE)
    print(median_steps_per_day)

    ## [1] 10765

### What is the average daily activity pattern?

    average_daily_activity <- activity[,.(average_activity=mean(steps, na.rm=TRUE)), by =interval]
    plot(x=average_daily_activity[,interval], y =average_daily_activity[,average_activity],
         type = "l", col="darkred", lwd = 2, xlab="Interval", ylab="Average number of steps",
         main="Average number of steps per intervals")

![](PA1_template--md-_files/figure-markdown_strict/unnamed-chunk-2-1.png)

    average_daily_activity[which.max(average_daily_activity[,average_activity]),.(interval, average_activity)]

    ##    interval average_activity
    ## 1:      835         206.1698

### Imputing missing values

    activity[!complete.cases(activity), .N]

    ## [1] 2304

    activity2 <- copy(activity)
    setkey(activity2,interval)
    mean_steps_intrval <- activity2[!is.na(steps), .(steps = round(mean(steps),0)), by = interval]
    setkey(mean_steps_intrval, interval)
    setkey(activity2,interval) 
    activity2 <- activity2[is.na(steps), steps := mean_steps_intrval[.(.SD),steps]][order(date)]
    head(activity2,10)

    ##     steps       date interval
    ##  1:     2 2012-10-01        0
    ##  2:     0 2012-10-01        5
    ##  3:     0 2012-10-01       10
    ##  4:     0 2012-10-01       15
    ##  5:     0 2012-10-01       20
    ##  6:     2 2012-10-01       25
    ##  7:     1 2012-10-01       30
    ##  8:     1 2012-10-01       35
    ##  9:     0 2012-10-01       40
    ## 10:     1 2012-10-01       45

    steps_per_day2 <- activity2[,.(steps_day=sum(steps)),by =date]
    ggplot(data = steps_per_day2, aes(x=steps_day)) +
          geom_histogram(boundary=0, binwidth = 2500, fill="blue4",col="black") +
          labs(title = "Total number of steps taken per day", x = "Total steps taken per day") 

![](PA1_template--md-_files/figure-markdown_strict/unnamed-chunk-3-1.png)

    mean_steps_per_day2 <- steps_per_day2[,mean(steps_day)]
    print(mean_steps_per_day2)

    ## [1] 10765.64

    median_steps_per_day2 <- steps_per_day2[,median(steps_day)]
    print(median_steps_per_day2)

    ## [1] 10762

### Are there differences in activity patterns between weekdays and weekends?

    activity2[, weekday := weekdays(date)]
    activity2[weekday %in% c("Sunday","Saturday"),daytype := "weekend"]
    activity2[is.na(daytype),daytype := 'weekday']
    steps_by <- activity2[,.(average_steps=mean(steps)),by =.(interval,daytype)]
    ggplot(data = steps_by, aes(x=interval,y =average_steps, color = daytype)) + 
       geom_line() + facet_grid(daytype~.) + scale_colour_manual(values =c("goldenrod4","darkviolet"))+
          ggtitle("Average steps per time interval: weekdays vs. weekends")+
             xlab("interval") + ylab("average steps") 

![](PA1_template--md-_files/figure-markdown_strict/unnamed-chunk-4-1.png)
