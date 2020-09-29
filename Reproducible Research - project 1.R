library(data.table)
library(ggplot2)
library(dplyr)
activity <- fread("C:/Users/Ben/Downloads/repdata_data_activity/activity.csv")
activity <- activity[, date := as.Date(date)]

mean_steps <- mean(activity[,steps], na.rm = TRUE)
#activity <- activity[,day_week := weekdays(activity[,date])]
steps_per_day <- activity[, .(steps_day = sum(steps)),by = date]
ggplot(data = steps_per_day, aes(x=steps_day )) +
      geom_histogram(boundary=0, binwidth = 2500, fill="green4",col="black") +
      labs(title = "Total number of steps taken per day", x = "Total steps taken per day") 
mean_steps_per_day <- mean(steps_per_day[,steps_day], na.rm = TRUE)
print(mean_steps_per_day)
median_steps_per_day <- median(steps_per_day[,steps_day],na.rm = TRUE)
print(median_steps_per_day)

average_daily_activity <- activity[,.(average_activity=mean(steps, na.rm=TRUE)), by =interval]
plot(x=average_daily_activity[,interval], y =average_daily_activity[,average_activity],
     type = "l", col="darkred", lwd = 2, xlab="Interval", ylab="Average number of steps",
     main="Average number of steps per intervals")
average_daily_activity[which.max(average_daily_activity[,average_activity]),.(interval, average_activity)]

mean_steps <- mean(activity[,steps], na.rm = TRUE)
#activity <- activity[,day_week := weekdays(activity[,date])]
steps_per_day <- activity[, .(steps_day = sum(steps)),by = date]
ggplot(data = steps_per_day, aes(x=steps_day )) +
      geom_histogram(boundary=0, binwidth = 2500, fill="green4",col="black") +
      labs(title = "Total number of steps taken per day", x = "Total steps taken per day") 
mean_steps_per_day <- mean(steps_per_day[,steps_day], na.rm = TRUE)
print(mean_steps_per_day)
median_steps_per_day <- median(steps_per_day[,steps_day],na.rm = TRUE)
print(median_steps_per_day)

average_daily_activity <- activity[,.(average_activity=mean(steps, na.rm=TRUE)), by =interval]
plot(x=average_daily_activity[,interval], y =average_daily_activity[,average_activity],
     type = "l", col="darkred", lwd = 2, xlab="Interval", ylab="Average number of steps",
     main="Average number of steps per intervals")
average_daily_activity[which.max(average_daily_activity[,average_activity]),.(interval, average_activity)]

activity[!complete.cases(activity), .N]
activity2 <- copy(activity)
setkey(activity2,interval)
mean_steps_intrval <- activity2[!is.na(steps), .(steps = round(mean(steps),0)), by = interval]
setkey(mean_steps_intrval, interval)
setkey(activity2,interval) 
activity2 <- activity2[is.na(steps), steps := mean_steps_intrval[.(.SD),steps]][order(date)]
head(activity2,10)
steps_per_day2 <- activity2[,.(steps_day=sum(steps)),by =date]
ggplot(data = steps_per_day2, aes(x=steps_day)) +
      geom_histogram(boundary=0, binwidth = 2500, fill="blue4",col="black") +
      labs(title = "Total number of steps taken per day", x = "Total steps taken per day") 
mean_steps_per_day2 <- steps_per_day2[,mean(steps_day)]
print(mean_steps_per_day2)

activity2[, weekday := weekdays(date)]
activity2[weekday %in% c("Sunday","Saturday"),daytype := "weekend"]
activity2[is.na(daytype),daytype := 'weekday']
steps_by <- activity2[,.(average_steps=mean(steps)),by =.(interval,daytype)]
ggplot(data = steps_by, aes(x=interval,y =average_steps, color = daytype)) + 
      geom_line() + facet_grid(daytype~.) + scale_colour_manual(values =c("goldenrod4","darkviolet"))+
      ggtitle("Average steps per time interval: weekdays vs. weekends")+
      xlab("interval") + ylab("average steps")