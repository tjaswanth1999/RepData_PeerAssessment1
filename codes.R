activity_data<-read.csv("activity.csv",header = TRUE)
tail(activity_data)
dates<-activity_data$date
num_data_steps<-activity_data[grep("^[0-9]",activity_data$steps),]
num_data_steps
sum_data_steps<-tapply(num_data_steps$steps,num_data_steps$date, sum)
sum_data_steps
day_wise_data<-data.frame(date=unique(num_data_steps$date),steps=sum_data_steps)
day_wise_data
hist(day_wise_data$steps,xlab = "total steps per day",ylab="frequency of no.of days ",main = "histogram of steps for each day",breaks = 20,col = "red")
rug(day_wise_data$steps)
mean_data<-mean(sum_data_steps)
mean_data
median_data<-median(day_wise_data$steps)
median_data
interval_steps_sum<-aggregate(num_data_steps$steps,by=list(num_data_steps$interval),mean)
colnames(interval_steps_sum)<-c("interval","steps")
plot(interval_steps_sum$interval,interval_steps_sum$steps,type="l",ylab = "no.of steps taken",xlab="intarval",main="average number of steps for every 5-minute interval")
interval_steps_sum[max(interval_steps_sum$steps)==interval_steps_sum$steps,]
number_of_missing_values<-sum(is.na(activity_data))
number_of_missing_values
missing_data<-activity_data[is.na(activity_data),]
missing_data$steps<-ifelse(missing_data$interval==interval_steps_sum$interval,as.integer(interval_steps_sum$steps))
missing_data
activity_data_without_NA<-rbind(num_data_steps,missing_data)
activity_data_without_NA
sum(is.na(activity_data_without_NA))
sum_data_steps_without_na<-tapply(activity_data_without_NA$steps,activity_data_without_NA$date,sum)
hist(sum_data_steps_without_na,xlab = "total steps per day",ylab="frequency of no.of days ",main = "histogram of steps for each day with data without NA'S ",breaks = 20,col = "blue")
rug(day_wise_data$steps)
mean(sum_data_steps_without_na)
median(sum_data_steps_without_na)
sprintf("mean of data with NA's in it: %i",as.integer(mean(sum_data_steps)))
sprintf("median of data with NA's in it: %i",as.integer(median(sum_data_steps)))
sprintf("mean of data with no missing values(all missing values imputed: %i",as.integer(mean(sum_data_steps_without_na)))
sprintf("median of data with no missing values(all missing values imputed: %i",as.integer(median(sum_data_steps_without_na)))
activity_data_without_NA$dayofweek <- weekdays(as.Date(activity_data_without_NA$date))
activity_data_without_NA$weekend <-as.factor(activity_data_without_NA$dayofweek=="Saturday"|activity_data_without_NA$dayofweek=="Sunday")
levels(activity_data_without_NA$weekend) <- c("Weekday", "Weekend")
activity_data_weekday<-activity_data_without_NA[activity_data_without_NA$weekend=="Weekday",]
activity_data_weekday
activity_data_weekend<-activity_data_without_NA[activity_data_without_NA$weekend=="Weekend",]
activity_data_weekend
interval_steps_sum_weekday<-aggregate(activity_data_weekday$steps,by=list(activity_data_weekday$interval),mean)
interval_steps_sum_weekday$weekend<-rep("Weekday",length(interval_steps_sum_weekday))
interval_steps_sum_weekday
interval_steps_sum_weekend<-aggregate(activity_data_weekend$steps,by=list(activity_data_weekend$interval),mean)
interval_steps_sum_weekend$weekend<-rep("Weekend",length(interval_steps_sum_weekend))
interval_steps_sum_merged<-rbind(interval_steps_sum_weekday,interval_steps_sum_weekend)
colnames(interval_steps_sum_merged)<-c("interval","steps","weekend")
library(ggplot2)
ggplot(interval_steps_sum_merged,aes(x = interval,y=steps))+geom_line()+facet_grid(weekend~.)+ggtitle("mean of steps for every 5-min interval in weekend and weekdays")
