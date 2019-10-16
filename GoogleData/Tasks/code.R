library(lubridate)
library(lattice)

setwd("~/Desktop/MyProject/GoogleData/Tasks")

data = read.csv('data.csv')
data$date = as.Date(data$date)
data$count = 1
sum_by_day = aggregate(data[c("count")], by=list(date=data$date), FUN=sum)

#######################################
# Daily Avg By Week Bar Plot
#######################################
avg_by_week = data.frame(tapply(sum_by_day$count, paste(year(sum_by_day$date), "-W", sprintf("%02d", week(sum_by_day$date)), sep=""), mean))
avg_by_week = t(avg_by_week)
rownames(avg_by_week) = "avg_tasks"
week_plot = barplot(avg_by_week, col=c('cyan'), main="Daily Average Completed Tasks by Week", xlab="Week", ylab="Average Completed Tasks")

#######################################
# Daily Avg By Month Bar Plot
#######################################
avg_by_month = data.frame(tapply(sum_by_day$count, paste(year(sum_by_day$date), sprintf("%02d", month(sum_by_day$date)), sep="-"), mean))
avg_by_month = t(avg_by_month)
rownames(avg_by_month) = "avg_tasks"
barplot(avg_by_month, col=c('green'), main="Daily Average Completed Tasks by Month", xlab="Month", ylab="Average Completed Tasks")

#######################################
# DOW Avg By Month Grouped Bar Plot
#######################################
sum_by_day_dow = sum_by_day
sum_by_day_dow$dow = weekdays(sum_by_day_dow$date)
sum_by_day_dow$yearmon =paste(year(sum_by_day_dow$date), sprintf("%02d", month(sum_by_day_dow$date)), sep="-")
dow_by_month_mean = aggregate(sum_by_day_dow[c("count")], by=list(yearmon=sum_by_day_dow$yearmon, dow=sum_by_day_dow$dow), FUN=mean)
barchart(count ~ yearmon, groups=dow, dow_by_month_mean, horizontal = FALSE,
         xlab="Month", scales=list(x=list(rot=90)), ylab="Tasks", ylim=c(0,18), main="Tasks",
         auto.key=list(size = 1, columns = 7, text=c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"), cex.title=1, title=""))
