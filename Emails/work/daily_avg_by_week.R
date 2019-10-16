library('plyr')
library('xts')
library('lubridate')
library('MASS')

setwd("~/Developer/data-visualization-projects/WorkEmails")

get_daily_average_by_week = function(data) {
  data$count = 1
  sum_by_day = aggregate(data[c("count")], by=list(year=data$year, month=data$month, day=data$day), FUN=sum)
  sum_by_day = sum_by_day[order(sum_by_day[,1], sum_by_day[,2], sum_by_day[,3]), ]
  sum_by_day['cumsum'] = cumsum(sum_by_day[,4])
  sum_by_day['date'] = as.Date(paste(sum_by_day$year, sum_by_day$month, sum_by_day$day, sep="-"))
  plot(sum_by_day$date, sum_by_day$cumsum, type='h')
  plot(sum_by_day$date, sum_by_day$count, type='h')
  date_count_df = data.frame(sum_by_day$date, sum_by_day$count)
  weekly_avg = data.frame(tapply(date_count_df[,2], week(date_count_df[,1]), mean))
  weekly_avg = data.frame(c(weekly_avg[8:dim(weekly_avg)[1],], weekly_avg[1:7,]))
  weekly_avg['weeknum'] = paste('week', rownames(weekly_avg), sep='')
  weekly_avg
}

inbox = read.csv('inbox_data.csv')
inbox_weekly = get_daily_average_by_week(inbox)

sent = read.csv('sent_data.csv')
sent_weekly = get_daily_average_by_week(sent)
sent_weekly = rbind(c(0, "week28"), sent_weekly) 
sent_weekly = rbind(c(0, "week27"), sent_weekly) 
sent_weekly = rbind(c(0, "week26"), sent_weekly) 

combined_weekly = rbind("inbox"=input_weekly[,1], "sent"=sent_weekly[,1])

colors = c('cyan', 'pink')
labels = c('recieved', 'sent')
barplot(combined_weekly, names.arg = input_weekly[,2], 
        xlab="Week of Year", ylab="Average Emails per Day", main = "Average Daily Emails by Week",
        col = colors)

legend("topleft", labels, cex = 1, fill = colors, bty = "n")
