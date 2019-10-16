library(reshape2)
library(ggplot2)

setwd("~/Developer/data-visualization-projects/WorkEmails")

inbox = read.csv('inbox_data.csv')
sent = read.csv('sent_data.csv')

aggregate_by_hour = function(data) {
  data$count = 1
  sum_by_hour = aggregate(data[c("count")], by=list(hour=data$hour), FUN=sum)
  total = sum(sum_by_hour$count)
  sum_by_hour$percent = (sum_by_hour$count/total)*100
  sum_by_hour
}

inbox_hourly = aggregate_by_hour(inbox)

sent_hourly = aggregate_by_hour(sent)
sent_hourly = rbind(sent_hourly, data.frame(hour=2,count=0,percent=0))
sent_hourly = sent_hourly[order(sent_hourly$hour),]

combo_hourly_percent = data.frame(hour=inbox_hourly$hour, received=inbox_hourly$percent, sent=sent_hourly$percent)
combo_hourly_percent_melt =  melt(combo_hourly_percent, id.vars = "hour")

ggplot(combo_hourly_percent_melt, aes(x = hour, y = value,fill=variable)) +
  scale_x_continuous(breaks=c(0,6,12,18), labels=c(0,6,12,18)) +
  geom_bar(stat='identity') + coord_polar()
