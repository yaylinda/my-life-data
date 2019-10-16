library('plyr')
library('xts')
library('lubridate')
library('MASS')
library('zoo')
library('lattice')
library('UsingR')

setwd("~/Developer/data-visualization-projects/WorkEmails")

aggregate_weekday = function(data) {
  data$count = 1
  sum_by_day = aggregate(data[c("count")], by=list(year=data$year, month=data$month, day=data$day, dow=data$dow), FUN=sum)
  sum_by_day = sum_by_day[order(sum_by_day[,1], sum_by_day[,2], sum_by_day[,3]), ]
  sum_by_day['date'] = as.yearmon(paste(sum_by_day$year, sum_by_day$month, sep="-"))
  aggregated_sum = aggregate(sum_by_day[c("count")], by=list(date=sum_by_day$date, dow=sum_by_day$dow), FUN=mean)
  aggregated_sum
}

do_barchart = function(data, type) {
  barchart(count ~ date, groups=dow, data, horizontal = FALSE,
           xlab="Month", ylab=paste("Averge Emails", type, sep=" "), ylim=c(0,40), main=paste("Average Emails", type, "by DOW per Month", sep=" "),
           auto.key=list(size = 1, columns = 7, text=c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"), cex.title=10, title="")) 
}

inbox = read.csv('inbox_data.csv')
inbox_monthy = aggregate_weekday(inbox)

sent = read.csv('sent_data.csv')
sent_monthy = aggregate_weekday(sent)

par(mfrow=c(2,1))
a = do_barchart(input_monthy, "Recieved")
b = do_barchart(sent_monthy, "Sent")
print(a, position = c(0, 0, 0.5, 1), more = TRUE)
print(b, position = c(0.5, 0, 1, 1))