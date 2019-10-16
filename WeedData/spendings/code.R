setwd("~/Developer/data-visualization-projects/WeedSpendings")

library(ggplot2)
library(stats)

# read data
data = read.csv("data.csv")
firstDate = as.Date("2016-07-22")

# modify data
data$card = NULL
data = data[order(data$date),]
data$total = cumsum(data$amount)
data$daysSince = as.numeric(as.Date(data$date) - rep(firstDate, length(data$date)))
data$daysSinceFirst = as.numeric(as.Date(data$date) - rep(as.Date(data$date[1]), length(data$date)))

# plot
plotData = data.frame(x0=data$daysSinceFirst, x1=data$daysSince, y=data$total)

xscale = seq(0,max(plotData$x1),5*7)
yscale = seq(0,max(plotData$y),250)

coeff = coefficients(lm(plotData$y~plotData$x1))
slope = coeff[2]
intercept = coeff[1]

numDaysNewYear = 163

ggplot(data=plotData) +
  geom_vline(xintercept=numDaysNewYear) +
  geom_text(x=numDaysNewYear+25, y=max(plotData$y), label="2017") +
  geom_text(x=numDaysNewYear-21, y=max(plotData$y), label="2016") +
  geom_abline(intercept = intercept, slope = slope, color="red", linetype="dashed", size=1.5) +
  geom_point(aes(x=x1, y=y), color="blue", size=3.5) +
  geom_label(x=45*7, y=375, label=paste0("Average ~$", round(slope,1), "/day"), color="green", size=5, label.size=0.5) +
  scale_x_continuous(limits=c(0,max(plotData$x1)), breaks=xscale, minor_breaks=NULL, labels=xscale/7) +
  scale_y_continuous(breaks=yscale, labels=yscale) +
  labs(
    title = "Cumulative Spendings on Weed",
    subtitle = "Starting from \"First day of Real Life\"!",
    x = "Number of Weeks Since 7/22/16",
    y = "Amount Spent (USD)"
  )
ggsave("plot.png", width=8, height=6, units="in", dpi=1000)

###############################################################################
# (Experimental) Fill Missing Rows
###############################################################################
fillRows = function(startDate, endDate, startVal, endVal, trim) {
  dates = seq(from=startDate, to=endDate, by=1)
  
  vals = rep(startVal, length(dates)-1)
  vals = c(vals, endVal)
  
  if (trim) {
    dates = dates[-1]
    vals = vals[-1]
  }
  
  temp = data.frame(date=dates, total=vals)
  temp
}

dataLong = data.frame(row.names = c("date", "total"))
for (i in 1:(length(data$date)-1)) {
  startDate = as.Date(data$date[i])
  endDate = as.Date(data$date[i+1])
  
  startVal = data[i,3]
  endVal = data[i+1,3]
  
  temp = c()
  if (i > 1) {
    temp = fillRows(startDate, endDate, startVal, endVal, TRUE)
  } else {
    temp = fillRows(startDate, endDate, startVal, endVal, FALSE)
  }
  dataLong = rbind(dataLong, temp)
}

# prepend start dates
firstDate = as.Date("2016-07-22")
firtVal = 0
startDate = as.Date(data[1,1])
startVal = data[1,3]
prepend = fillRows(firstDate, startDate, firtVal, startVal, FALSE)
prepend = prepend[-nrow(prepend),]
dataLong = rbind(prepend, dataLong)

# append end dates
endDate = as.Date(data[length(data$date),1])
endVal = data[length(data$date),3]
lastDate = today()
append = fillRows(endDate, lastDate, endVal, endVal, TRUE)
dataLong = rbind(dataLong, append)

# days since
dataLong$daysSince = 0:(nrow(dataLong)-1)
