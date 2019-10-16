library(lubridate)
library(reshape2)
library(zoo)
library(ggplot2)

setwd("~/Developer/my-life-data/PersonalFinance")

# TODO - refactor and make plot prettier

data = read.csv("data.csv")
data$balance = as.numeric(gsub(',', '', as.character(data$balance)))
data$date = as.Date(data$date, "%m/%d/%y")
data = data[order(data$date),]
data$date = format(data$date, "%Y-%m")
dataLong = data
data = reshape(data, timevar='account', idvar='date', direction='wide')
rownames(data) = data$date
colnames(data) = c("date", "BOA_Checkings", "BOA_Savings", "BOA_IRA", "C1_Savings", "Fed_401k")
data$BOA_IRA[1] = 0
data$BOA_IRA = na.locf(data$BOA_IRA)
data$C1_Savings[1] = 0
data$C1_Savings = na.locf(data$C1_Savings)
data$Fed_401k[1] = 0
data$Fed_401k = na.locf(data$Fed_401k)

dataSubset = t(subset(data, select=-c(date)))
labels = c("BOA_Checkings", "BOA_Savings", "BOA_IRA", "C1_Savings", "401k")
colors = c('red', 'yellow', 'purple', 'cyan', 'green')
barplot(as.matrix(dataSubset), col = colors, xlab="Date", ylab="Balance", main = "Bank Accounts Over Time", ylim=c(0,60000))
legend("topleft", labels, cex = 1, fill = colors, bty = "n")


myplot = ggplot(dataLong, aes(x = as.factor(date), y = balance, fill = account)) + geom_area(position = 'stack') + ylim(0,60000)
print(myplot)

gg <- ggplot(dataLong, aes(x=date, y=balance))
gg <- gg + geom_area(aes(colour=account, fill=account))
gg <- gg + scale_x_discrete(labels=levels(dataLong$date))
gg
