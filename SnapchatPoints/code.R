setwd("~/Developer/data-visualization-projects/SnapchatPoints")

library(ggplot2)

data = read.csv("data.csv")

agg_gender = aggregate(data$Score, by=list(gender=data$Gender, age=data$Age.Range), FUN=mean)

ggplot(data=agg_gender)
agg_gender[7,]
