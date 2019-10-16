setwd("~/Developer/data-visualization-projects/WeedData/times")

library(ggplot2)
library(stats)

# read data
data = read.csv("times.csv")
colnames(data) = c("hour")
data$count = 1

# aggregate count by hour
agg_data = aggregate(data$count, by=list(hour=data$hour), FUN=sum)

# fill in missing hours with 0
for (i in 0:23) {
  if (i %in% agg_data$hour) {} 
  else { agg_data = rbind(agg_data, c(i, 0)) }
}

# order
agg_data = agg_data[order(agg_data$hour),]
agg_data$hour = factor(agg_data$hour, levels = rev(agg_data$hour))

agg_data$percent = (agg_data$x/sum(agg_data$x)) * 100
###############################################################################
# PLOTTING
###############################################################################

ggplot(data = agg_data, aes(x=hour, y='')) + 
  geom_tile(aes(fill=percent)) +
  scale_fill_gradient(low="white", high="green") +
  guides(alpha="none", fill="none") +
  theme(
    panel.grid.major=element_blank(),
    panel.grid.minor=element_blank(),
    panel.background=element_blank(),
    panel.border=element_rect(color="black", fill=NA)
  ) + coord_flip()

ggsave("times.png", height=15, width=5, dpi=300)






