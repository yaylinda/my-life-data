setwd("~/Developer/data-visualization-projects/MomentData")

library(ggplot2)
library(reshape2)
library(lubridate)

###############################################################################
# date, num_pickups, num_sessions
###############################################################################
daily_num = read.csv("daily_pickups_min.csv")
daily_num$date = as.Date(daily_num$date)

ggplot(data = daily_num, aes(x=date, y=num_minutes)) +   
  geom_area()

daily_num_melt = melt(daily_num, measure.vars=c("num_pickups", "num_minutes"))

ggplot(data = daily_num_melt, aes(x=date, y=value)) +   
  geom_bar(aes(fill = variable), position = "dodge", stat="identity")

###############################################################################
# date, time, session_min
###############################################################################
daily_session = read.csv("daily_session_min.csv")
daily_session$date = as.Date(daily_session$date)
daily_session$dow = weekdays(daily_session$date)
daily_session$hour = hour(hms(daily_session$time))
daily_session$hourP = daily_session$hour
for (i in 1:length(daily_session$hour)) {
  time = daily_session$hour[i]
  if (time == 0) {
    daily_session$hourP[i] = "12:00 AM"
  } else if (time < 12) {
    daily_session$hourP[i] = paste0(time, ":00 AM")
  } else if (time == 12) {
    daily_session$hourP[i] = "12:00 PM"
  } else if (time > 12) {
    daily_session$hourP[i] = paste0(as.numeric(time)-12, ":00 PM")
  }
}

daily_session$session_min[which(daily_session$session_min == 464)] = 10
daily_session$session_min[which(daily_session$session_min == 170)] = 10

###############################################################################
# agg_hour_dow_daily_session
###############################################################################
agg_hour_dow_daily_session = aggregate(x=daily_session$session_min, by=list(daily_session$dow, daily_session$hourP), FUN="sum")
agg_hour_dow_daily_session$Group.1 = factor(agg_hour_dow_daily_session$Group.1, levels=c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))
agg_hour_dow_daily_session$Group.2 = factor(agg_hour_dow_daily_session$Group.2, levels=rev(c("12:00 AM", "1:00 AM", "2:00 AM", "3:00 AM", "4:00 AM", "5:00 AM", "6:00 AM", "7:00 AM", "8:00 AM", "9:00 AM", "10:00 AM", "11:00 AM", "12:00 PM", "1:00 PM", "2:00 PM", "3:00 PM", "4:00 PM", "5:00 PM", "6:00 PM", "7:00 PM", "8:00 PM", "9:00 PM", "10:00 PM", "11:00 PM")))

ggplot(data=agg_hour_dow_daily_session, aes(x=Group.1, y=Group.2))+
  geom_tile(aes(fill=x), color="white") + 
  scale_fill_gradient(low = "white", high = "royalblue") +
  theme(
    panel.grid.major=element_blank(),
    panel.grid.minor=element_blank(),
    panel.background=element_blank(),
    panel.border=element_rect(color="black", fill=NA)
  )

ggsave("heatmap-hours-dow.png", width=6, height=6, units="in", dpi=1000)

###############################################################################
# agg_hour_daily_session
###############################################################################

agg_hour_daily_session = aggregate(x=daily_session$session_min, by=list(daily_session$hourP), FUN="sum")
agg_hour_daily_session$Group.1 = factor(agg_hour_daily_session$Group.1, levels=rev(c("12:00 AM", "1:00 AM", "2:00 AM", "3:00 AM", "4:00 AM", "5:00 AM", "6:00 AM", "7:00 AM", "8:00 AM", "9:00 AM", "10:00 AM", "11:00 AM", "12:00 PM", "1:00 PM", "2:00 PM", "3:00 PM", "4:00 PM", "5:00 PM", "6:00 PM", "7:00 PM", "8:00 PM", "9:00 PM", "10:00 PM", "11:00 PM")))

# heatmap
ggplot(data=agg_hour_daily_session, aes(y=Group.1, x=""))+
  geom_tile(aes(fill=x), color="white") + 
  scale_fill_gradient(low = "white", high = "royal blue") +
  theme(
    panel.grid.major=element_blank(),
    panel.grid.minor=element_blank(),
    panel.background=element_blank(),
    panel.border=element_rect(color="black", fill=NA)
  )

ggsave("heatmp-hours.png", width=3, height=6, units="in", dpi = 1000)


#polar map
agg_hour_daily_session$Group.1 = factor(agg_hour_daily_session$Group.1, levels=c("12:00 AM", "1:00 AM", "2:00 AM", "3:00 AM", "4:00 AM", "5:00 AM", "6:00 AM", "7:00 AM", "8:00 AM", "9:00 AM", "10:00 AM", "11:00 AM", "12:00 PM", "1:00 PM", "2:00 PM", "3:00 PM", "4:00 PM", "5:00 PM", "6:00 PM", "7:00 PM", "8:00 PM", "9:00 PM", "10:00 PM", "11:00 PM"))

ggplot(data=agg_hour_daily_session, aes(x=Group.1, y=x))+
  geom_bar(stat = "identity", color="white", fill="blue") + 
  theme(
    panel.grid.major=element_blank(),
    panel.grid.minor=element_blank(),
    panel.background=element_blank(),
    panel.border=element_rect(color="black", fill=NA)
  ) + coord_polar()

ggsave("polar-hours.png", width=6, height=6, units="in", dpi=1000)








