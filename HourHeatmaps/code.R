setwd("~/Developer/my-life-data/HourHeatmaps")

library(reshape2)
library(ggplot2)
library(zoo)
library(anytime)
library(zoo)

###############################################################################
# Read and clean data
###############################################################################

# google searches
json_files = list.files(path = paste(getwd(), "/searches", sep = ""), pattern = "*.json", full.names = TRUE)
all_google = NULL
for (file in json_files) {
  temp_data = fromJSON(json_files[i])
  temp_data = temp_data$event$query
  temp_data$timestamp = lapply(temp_data$id, function(row) {
    as.numeric(round(as.numeric(row$timestamp_usec)/1000000))
  })
  temp_data$id = NULL
  all_google = rbind(all_google, temp_data)
}
google = data.frame(timestamp = unlist(all_google$timestamp))
google$date = anytime(google$timestamp, tz='US/Central')
google = google[which(google$date > as.Date("2016-07-22")), ]
google$hour = hour(round(google$date, "hour"))
google$count = 1

# moment phone pickups/sessions
moment = read.csv("moment.csv")
moment$time = as.POSIXct(moment$time, format="%H:%M:%S")
moment$hour = hour(round(moment$time, "hour"))
moment$count = 1

# personal emails sent
sentp = read.csv("sentp.csv")
sentp$date = strptime(sentp$date, format = "%a, %d %b %Y %H:%M:%S %z", tz="US/Central")
sentp = data.frame(date = sentp[which(sentp$date > as.Date("2016-07-22")), ])
sentp$hour = hour(round(sentp$date, "hour"))
sentp$count = 1

# TODO redo work emails sent
sent = read.csv("sent.csv")
sent$count = 1

# swarm checkins
swarm = read.csv("swarm.csv")
swarm$offset = replace(swarm$offset, swarm$offset == -300, 'US/Central')
swarm = swarm[which(swarm$offset == 'US/Central'), ]
swarm = swarm[which(swarm$date > as.Date("2016-07-22")), ]
swarm$date = anytime(swarm$timestamp, tz='US/Central')
swarm$hour = hour(round(swarm$date, "hour"))
swarm$count = 1

# vaporizor refill times
times = read.csv("times.csv", header = F)
times$hour = times$V1
times$V1 = NULL
times = times[order(times$hour),]
times$count = 1

# whatsapp messages to bf
whatsapp = read.csv("whatsapp.csv")
whatsapp = whatsapp[which(whatsapp$name == 'Linda'), ]
whatsapp$hour = whatsapp$time
whatsapp$count = 1

###############################################################################
# Helper Functions
###############################################################################
aggregate_and_clean = function(data) {
  
  data = aggregate(data$count, by=list(hour = data$hour), FUN=sum)
  
  for (i in 1:23) {
    if (i %in% unique(data$hour)) { } 
    else { data = rbind(data, c(i,0)) }
  }
  
  data$percent = (data$x / sum(data$x)) * 100
  
  data = data[order(data$hour),]
  data
}

###############################################################################
# Set up data for plotting
###############################################################################

# aggregate
google_agg = aggregate_and_clean(google)
moment_agg = aggregate_and_clean(moment)
sentp_agg = aggregate_and_clean(sentp)
# sentw_agg = aggregate_and_clean(sentw)
swarm_agg = aggregate_and_clean(swarm)
times_agg = aggregate_and_clean(times)
whatsapp_agg = aggregate_and_clean(whatsapp)

# combine
combo = data.frame (
  hour = moment_agg$hour, 
  google = google_agg$percent,
  moment = moment_agg$percent,
  sentp = sentp_agg$percent,
  swarm = swarm_agg$percent,
  times = times_agg$percent,
  whatsapp = whatsapp_agg$percent
)

# melt and order
combo_melt = melt(combo, id="hour")
combo_melt = combo_melt[order(-combo_melt$hour),]
combo_melt$hour = factor(combo_melt$hour, levels = unique(combo_melt$hour))

###############################################################################
# Set up variables for plotting
###############################################################################

activity_labels = c(
  paste("Google Searches\n(n=", sum(google_agg$x), ")", sep = ""),
  paste("Phone Sessions\n(n=", sum(moment_agg$x), ")", sep = ""), 
  paste("Emails Sent\n(n=", sum(sentp_agg$x), ")", sep = ""), 
  paste("Swarm Check-In's\n(n=", sum(swarm_agg$x), ")", sep = ""), 
  paste("Vaporizor Refills\n(n=", sum(times_agg$x), ")", sep = ""), 
  paste("Messages to BF\n(n=", sum(whatsapp_agg$x), ")", sep = "")
)

activity_colors = c(
  "cyan",
  "purple", 
  "blue", 
  "orange", 
  "green", 
  "red")

###############################################################################
# Plot
###############################################################################

#--------------------------------------
# heatmap
#--------------------------------------

ggplot(data = combo_melt, aes(x = hour, y = variable)) +
  geom_tile(aes(fill = variable, alpha = value), 
            width = 1, height = 0.7, color = "white", size = 1) +
  # geom_vline(aes(xintercept=15), linetype="dotted") + 
  # geom_vline(aes(xintercept=13), linetype="dotted") +
  # geom_vline(aes(xintercept=7), linetype="dotted") + 
  # geom_vline(aes(xintercept=5), linetype="dotted") + 
  geom_vline(aes(xintercept=12), linetype="dotted") + 
  geom_text(aes(label = paste(as.character(round(value)), "%")), size = 2) +
  scale_fill_manual("Activities", values = activity_colors) +
  scale_y_discrete("Activities", labels = activity_labels) +
  guides(alpha = "none", fill = "none") +
  labs(
    title = "Frequency of Activities by Hour",
    subtitle = "Data from July 22, 2016 (or later) to Present",
    x = "Hour",
    y = "Activity"
  ) +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    panel.border = element_rect(color = "black", fill = NA)
  ) + coord_flip()

ggsave("hourly_heatmap.png", height = 4.3, width = 7, dpi = 1000)


#--------------------------------------
# dot plot
#--------------------------------------

ggplot(data = combo_melt, aes(x = hour, y = variable)) +
  geom_point(aes(color = variable, size = value), show.legend = F) +
  geom_vline(aes(xintercept=12), linetype="dotted") + 
  geom_text(aes(label = paste(as.character(round(value)), "%")), size = 2, nudge_y=0.2) +
  scale_y_discrete("Activities", labels = activity_labels) +
  guides(alpha = "none", fill = "none") +
  labs(
    title = "Frequency of Activities by Hour",
    subtitle = "Data from July 22, 2016 (or later) to Present",
    x = "Hour",
    y = "Activity"
  ) +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    panel.border = element_rect(color = "black", fill = NA)
  ) + coord_flip()

ggsave("hourly_dotplot.png", height = 4.3, width = 7, dpi = 1000)

#--------------------------------------
# polar (in progress)
#--------------------------------------

combo_melt$hour = as.numeric(combo_melt$hour)
ggplot(data = combo_melt, aes(x = hour, y = value, fill = variable)) +
  geom_bar(stat='identity') +
  scale_fill_manual("Activities", values = activity_colors) +
  labs(
    title = "Frequency of Activities by Hour",
    subtitle = "Data from July 22, 2016 (or later) to Present",
    x = "Hour",
    y = "Activity"
  ) +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    panel.border = element_rect(color = "black", fill = NA)
  ) + coord_polar()

#--------------------------------------
# line (in progress)
#--------------------------------------

combo_melt$hour = as.numeric(combo_melt$hour)
ggplot(data = combo_melt, aes(x = hour, y = value, color = variable)) +
  geom_line(stat='identity') +
  scale_fill_manual("Activities", values = activity_colors) +
  scale_x_continuous(breaks = c(0:23), labels = c(0:23)) + 
  labs(
    title = "Frequency of Activities by Hour",
    subtitle = "Data from July 22, 2016 (or later) to Present",
    x = "Hour",
    y = "Activity"
  ) +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    panel.border = element_rect(color = "black", fill = NA)
  )










