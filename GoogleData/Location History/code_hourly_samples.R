library(ggplot2)
library(jsonlite)

setwd("~/Developer/my-life-data/GoogleData/Location History")
Sys.setlocale("LC_TIME", "C")
Sys.setenv(TZ="America/Chicago")

google_location_data = fromJSON("Location History.json", flatten = TRUE)

google_locations = google_location_data$locations

c = as.POSIXct(as.numeric(google_locations$timestampMs)/1000, tz="America/Chicago", origin="1970-01-01")

google_locations$weekdays = factor(format(google_locations$timekeeping, "%a"), 
                                    levels = c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"))

total_days = max(google_locations$timekeeping) - min(google_locations$timekeeping)

ggplot(google_locations) +
  geom_line(aes(x = format(timekeeping, "%H"), y = (..count..)/(1779.205/7), 
                group = weekdays, color = weekdays), 
            stat = "count", size = 1) + 
  geom_line(aes(x = format(timekeeping, "%H"), y = (..count..)/(1779.205), 
                group = 1),
            stat = "count", linetype = 2, size = 0.7) + 
  scale_color_brewer(palette = "Set2") +
  ylim(c(23, 35)) +
  theme_bw() + 
  labs(color = "Weekdays",
       title = "Hourly Measurments of Google Location Data",
       subtitle = "From Location History of Google Takeout Data. Total number of days: 1780.") +
  xlab("Hour of the Day (Midnight - 11 PM)") +
  ylab("Average Number of Measurements")

#-----------------------------------------------------------------------------
# subset data from after 7/25/16

google_locations_c1 = subset(google_locations, timekeeping > "2016-07-25")

google_locations_c1$is_weekend = ifelse(google_locations_c1$weekdays == "Sat", "Weekend", ifelse(google_locations_c1$weekdays == "Sun", "Weekend", "Weekday"))

total_days_c1 = max(google_locations_c1$timekeeping) - min(google_locations_c1$timekeeping)

ggplot(google_locations_c1) +
  geom_line(aes(x = format(timekeeping, "%H"), y = (..count..)/(683.9023/7), 
                group = weekdays, color = weekdays), 
            stat = "count", size = 1) + 
  geom_line(aes(x = format(timekeeping, "%H"), y = (..count..)/(683.9023), 
                group = 1),
            stat = "count", linetype = 2, size = 0.7) + 
  scale_color_brewer(palette = "Set2") +
  theme_bw() + 
  labs(color = "Day of Week",
       title = "Hourly Measurments of Google Location Data",
       subtitle = "July 25, 2016 - June 8, 2018 (684 Total Days)") +
  xlab("Hour of the Day (Midnight - 11 PM)") +
  ylab("Average Number of Measurements")

