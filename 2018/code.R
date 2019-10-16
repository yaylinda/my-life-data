library(ggplot2)
library(plyr)
library(scales)
library(zoo)
library(lubridate)
library(reshape2)

setwd("~/Developer/my-life-data/2018")

{
  data = read.csv('data.csv')
  data$date = as.Date(data$date, format = "%m/%d/%y")
  data$week = week(data$date)
  data$month = format(data$date,"%B")
  data$month = factor(data$month, list("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"))
  data$yearmonth = as.yearmon(data$date)
  data$yearmonthf = factor(data$yearmonth)
  data$day_of_week = factor(data$day_of_week, rev(list("Saturday", "Friday", "Thursday", "Wednesday", "Tuesday", "Monday", "Sunday")))
  
  data[data==0] = "No"
  data[data==1] = "Yes"
  
  # calculate week of month, where sunday is new week
  weekofmonth = rep(0, length(data$date))
  weekNum = 1
  currentMonth = 1
  for (i in (1:length(data$date))) {
    date = data$date[i]
    if (weekdays(date) == "Sunday") {
      weekNum = weekNum + 1
    }
    if (month(date) > currentMonth) {
      currentMonth = currentMonth + 1
      weekNum = 1
    }
    weekofmonth[i] = weekNum
  }
  data$monthweek = weekofmonth
  data$monthweek = factor(data$monthweek, list(6, 5, 4, 3, 2, 1))
}

abandoned = c()
for (i in 1:length(data$abandoned_weekday)) {
  if (!is.na(data$abandoned_weekday[i])) {
    abandoned = c(abandoned, data$abandoned_weekday[i])
  } else {
    abandoned = c(abandoned, data$abandoned_weekend[i])
  }
}
data$abandoned = abandoned

percentages = list()
for (i in 3:length(data)) {
  var_name = colnames(data)[i]
  percentage = (sum(data[i] == 'Yes') / dim(data)[1]) * 100
  percentages = c(percentages, c(var_name, percentage))
}
#######################################
# Helper function to melt and plot data
#######################################

variable_labels = c(
  `weed` = "Weed",
  `ambien` = "Ambien",
  `alcohol` = "Alcohol",
  `ailment` = "Ailment",
  `poop` = "Poop",
  `journaled` = "Journaled",
  `sex` = "Sex",
  `cried` = "Cried",
  `happy` = "Happy\nOverall",
  `sad` = "Sad\nOverall",
  `work_happy` = "Happy about\nWork",
  `work_sad` = "Sad about\nWork",
  `happy_relationship` = "Happy about\nRelationship",
  `sad_relationship` = "Sad about\nRelationship",
  `social` = "Social\n(My Friends)",
  `social_sean` = "Social\n(Sean's Friends)",
  `anxiety_stress` = "Anxious\nand/or\nStressed",
  `angry_frustrated` = "Angry\nand/or\nFrustrated",
  `excited` = "Excited",
  `sean_alcohol` = "Sean\nAlcohol",
  `sean_drunk` = "Sean\nDrunk",
  `abandoned` = "Abandoned\nby Sean",
  
  `January` = "January", 
  `February` = "February", 
  `March` = "March", 
  `April` = "April", 
  `May` = "May", 
  `June` = "June", 
  `July` = "July", 
  `August` = "August", 
  `September` = "September", 
  `October` = "October", 
  `November` = "November", 
  `December` = "December"
)

melt_and_plot = function(data, subtitle) {
  melt = melt(
    data = data, 
    id = names(data)[1:4])
  
  ggplot(melt, aes(day_of_week, monthweek, fill = as.factor(value))) + 
    geom_tile(color = "white") + 
    facet_grid(variable ~ month, switch = "y", space = "free", labeller = as_labeller(variable_labels)) +
    # coord_equal(ratio = 1) + 
    labs(
      y = "",
      x = "",
      title = "Daily Stats 2018",
      fill = "Legend") + 
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.text.x = element_blank(), axis.text.y = element_blank(), axis.ticks = element_blank()) +
    theme(strip.text.y = element_text(size = 12, angle = 180)) +
    theme(strip.text.x = element_text(size = 12)) +
    theme(plot.title = element_text(size = 20))
}

#######################################
# Tables of Various Statistics
#######################################

#------------------
# ALL
#------------------
data.all = data.frame(
  "date" = data$date, 
  "month" = data$month, 
  "day_of_week" = data$day_of_week, 
  "monthweek" = data$monthweek, 
  "weed" = data$weed,
  "ambien" = data$ambien,
  "alcohol" = data$alcohol,
  "cried" = data$cried,
  "ailment" = data$ailment,
  "poop" = data$poop,
  "work_happy" = data$work_happy,
  "work_sad" = data$work_sad,
  "social" = data$social,
  "social_sean" = data$social_sean,
  "happy" = data$happy,
  "sad" = data$sad,
  "anxiety_stress" = data$anxiety_stress,
  "angry_frustrated" = data$angry_frustrated,
  "excited" = data$excited,
  "happy_relationship" = data$happy_relationship,
  "sad_relationship" = data$sad_relationship,
  "sex" = data$sex,
  "sean_alcohol" = data$sean_alcohol,
  "sean_drunk" = data$sean_drunk,
  "abandoned" = data$abandoned,
  "journaled" = data$journaled
)
melt_and_plot(data.all, "All")

#------------------
# DRUGS
#------------------
data.drugs = data.frame(
  "date" = data$date, 
  "month" = data$month, 
  "day_of_week" = data$day_of_week, 
  "monthweek" = data$monthweek, 
  "weed" = data$weed, 
  "ambien" = data$ambien, 
  "alcohol" = data$alcohol
)
melt_and_plot(data.drugs, "Drugs")

#------------------
# FEELINGS
#------------------
data.feelings = data.frame(
  "date" = data$date, 
  "month" = data$month, 
  "day_of_week" = data$day_of_week, 
  "monthweek" = data$monthweek, 
  "cried" = data$cried,
  "ailment" = data$ailment,
  "work_happy" = data$work_happy,
  "work_sad" = data$work_sad,
  "happy" = data$happy,
  "sad" = data$sad,
  "anxiety_stress" = data$anxiety_stress,
  "angry_frustrated" = data$angry_frustrated,
  "excited" = data$excited,
  "happy_relationship" = data$happy_relationship,
  "sad_relationship" = data$sad_relationship
)
melt_and_plot(data.feelings, "Feelings")

#------------------
# RELATIONSHIP
#------------------
data.relationship = data.frame(
  "date" = data$date, 
  "month" = data$month, 
  "day_of_week" = data$day_of_week, 
  "monthweek" = data$monthweek, 
  "happy_relationship" = data$happy_relationship,
  "sad_relationship" = data$sad_relationship,
  "sex" = data$sex,
  "sean_alcohol" = data$sean_alcohol,
  "sean_drunk" = data$sean_drunk,
  "abandoned_weekday" = data$abandoned_weekday,
  "abandoned_weekend" = data$abandoned_weekend
)
melt_and_plot(data.relationship, "Relationship")

#------------------
# WORK
#------------------
data.work = data.frame(
  "date" = data$date, 
  "month" = data$month, 
  "day_of_week" = data$day_of_week, 
  "monthweek" = data$monthweek, 
  "work_happy" = data$work_happy,
  "work_sad" = data$work_sad
)
melt_and_plot(data.work, "Work")

#------------------
# INTERESTING
#------------------
data.interesting = data.frame(
  "date" = data$date, 
  "month" = data$month, 
  "day_of_week" = data$day_of_week,  
  "monthweek" = data$monthweek,
  "weed" = data$weed,
  "ambien" = data$ambien,
  "alcohol" = data$alcohol,
  "ailment" = data$ailment,
  "poop" = data$poop,
  "journaled" = data$journaled,
  "sex" = data$sex,
  "cried" = data$cried,
  "happy" = data$happy,
  "sad" = data$sad,
  "work_happy" = data$work_happy,
  "work_sad" = data$work_sad,
  "happy_relationship" = data$happy_relationship,
  "sad_relationship" = data$sad_relationship
)
melt_and_plot(data.interesting, "Various Statistics")

ggplot(data, aes(x = data$day_of_week, fill = abandoned_weekday)) + 
  geom_bar(width = 0.25) + 
  scale_fill_manual(values = c('green', 'red'))


