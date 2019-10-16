library(lubridate)
setwd("~/Desktop/MyProject/Amazon")

#######################################
# Get and prepare data
#######################################

# items
items = read.csv("items.csv")
linda_items = items[which("Linda" == items$Shipping.Address.Name),]
lindaz_items = items[which("Linda Zheng" == items$Shipping.Address.Name),]
hanzhang_items = items[which("Hanzhang Zheng" == items$Shipping.Address.Name),]
items_temp = rbind(linda_items, lindaz_items, hanzhang_items)
items = data.frame(date=items_temp$Order.Date, category=items_temp$Category, subtotal=items_temp$Item.Subtotal, tax=items_temp$Item.Subtotal.Tax, total=items_temp$Item.Total)

items$date = as.Date(as.character(items$date), format="%m/%d/%y")
items$category = as.character(items$category)
items$category[which(items$category == "")] = "Other"
items$category = as.factor(items$category)

items$subtotal = as.numeric(substring(as.character(items$subtotal), 2))
items$tax = as.numeric(substring(as.character(items$tax), 2))
items$total = as.numeric(substring(as.character(items$total), 2))

items$yearmon = paste(year(items$date), sprintf("%02d", month(items$date)), sep="-")
items$weekday = weekdays(items$date)

# orders
orders = read.csv("orders.csv")
linda_orders = orders[which("Linda" == orders$Shipping.Address.Name),]
lindaz_orders = orders[which("Linda Zheng" == orders$Shipping.Address.Name),]
hanzhang_orders = orders[which("Hanzhang Zheng" == orders$Shipping.Address.Name),]
orders = rbind(linda_orders, lindaz_orders, hanzhang_orders)

#######################################
# box plots
#######################################
boxplot(total~category, data=items)

boxplot(total~yearmon, data=items)

boxplot(total~weekday, data=items)

#######################################
# spending per month over time (sum)
#######################################
subtotal_per_month = aggregate(items$subtotal, by=list(yearmon = items$yearmon), FUN=sum)
tax_per_month = aggregate(items$tax, by=list(yearmon = items$yearmon), FUN=sum)
combined_total_per_month = data.frame(yearmon=subtotal_per_month$yearmon,
                                      subtotal=subtotal_per_month$x,
                                      tax=tax_per_month$x)

colors = c('green', 'cyan')
labels = c('item price', 'tax')
barplot(t(combined_total_per_month[,2:3]), names.arg=combined_total_per_month$yearmon, xlab="Month", ylab="Amount Spent ($)", 
        main="Amazon Spendings", ylim=c(0,600), col=colors)
legend("topleft", labels, cex = 1, fill = colors, bty = "n")

#######################################
# spending per month over time 
# grouped by categories
#######################################


#######################################
# spending per category
#######################################

#######################################
# spendings per order
#######################################

#######################################
# orders per month over time
#######################################