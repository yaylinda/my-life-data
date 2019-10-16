library('lattice')
library('ggplot2')
library('reshape2')
library('zoo')
library('lubridate')
setwd("~/Developer/my-life-data/MobileGameSpendings")

#######################################
# stacked bar chart: money spent per game per month
#######################################
quarterly_aggregated_game = aggregate(data$price,by=list(date=data$yearQ, game=data$game),FUN=sum)
quarterly_aggregated_game = quarterly_aggregated_game[order(quarterly_aggregated_game$date),]
matrix_quarterly_aggregated_game = reshape(quarterly_aggregated_game, timevar='date', idvar='game', direction='wide')
matrix_quarterly_aggregated_game[is.na(matrix_quarterly_aggregated_game)]=0
colnames(matrix_quarterly_aggregated_game) = gsub("x.", "", colnames(matrix_quarterly_aggregated_game))
rownames(matrix_quarterly_aggregated_game) = matrix_quarterly_aggregated_game$game
matrix_quarterly_aggregated_game$game = NULL

barplot(as.matrix(matrix_quarterly_aggregated_game), 
        main="Mobile Game Spending", xlab="Quarter", ylab="Amount Spent ($)", ylim=c(0,450), 
        col=c('gray', 'orange', 'purple', 'blue', 'green', 'yellow', 'red'), 
        legend=rownames(matrix_quarterly_aggregated_game), args.legend=list(x=3,y=400,bty="n"))

#######################################
# ggplot stacked bar chart: money spent per game per month
#######################################
ggplot(quarterly_aggregated_game, aes(x=date, y=x, fill=game)) + 
  geom_bar(stat='identity') + xlab("Quarter") + ylab("Amount Spent ($)") + ggtitle("Quarterly Mobile Game Spending")


#######################################
# separate by game monthly
#######################################
data = read.csv("purchases.csv")
data$game = as.character(data$game)
data$date = as.Date(data$date, "%m/%d/%y")
data$yearmon = format(data$date, "%Y-%m")
data = data[order(data$date),]

aggregated_game = aggregate(data$price,by=list(data$game),FUN=sum)
colnames(aggregated_game) = c("game", "price")
top_spending_games = aggregated_game[which(aggregated_game$price>50),1]
data$game[!(data$game %in% top_spending_games)] = "Other"

all_months = data.frame(date = seq(as.Date("2014-01-01"), as.Date("2017-10-01"), by = "1 month"))
all_months$yearmon = format(all_months$date, "%Y-%m")

clashClansMonthly = data[data$game == 'Clash of Clans', c(1,3,4,5)]
clashRoyaleMonthly = data[data$game == 'Clash Royale', c(1,3,4,5)]
gordonRamsayMonthly = data[data$game == 'Gordon Ramsay Dash', c(1,3,4,5)]
happyStreetMonthly = data[data$game == 'Happy Street', c(1,3,4,5)]
hearthstoneMonthly = data[data$game == 'Hearthstone', c(1,3,4,5)]
pokemonMonthly = data[data$game == 'Pokemon Go', c(1,3,4,5)]
othersMonthly = data[data$game == 'Other', c(1,3,4,5)]

monthlyAllGames = data.frame(
  yearmon = character(), 
  clashClans = double(),
  clashRoyale = double(),
  gordonRamsay = double(),
  happyStreet = double(),
  hearthstone = double(),
  pokemon = double(),
  others = double()
)

for (i in 1:dim(all_months)[1]) {
  yearmon = all_months$yearmon[i]
  
  clashClansPrice = 0
  if (yearmon %in% clashClansMonthly$yearmon) {
    clashClansPrice = clashClansMonthly[which(clashClansMonthly$yearmon == yearmon), 3]
  }
  
  clashRoyalePrice = 0
  if (yearmon %in% clashRoyaleMonthly$yearmon) {
    clashRoyalePrice = clashRoyaleMonthly[which(clashRoyaleMonthly$yearmon == yearmon), 3]
  }
  
  gordonRamsayPrice = 0
  if (yearmon %in% gordonRamsayMonthly$yearmon) {
    gordonRamsayPrice = gordonRamsayMonthly[which(gordonRamsayMonthly$yearmon == yearmon), 3]
  }
  
  happyStreetPrice = 0
  if (yearmon %in% happyStreetMonthly$yearmon) {
    happyStreetPrice = happyStreetMonthly[which(happyStreetMonthly$yearmon == yearmon), 3]
  }
  
  hearthstonePrice = 0
  if (yearmon %in% hearthstoneMonthly$yearmon) {
    hearthstonePrice = hearthstoneMonthly[which(hearthstoneMonthly$yearmon == yearmon), 3]
  }
  
  pokemonPrice = 0
  if (yearmon %in% pokemonMonthly$yearmon) {
    pokemonPrice = pokemonMonthly[which(pokemonMonthly$yearmon == yearmon), 3]
  }
  
  othersPrice = 0
  if (yearmon %in% othersMonthly$yearmon) {
    othersPrice = othersMonthly[which(othersMonthly$yearmon == yearmon), 3]
    
  }
  
  clashClansPrice = sum(clashClansPrice)
  clashRoyalePrice = sum(clashRoyalePrice)
  gordonRamsayPrice = sum(gordonRamsayPrice)
  happyStreetPrice = sum(happyStreetPrice)
  hearthstonePrice = sum(hearthstonePrice)
  pokemonPrice = sum(pokemonPrice)
  othersPrice = sum(othersPrice)
  
  newRow = data.frame(yearmon, clashClansPrice, clashRoyalePrice, gordonRamsayPrice, happyStreetPrice, hearthstonePrice, pokemonPrice, othersPrice)
  names(newRow) = c("yearmon", "clashClans", "clashRoyale", "gordonRamsay", "happyStreet", "hearthstone", "pokemon", "others")
  monthlyAllGames = rbind(monthlyAllGames, newRow)
}

monthlyAllGames$clashClansCum = cumsum(monthlyAllGames$clashClans)
monthlyAllGames$clashRoyaleCum = cumsum(monthlyAllGames$clashRoyale)
monthlyAllGames$gordonRamsayCum = cumsum(monthlyAllGames$gordonRamsay)
monthlyAllGames$happyStreetCum = cumsum(monthlyAllGames$happyStreet)
monthlyAllGames$hearthstoneCum = cumsum(monthlyAllGames$hearthstone)
monthlyAllGames$pokemonCum = cumsum(monthlyAllGames$pokemon)
monthlyAllGames$othersCum = cumsum(monthlyAllGames$others)

monthlyGamesCum = monthlyAllGames[,c(1,9:15)]
monthlyGamesCumMelt = melt(monthlyGamesCum, id.vars="yearmon")

ggplot(data=monthlyGamesCumMelt, aes(x=yearmon, y=value)) + 
  geom_area(aes(color=variable, fill=variable)) + 
  xlab("Month") +
  ylab("Amount Spent ($)") + 
  ggtitle("Monthly Cumulative Mobile Game Spending")

ggplot(data=monthlyGamesCumMelt, aes(x=yearmon, y=value)) + 
  geom_area(aes(color=variable, fill=variable), position="identity") + 
  xlab("Month") +
  ylab("Amount Spent ($)") + 
  ggtitle("Monthly Mobile Game Spending")








