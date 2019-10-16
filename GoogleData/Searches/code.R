library(jsonlite)
library(wordcloud)
library(tm)
library(SnowballC)
library(tidyverse)
library(plyr)

json_files = list.files(pattern="*.json", full.names=T, recursive=TRUE)
original_data_obj = NULL

for (i in 1:length(json_files)) {
  temp_data = fromJSON(json_files[i])
  temp_data_obj = temp_data$event$query
  
  temp_data_obj$timestamp = lapply(temp_data_obj$id, function(row) {
    as.numeric(round(as.numeric(row$timestamp_usec)/1000000))
  })
  
  temp_data_obj$id = NULL
  
  original_data_obj = rbind(original_data_obj, temp_data_obj)
}

original_data_obj$time_counts = lapply(original_data_obj$timestamp, function(row) {
  length((row))
})

data_obj = original_data_obj
data_obj = original_data_obj[-which(original_data_obj$time_counts > 1),]
data_obj_dups = original_data_obj[which(original_data_obj$time_counts > 1),]
  
toAdd = data.frame()
for(i in 1:dim(data_obj_dups)[1]) {
  row = data_obj_dups[i,]
  times = row$timestamp[[1]]
  
  for (j in 1:length(times)) {
    toAdd = rbind(toAdd, data.frame("query_text"=row$query_text, "timestamp"=times[j], "time_counts"=1))
  }
}

data_obj = rbind(data_obj, toAdd)
finalized_data_obj$timestamp = unlist(data_obj$timestamp)
finalized_data_obj$time_counts = unlist(data_obj$time_counts)
finalized_data_obj$date = as.Date(as.POSIXct(data_obj$timestamp, origin="1970-01-01"))
finalized_data_obj$yearmon = paste(substring(data_obj$date,1,4), substring(data_obj$date,6,7), sep="-")
finalized_data_obj$dow = weekdays(data_obj$date)

#######################################
# sum num searches per mon-year
#######################################
sum_num_searches_by_yearmon = aggregate(finalized_data_obj[c("time_counts")], by=list(yearmon=finalized_data_obj$yearmon), FUN=sum)
sum_num_searches_by_yearmon = sum_num_searches_by_yearmon[-1,]

ggplot(data=sum_num_searches_by_yearmon, aes(x=sum_num_searches_by_yearmon$yearmon, y=sum_num_searches_by_yearmon$time_counts)) +geom_bar(stat="identity")

#######################################
# avg num searches per dow
#######################################
sum_num_searches_by_dow = aggregate(finalized_data_obj[c("time_counts")], by=list(dow=finalized_data_obj$dow), FUN=sum)
sum_num_searches_by_dow$dow <- factor(sum_num_searches_by_dow$dow, levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))
ggplot(data=sum_num_searches_by_dow, aes(x=sum_num_searches_by_dow$dow, y=sum_num_searches_by_dow$time_counts)) +geom_bar(stat="identity")

#######################################
# word cloud of search terms
#######################################

data_obj_wordcloud = data_obj[which(regexpr('(Current Location)', data_obj$query_text) == -1),]
data_obj_wordcloud = data_obj_wordcloud[which(regexpr(' -> ', data_obj_wordcloud$query_text) == -1),]

queryCorpusUnique = Corpus(VectorSource(unique(data_obj_wordcloud$query_text)))
queryCorpusUnique = tm_map(queryCorpusUnique, PlainTextDocument)
queryCorpusUnique = tm_map(queryCorpusUnique, removePunctuation)
queryCorpusUnique = tm_map(queryCorpusUnique, removeWords, stopwords('english'))
wordcloud(queryCorpusUnique, max.words = 100, random.order = FALSE)

#######################################
# most popular word per mon-year
#######################################

#######################################
# average dow searches grouped by month
#######################################