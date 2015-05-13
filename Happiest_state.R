happiness <- function(sentiment.file = "D:/Study/data-engineer-problem/AFINN-111.txt" ,tweet.path="D:/Study/data-engineer-problem/twitter-stream.txt" ) {
library(jsonlite)
library(rgdal)
library(dplyr)

df <- NULL
date <- NULL
text <- NULL
language <- NULL
latitude <- NULL
longitude <- NULL
happiness <- NULL
state <- NULL

data <- readLines(tweet.path)
states <- read.table("D:/Program Files/R/Intern/states.txt", header = TRUE, stringsAsFactors = FALSE)
words <- readChar(sentiment.file, nchars = 100000000)

j <- 1
for (i in 1:length(data)) {
  x <- unlist(strsplit(data[i], "{", fixed = TRUE))
  ifelse (x[2] ==  "\"delete\":", next, json <- data[i])
  json_data <- fromJSON(json, simplifyVector=FALSE)
  if (!is.null(json_data$coordinates))  {  
    date[j] <- json_data$created_at                     
    text[j] <- json_data$text                   
    language[j] <- json_data$lang
    latitude[j] <- json_data$coordinates[2]$coordinates[2]
    longitude[j] <- json_data$coordinates[2]$coordinates[1]
    j <- j+1
  } 
}
df <- data.frame(cbind(date, text, latitude, longitude, language), stringsAsFactors = FALSE)
df <- df[df$language == "en", ]
var <- unlist(strsplit(words, "\t", fixed = TRUE))
var <- unlist(strsplit(var, "\n", fixed = TRUE))
words <- data.frame(2, 1:(length(var)/2), stringsAsFactors = FALSE)
x <- 0
for (i in 1:(length(var)/2)){
  words[i,1] <- var[x+i]
  words[i,2] <- var[x+i+1]
  x <- x+1
}
words[,2] <- as.numeric(words[,2])

for (i in 1:nrow(df)){
  x <- 0
  for(j in 1:nrow(words)){
    if(grepl(words[j, 1] , df[i, 2]))
      x[j] <- words[j, 2] 
    else next
  }
  happiness[i] <- sum(x, na.rm = TRUE)
}
df$happiness < -happiness

for (i in 1:nrow(df)){
  for(j in 1:nrow(states)){
    ifelse (states[j, 5] < df$latitude[[i]] & df$latitude[[i]] < states[j, 4] & 
      states[j, 3] > df$longitude[[i]] & df$longitude[[i]] > states[j, 2],
            state[i]<-states[j, 1], next)
  } 
}

df$state <- state
df <- na.omit(df)

grp <- group_by(df, state)
hap.sum <- summarize(grp, happiness = sum(happiness))
hap.states <- arrange(hap.sum, desc(happiness))[1,]
return (hap.states[1])
}
