# This is function that computes USA states sentiment based on Twitter data and AFINN word list. 
# Function was developed for Grammarly internship by Teplyakov K.
happiness <- function(sentiment.file = "D:/Study/data-engineer-problem/AFINN-111.txt" , tweet.path="D:/Study/data-engineer-problem/twitter-stream.txt" ) {
# Args:
# sentiment.file: file containing sentiment words with numerical valence
# tweet.path: file containing Twitter data
#
# Returns:
# two-word abbreviated state name with highest sentiment level

  library(jsonlite) 
  library(dplyr)

  df            <- NULL
  date          <- NULL
  text          <- NULL
  language      <- NULL
  happiness.lvl <- NULL
  usa.state     <- NULL

  state <- function(states, long, lat) { 
  # function that determines if tweet was written in USA or not
  # compares lantitude & longitude coordinates in tweet with coordinates of each state
  # Args: 
  # states - file with coordinates of each USA state
  # long - longitude coordinate of tweet
  # lat - latitude coordinate of tweet
  # Returns:
  # state - character, name of state or "Not USA" value
    for (l in 1:nrow(states)) { 
      if (states[l, 5] < lat & lat < states[l, 4] & states[l, 3] > long & long > states[l, 2]) { 
        state <- states[l,1]
        break
      } else {
        state <- "Not USA"
      }
    }

    return (state)
  }

  happiness <- function (text, words){
  # function that computes sentiments of tweet
  # Args:
  # text - text of tweet
  # words - file contains sentiment words with valents
  # Returns:
  # happiness - numeric, contains sum of sentiment in tweet
    x <- 0
    for (y in 1:nrow(words)) {
      if (grepl(words[y, 1], text)) {
        x[y] <- words[y,2]
      } else {
        next
      }
    }

    happiness <- sum(x, na.rm = TRUE)
    return (happiness)
  }

  data <- readLines(tweet.path) 
  words <- readChar(sentiment.file, nchars = 100000000)
  states <- read.table("D:/Program Files/R/Intern/states.txt", header = TRUE, stringsAsFactors = FALSE)

  var <- unlist(strsplit(words, "\t", fixed = TRUE))
  var <- unlist(strsplit(var, "\n", fixed = TRUE))
  words <- data.frame(2,1:(length(var)/2), stringsAsFactors = FALSE)
  x <- 0

  for (i in 1:(length(var) / 2)) {
    words[i, 1] <- var[x+i]
    words[i, 2] <- var[x+i+1]
    x <- x+1
  }
  words[ ,2] <- as.numeric(words[,2])

  j <- 1
  for (i in 1:length(data)) {
    json_data <- fromJSON(data[i], simplifyVector = FALSE) # takes line from data and convert it from JSON to list

    if (!is.null(json_data$coordinates) & is.null(json_data$delete)) { # if data not delete and has not null coordinates, than proceed
      lat <- json_data$coordinates[2]$coordinates[2]  # write latitude & longitude coordinates of tweet
      long <- json_data$coordinates[2]$coordinates[1] 

      if (min(states[ ,5]) < lat & lat < max(states[ ,4]) & max(states[ ,3]) > long & long > min(states[ ,2])) { # compare coordinates with USA boundary coordinates
        text[j] <- json_data$text # tweet text
        language[j] <- json_data$lang # tweet language
        usa.state[j] <- state(long = long, lat = lat, states = states) # compute state belongness
        happiness.lvl[j] <- happiness(words = words, text = text[j]) # compute sentiment
        j <- j+1 #counter for iterations
      }
    }
  }

  df <- data.frame(cbind(text, happiness.lvl, usa.state, language), stringsAsFactors = FALSE) # create dataframe
  df <- df[df$usa.state != "Not USA", ] # filter dataframe with only USA tweets
  grp <- group_by(df, usa.state) # group variables for states
  hap.sum <- summarize(grp, happiness = sum(as.numeric(happiness.lvl))) # counting sum of sentiment in each state
  hap.states <- arrange(hap.sum, desc(happiness))[1,] # arranging sentiment by desc and saving states names

  return (hap.states[1]) # return first value of hap.states vector
}
