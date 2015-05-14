happiness <- function(sentiment.file = "D:/Study/data-engineer-problem/AFINN-111.txt" , tweet.path="D:/Study/data-engineer-problem/twitter-stream.txt" ) {
  library(jsonlite)
  library(dplyr)

  df            <- NULL
  date          <- NULL
  text          <- NULL
  language      <- NULL
  latitude      <- NULL
  longitude     <- NULL
  happiness.lvl <- NULL
  usa.state     <- NULL

  state <- function(states, long, lat) {
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
    json_data <- fromJSON(data[i], simplifyVector = FALSE)

    if (!is.null(json_data$coordinates) & is.null(json_data$delete)) {
      lat <- json_data$coordinates[2]$coordinates[2]
      long <- json_data$coordinates[2]$coordinates[1]

      if (min(states[ ,5]) < lat & lat < max(states[ ,4]) & max(states[ ,3]) > long & long > min(states[ ,2])) {
        text[j] <- json_data$text
        language[j] <- json_data$lang
        latitude[j] <- json_data$coordinates[2]$coordinates[2]
        longitude[j] <- json_data$coordinates[2]$coordinates[1]
        usa.state[j] <- state(long = longitude[[j]], lat = latitude[[j]], states = states)
        happiness.lvl[j] <- happiness(words = words, text = text[j])
        j <- j+1
      }
    }
  }

  df <- data.frame(cbind(text, happiness.lvl, usa.state, language), stringsAsFactors = FALSE)
  df <- df[df$usa.state != "Not USA", ]
  grp <- group_by(df, usa.state)
  hap.sum <- summarize(grp, happiness = sum(as.numeric(happiness.lvl)))
  hap.states <- arrange(hap.sum, desc(happiness))[1,]

  return (hap.states[1])
}
