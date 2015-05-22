# Grammarly Internship task 
Task is to compute the sentiment of each tweet based on the sentiment scores of the terms in the tweet. The sentiment of a tweet is equivalent to the sum of the sentiment scores for each term in the tweet.

The file AFINN-111.txt contains a list of pre-computed sentiment scores. Each line in the file contains a word or phrase followed by a sentiment score. See the file AFINN-README.txt for more information. 

Script  takes a file of tweets and returns the name of the happiest state as a string.

Script written in R language, using "jsonlite", "dplyr" packages.
Visualisation script uses "ggplot2", "rgdal" and "gmap" packages.

Version updates:

Happiest_state - taking data as character. Uses strsplit and ifelse for searching matches. Work time - 35 min.

Happiest_state v2.0 - parsing data as JSON string. Work time - 12 min.

Happiest_state v3.0 - parsing data as JSON string and looking for matches in coordinates, also script was optimised by decreasing loops to 1, else was written as functions. Work time - 2 min.

https://cloud.githubusercontent.com/assets/12424814/7780678/d83b6bae-00ea-11e5-9060-c496eb87ec9c.png
