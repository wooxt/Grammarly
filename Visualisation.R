# Script for visualisation of data in happiest state
# Using rgdal, ggplot2, ggmap libraries
library(rgdal)
library(ggplot2)
library(ggmap)

counties <- readOGR("D:/Program Files/R/Intern", layer = "states" ) # reading .shp file of USA state 
plotdata <- fortify(counties)
plotdata$happiness <- 0
for (i in 1:nrow(counties@data)){ # writing sum of sentiments in state for every state id in counties 
  for (j in 1:nrow(hap.sum)){
  ifelse(as.character(counties@data[i, 5]) == hap.sum[j, 1],
                       plotdata$happiness[plotdata$id == i-1] <- as.numeric(paste(hap.sum[j, 2])), next)
}
}
  
b <- bbox(counties) # retrieving spatial bounding box from spatial data
b[1, ] <- (b[1, ] - mean(b[1, ])) * 1.05 + mean(b[1, ]) #
b[2, ] <- (b[2, ] - mean(b[2, ])) * 1.05 + mean(b[2, ])

osm <- get_map(location = b, source = "google", maptype = "roadmap", zoom = 3) # getting google map
lnd.b <- ggmap(osm)

p <- lnd.b+geom_polygon(data = plotdata, aes(x = long, y = lat, group = group, # plotting data
                                             fill = plotdata$happiness), color = "#B6B6B6", size = 0.25, alpha = 0.5)+
coord_map()+
  scale_fill_distiller("Sentiment level",
                       palette = "RdYlGn")+
  guides(fill = guide_legend(reverse = TRUE))+
  theme_nothing(legend = TRUE) +
  ggtitle ("Sentiment distribution")+
  theme(plot.title = element_text(size = 12, face="bold"),
        legend.title = element_text(colour="black", size = 9, face = "bold"))   
ggsave(p, file = "map1.png", width = 6, height = 4.5, type = "cairo-png") # save plot 
