library(dplyr)

 ## Load the trashtracking data (uses a dummy function to locate the source directory) !!!Must use Source instead of Run!!!
trashData <- read.csv(paste(getSrcDirectory(function(x) {x}), "Data/output.csv", sep = "/"))

plot( trashData$latitude ~ trashData$longitude, ylab="latitude"
      , ylim=c(50, 54), xlab="longitude", xlim=c(2.5, 8)
      , main="trash data", col="red" )

points(trashData$longitude[trashData
                           $brand==" Heineken"]
       ,trashData$latitude[trashData$brand==" Heineken"]
       , col="blue")
getSrcDirectory(function(dummy) {dummy})