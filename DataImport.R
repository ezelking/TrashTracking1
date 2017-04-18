library(dplyr)
library(rvest)
library(RCurl)
library(rjson)

 ## Load the trashtracking data (uses a dummy function to locate the source directory) !!!Must use Source instead of Run!!!
trashData <- read.csv(paste(getSrcDirectory(function(x) {x}), "Data/output.csv", sep = "/"))

schools <- read.csv(paste(getSrcDirectory(function(x) {x}), "Data/03.-alle-vestigingen-basisonderwijs.csv", sep = "/"), sep = ";")

 ## Create Matrix for the coordinates
schoolCoordinates <- matrix(nrow = nrow(schools), ncol = 2)
colnames(schoolCoordinates) <- c("Longtitude", "Latitude")

 ## Get coordinates of the schools from the addresses
for (i in 1:nrow(schools)){
 try(
   schoolCoordinates[i,] <- c(fromJSON(getURL(paste0("http://maps.google.com/maps/api/geocode/json?address=", schools[i,"HUISNUMMER.TOEVOEGING"],"+", gsub(" ", "", schools[i,"STRAATNAAM"]), ",+NL,+", gsub(" ", "", schools[i,"POSTCODE"]))))$results[[1]]$geometry$location$lng,
                              fromJSON(getURL(paste0("http://maps.google.com/maps/api/geocode/json?address=", schools[i,"HUISNUMMER.TOEVOEGING"],"+", gsub(" ", "", schools[i,"STRAATNAAM"]), ",+NL,+", gsub(" ", "", schools[i,"POSTCODE"]))))$results[[1]]$geometry$location$lat
   ))
 print(i)
}

# plot( trashData$latitude ~ trashData$longitude, ylab="latitude"
#       , ylim=c(50, 54), xlab="longitude", xlim=c(2.5, 8)
#       , main="trash data", col="red" )

# points(trashData$longitude[trashData
#                            $brand==" Heineken"]
#        ,trashData$latitude[trashData$brand==" Heineken"]
#        , col="blue")

