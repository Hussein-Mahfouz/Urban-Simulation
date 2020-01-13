# http://rpubs.com/adam_dennett/257231

library(sp)
library(MASS)
library(reshape2)
library(geojsonio)
library(rgdal)
library(downloader)
library(maptools)
library(dplyr)
library(broom) 
library(stplanr)
library(ggplot2)
library(leaflet)

#Fetch a GeoJson of some district-level boundaries from the ONS Geoportal. First add the URL to an object
EW <- geojson_read("http://geoportal.statistics.gov.uk/datasets/8edafbe3276d4b56aec60991cbddda50_2.geojson", 
                   what = "sp")

#have a quick look at the top of the data file
head(EW@data)

#pull out london using grep and the regex wildcard for'start of the string' (^) to to look for the bit of 
#the district code that relates to London (E09) from the 'lad15cd' column in the data slot of our spatial polygons dataframe
London <- EW[grep("^E09",EW@data$lad15cd),]
#plot it
plot(London)

#and have a look under the bonnet
summary(London)

#CALCULATING A DISTANCE MATRIX

#boundaries we have are not in British National Grid - the bit that says proj4string tells me 
#that we are in WGS84 or using latitude and longitude coordinates. We need to change this to 
#British National Grid so our distances are in metres and not decimal degrees, then do everything 
#we need to do to generate a distance matrix.

#first transfrom to BNG - this will be important for calculating distances using spTransform
BNG = "+init=epsg:27700"
LondonBNG <- spTransform(London, BNG)
#now, order by borough code - *this step will be imporant later on*
LondonBNG <- LondonBNG[order(LondonBNG$lad15cd),]
#now use spDists to generate a big distance matrix of all distances between boroughs in London
dist <- spDists(LondonBNG)
#melt this matrix into a list of origin/destination pairs using melt. Melt in in the reshape2 package. Reshape2, dplyr and ggplot, together, are some of the best packages in R, so if you are not familiar with them, get googling and your life will be much better!
distPair <- melt(dist)

# FLOW DATA

#read in your London Commuting Data
cdata <- read.csv("https://www.dropbox.com/s/7c1fi1txbvhdqby/LondonCommuting2001.csv?raw=1")
#read in a lookup table for translating between old borough codes and new borough codes
CodeLookup <- read.csv("https://www.dropbox.com/s/h8mpvnepdkwa1ac/CodeLookup.csv?raw=1")
#read in some population and income data
popincome <- read.csv("https://www.dropbox.com/s/84z22a4wo3x2p86/popincome.csv?raw=1")

#now merge these supplimentary data into your flow data dataframe
cdata$OrigCodeNew <- CodeLookup$NewCode[match(cdata$OrigCode, CodeLookup$OldCode)]
cdata$DestCodeNew <- CodeLookup$NewCode[match(cdata$DestCode, CodeLookup$OldCode)]
cdata$vi1_origpop <- popincome$pop[match(cdata$OrigCodeNew, popincome$code)]
cdata$vi2_origsal <- popincome$med_income[match(cdata$OrigCodeNew, popincome$code)]
cdata$wj1_destpop <- popincome$pop[match(cdata$DestCodeNew, popincome$code)]
cdata$wj2_destsal <- popincome$med_income[match(cdata$DestCodeNew, popincome$code)]

#Data needs to be ordered by borough code, if it's not, we will run into problems when 
#we try to merge our distance data back in later, so to make sure, we can arrange by orign 
#and then destination using dplyr's 'arrange' function

cdata <- arrange(cdata, OrigCodeNew, DestCodeNew)



#First create a new total column which excludes intra-borough flow totals (well sets them to a very very small number for reasons you will see later...)
cdata$TotalNoIntra <- ifelse(cdata$OrigCode == cdata$DestCode,0,cdata$Total)
cdata$offset <- ifelse(cdata$OrigCode == cdata$DestCode,0.0000000001,1)
# now add the distance column into the dataframe
cdata$dist <- distPair$value

head(cdata)

# to make this demonstration even easier, let’s just select a small subset of these 
# flows (we can come back to the whole dataset later on

#We'll just use the first 7 boroughs by code, so first, create a vector of these 7 to match with our data
toMatch<-c("00AA", "00AB", "00AC", "00AD", "00AE", "00AF", "00AG")
#subset the data by the 7 sample boroughs
#first the origins
cdatasub <- cdata[grep(paste(toMatch,collapse = "|"), cdata$OrigCode),]
#then the destinations
cdatasub <- cdatasub[grep(paste(toMatch,collapse = "|"), cdata$DestCode),]
#now chop out the intra-borough flows
cdatasub <- cdatasub[cdatasub$OrigCode!=cdatasub$DestCode,]
#now unfortunately if you look at the file, for some reason the grep process has left a lot of empty data cells in the dataframe, so let's just chop out everything after the 7*7 - 7 (42) pairs we are interested in...
cdatasub <- cdatasub[1:42,]
#now re-order so that OrigCodeNew, DestCodeNew and TotalNoIntra are the first three columns *note that you have to be explicit about the select function in the dplyr package as MASS also has a 'select' function and R will try and use this by default. We can be explict by using the syntax package::function
cdatasub <- dplyr::select(cdatasub, OrigCodeNew, DestCodeNew, Total, everything())
# re-order so that 'lad15cd' is the first column in LondonBNG
cdatasub <- dplyr::select(cdatasub, OrigCodeNew, DestCodeNew, Total, everything())

# HUSSEIN
library(sf)
LondonBNG_sf <- st_as_sf(LondonBNG)
# re-order so that 'lad15cd' is the first column in LondonBNG
LondonBNG_sf <- dplyr::select(LondonBNG_sf, lad15cd, everything())
# convert back to sp
LondonBNG <- as(LondonBNG_sf, 'Spatial')
# End HUSSEIN
 
#use the od2line function from Robin Lovelace's excellent stplanr package
travel_network <- od2line(flow = cdatasub, zones = LondonBNG)
#and set the line widths to some sensible value according to the flow
w <- cdatasub$Total / max(cdatasub$Total) * 10
#now plot it...
plot(travel_network, lwd = w)
plot(LondonBNG, add=T)

