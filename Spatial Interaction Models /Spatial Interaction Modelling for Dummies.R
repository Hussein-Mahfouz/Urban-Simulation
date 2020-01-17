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
# re-order so that 'lad15cd' is the first column in LondonBNG - OTHERWISE od2line WON'T WORK
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

# leaflet map

#transform to wgs84
travel_networkwgs <- spTransform(travel_network, "+init=epsg:4326")
#plot in leaflet
leaflet() %>% addTiles() %>% addPolylines(data = travel_networkwgs)


#now we can create pivot table to turn paired list into matrix (and compute the margins as well)
cdatasubmat <- dcast(cdatasub, Orig ~ Dest, sum, value.var = "Total", margins=c("Orig", "Dest"))
cdatasubmat

# MODELLING

#First plot the commuter flows against distance and then fit a model line with a ^-2 parameter
# -2 is the parameter used for Newton's Gravity Model. We are just using it as a starting point
plot1 <- qplot(cdata$dist, cdata$Total)
#and now the model fit...
plot1 + stat_function(fun=function(x)x^-2, geom="line", aes(colour="^-2"))

#now, what about the origin and destination data...
plot2 <- qplot(cdata$vi1_origpop, cdata$Total)
plot2 + stat_function(fun=function(x)x^1, geom="line", aes(colour="^1"))

plot3 <- qplot(cdata$wj2_destsal, cdata$Total)
plot3 + stat_function(fun=function(x)x^1, geom="line", aes(colour="^1"))

#OK, so it looks like we’re not far off (well, destination salary doesn’t look too promising as a predictor, 
#but we’ll see how we get on…), so let’s see what flow estimates with these starting parameters look like.

#set up some variables to hold our parameter values in:
mu <- 1
alpha <- 1
beta <- -2
k <- 1
T2 <- sum(cdatasub$Total)

#Now let’s create some flow estimates using Equation 2 above… Begin by applying the parameters to the variables:

vi1_mu <- cdatasub$vi1_origpop^mu
wj2_alpha <- cdatasub$wj2_destsal^alpha
dist_beta <- cdatasub$dist^beta
T1 <- vi1_mu*wj2_alpha*dist_beta
k <- T2/sum(T1)

#run the model and store all of the new flow estimates in a new column in the dataframe
cdatasub$unconstrainedEst1 <- round(k*vi1_mu*wj2_alpha*dist_beta,0)
#check that the sum of these estimates makes sense
sum(cdatasub$unconstrainedEst1)

#turn it into a little matrix and have a look at your handy work
cdatasubmat1 <- dcast(cdatasub, Orig ~ Dest, sum, value.var = "unconstrainedEst1", margins=c("Orig", "Dest"))
cdatasubmat1

# How do the flow estimates compare with the actual flows? Eyeballing works, but we need something more mathy

# TESTING THE “GOODNESS-OF-FIT”.

# METHOD 1: R-Squared
CalcRSquared <- function(observed,estimated){
  r <- cor(observed,estimated)
  R2 <- r^2
  R2
}

CalcRSquared(cdatasub$Total,cdatasub$unconstrainedEst1)

# our model accounts for about 51% of the variation of flows in the system. Not bad, but not brilliant either.

# METHOD 2: RMSE

CalcRMSE <- function(observed,estimated){
  res <- (observed - estimated)^2
  RMSE <- round(sqrt(mean(res)),3)
  RMSE
}

CalcRMSE(cdatasub$Total,cdatasub$unconstrainedEst1)

# The closer to 0 the RMSE value, the better the model. (Now it is 2503...let's try to improve it)

# POISSON REGRESSION - To Calibrate

# Flow distribution 
qplot(cdata$Total) + geom_histogram()   # If it looks like Poisson, and it quacks like Poisson...

qplot(log(dist), log(Total), data=cdata) + geom_smooth(method = lm)


#run the unconstrained model
uncosim <- glm(Total ~ log(vi1_origpop)+log(wj2_destsal)+log(dist), na.action = na.exclude, family = poisson(link = "log"), data = cdatasub)

summary(uncosim) 

# Calibrated values

# k  (intercept) = -15.631802
# μ = 1.747997
# α = 1.642331
# β = -1.411889

# Calculate Flow Estimates Using Calibrated Coefficients

#first asign the parameter values from the model to the appropriate variables
k <- uncosim$coefficients[1]
mu <- uncosim$coefficients[2]
alpha <- uncosim$coefficients[3]
beta <- -uncosim$coefficients[4]

#now plug everything back into the Equation 6 model... (be careful with the positive and negative signing of the parameters as the beta parameter may not have been saved as negative so will need to force negative)
cdatasub$unconstrainedEst2 <- exp(k+(mu*log(cdatasub$vi1_origpop))+(alpha*log(cdatasub$wj2_destsal))-(beta*log(cdatasub$dist)))

#which is exactly the same as this...
cdatasub$unconstrainedEst2 <- (exp(k)*exp(mu*log(cdatasub$vi1_origpop))*exp(alpha*log(cdatasub$wj2_destsal))*exp(-beta*log(cdatasub$dist)))

#and of course, being R, there is an even easier way of doing this...
cdatasub$fitted <- fitted(uncosim)

#run the model and store all of the new flow estimates in a new column in the dataframe
cdatasub$unconstrainedEst2 <- round(cdatasub$unconstrainedEst2,0)
sum(cdatasub$unconstrainedEst2)

#turn it into a little matrix and have a look at your handy work
cdatasubmat2 <- dcast(cdatasub, Orig ~ Dest, sum, value.var = "unconstrainedEst2", margins=c("Orig", "Dest"))
cdatasubmat2

# And the $1,000,000 question - has calibrating the parameters improved the model…?
CalcRSquared(cdatasub$Total,cdatasub$unconstrainedEst2)

CalcRMSE(cdatasub$Total,cdatasub$unconstrainedEst2)


