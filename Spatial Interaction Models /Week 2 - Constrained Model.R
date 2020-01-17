library(sp)
library(reshape2)
library(geojsonio)
library(rgdal)
library(downloader)
library(maptools)
library(dplyr)
library(broom) 
library(stplanr)
library(ggplot2)
library(MASS)
library(sf)
library(tmap)
library(tmaptools)
library(stringr)

# Unconstrained Gravity Model is sub-optimal as it does not make use of all of the 
# available data in the system we are studying

# It only constrined the TOTAL FLOW SUM
# Constrained Models can constrain flow estimates by incorporating row (origin) totals & column (destination) totals

#run a production constrained SIM (the "-1" indicates no intercept in the regression model).
prodSim <- glm(Total ~ OrigCodeNew+log(wj2_destsal)+log(dist)-1, na.action = na.exclude, family = poisson(link = "log"), data = cdatasub)
#let's have a look at it's summary...
summary(prodSim)

# the α parameter related to the destination attractiveness: 2.04693
#the β distance decay parameter: -2.24218

# Model Estimates

# What were the effects of the constraints?

#create some Oi and Dj columns in the dataframe and store row and column totals in them:
#to create O_i, take cdatasub ...then... group by origcodenew ...then... summarise by calculating the sum of Total
O_i <- cdatasub %>% group_by(OrigCodeNew) %>% summarise(O_i = sum(Total))
cdatasub$O_i <- O_i$O_i[match(cdatasub$OrigCodeNew,O_i$OrigCodeNew)]
D_j <- cdatasub %>% group_by(DestCodeNew) %>% summarise(D_j = sum(Total))
cdatasub$D_j <- D_j$D_j[match(cdatasub$DestCodeNew,D_j$DestCodeNew)]

#fish the coefficients out of the prodsim glm object

#You can do this in a number of ways, for example this prints everything:
prodSim_out <- tidy(prodSim)
prodSim_out


# pull out the parameter values for μi and store them back in the dataframe along with Oi and Dj

# or you can just pull out the coefficients and put them into an object
coefs <- as.data.frame(prodSim$coefficients)
#then once you have done this, you can join them back into the dataframe using a regular expression to match the bits of the identifier that you need - *note, this bit of code below took me about 2 hours to figure out!*
cdatasub$mu_i <- coefs$`prodSim$coefficients`[match(cdatasub$OrigCodeNew,sub(".*OrigCodeNew","", rownames(coefs)))]
#now, where we have missing values for our reference mu_i variable, fill those with 1s
head(cdatasub)

# save our parameter values into some variables…

mu_i <- prodSim$coefficients[1:7]
alpha <- prodSim$coefficients[8]
beta <- prodSim$coefficients[9]

#we’re ready to generate our estimates:
cdatasub$prodsimest1 <- exp((cdatasub$mu_i)+(alpha*log(cdatasub$wj2_destsal))+(beta*log(cdatasub$dist)))

# easy way to do the above (Hard way is good for playing around with parameters):

cdatasub$prodsimFitted <- fitted(prodSim)

head(cdatasub)

# ASSESSING MODEL OUTPUT

# Flow Matrix

#first round the estimates
cdatasub$prodsimFitted <- round(fitted(prodSim),0)
#now we can create pivot table to turn paired list into matrix (and compute the margins as well)
cdatasubmat3 <- dcast(cdatasub, Orig ~ Dest, sum, value.var = "prodsimFitted", margins=c("Orig", "Dest"))
cdatasubmat3

# How do the fits compare with the unconstrained model from last time?

#use the functions from the last practical to calculate some goodness-of-fit statistics

CalcRSquared(cdatasub$Total,cdatasub$prodsimFitted) #0.81

CalcRMSE(cdatasub$Total,cdatasub$prodsimFitted)  #1382


# WHAT IF SCENARIOS

#What if the government invested loads of money into a new Car Plant in Barking and Dagenham and as a 
#result, average wages increased from a mere £16,200 to £25,000. A far fetched scenario, but one that 
#could make a good experiment.

#create create a new variable with these altered salaries:
  
cdatasub$wj3_destsalScenario <- cdatasub$wj2_destsal
cdatasub$wj3_destsalScenario <- ifelse(cdatasub$wj3_destsalScenario == 16200,25000,cdatasub$wj3_destsalScenario)

#plug these new values into the model and see how this changes the flows in the system…

cdatasub$prodsimest2 <- exp((cdatasub$mu_i)+(alpha*log(cdatasub$wj3_destsalScenario))+(beta*log(cdatasub$dist)))

cdatasub$prodsimest2 <- round(cdatasub$prodsimest2,0)
#now we can create pivot table to turn paired list into matrix (and compute the margins as well)
cdatasubmat4 <- dcast(cdatasub, Orig ~ Dest, sum, value.var = "prodsimest2", margins=c("Orig", "Dest"))
cdatasubmat4

# INFLOW INCREASED FROM 6092 to 14787 but inflows to other areas haven't decreased...
# Constraints not warking for Barking and Dagenham

#return to the multiplicative model in Equation 1 and run this model after calculating our own Ai balancing factors.

#calculate some new wj^alpha and dij^beta values
wj2_alpha <- cdatasub$wj2_destsal^alpha
dist_beta <- cdatasub$dist^beta
#calculate the first stage of the Ai values
cdatasub$Ai1 <- wj2_alpha*dist_beta
#now do the sum over all js bit
A_i <- cdatasub %>% group_by(OrigCodeNew) %>% summarise(A_i = sum(Ai1))
#now divide in to 1
A_i[,2] <- 1/A_i[,2]
#and write the A_i values back into the data frame
cdatasub$A_i <- A_i$A_i[match(cdatasub$OrigCodeNew,A_i$OrigCodeNew)]

#To check everything works, recreate the original estimates
cdatasub$prodsimest3 <- cdatasub$A_i*cdatasub$O_i*wj2_alpha*dist_beta

#Let’s try with our new values for the destination salary in Barking and Dagenham

#calculate some new wj^alpha and dij^beta values
wj3_alpha <- cdatasub$wj3_destsalScenario^alpha
#calculate the first stage of the Ai values
cdatasub$Ai1 <- wj3_alpha*dist_beta
#now do the sum over all js bit
A_i <- cdatasub %>% group_by(OrigCodeNew) %>% summarise(A_i = sum(Ai1))
#now divide in to 1
A_i[,2] <- 1/A_i[,2]
#and write the A_i values back into the data frame
cdatasub$A_i <- A_i$A_i[match(cdatasub$OrigCodeNew,A_i$OrigCodeNew)]

#To check everything works, recreate the original estimates
cdatasub$prodsimest4_scenario <- cdatasub$A_i*cdatasub$O_i*wj3_alpha*dist_beta

cdatasub$prodsimest4_scenario <- round(cdatasub$prodsimest4_scenario,0)
#now we can create pivot table to turn paired list into matrix (and compute the margins as well)
cdatasubmat5 <- dcast(cdatasub, Orig ~ Dest, sum, value.var = "prodsimest4_scenario", margins=c("Orig", "Dest"))
cdatasubmat5


