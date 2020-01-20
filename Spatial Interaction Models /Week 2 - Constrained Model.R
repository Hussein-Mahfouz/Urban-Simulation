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
#round values
cdatasub$prodsimest4_scenario <- round(cdatasub$prodsimest4_scenario,0)
#now we can create pivot table to turn paired list into matrix (and compute the margins as well)
cdatasubmat5 <- dcast(cdatasub, Orig ~ Dest, sum, value.var = "prodsimest4_scenario", margins=c("Orig", "Dest"))
cdatasubmat5


# ATTRACTION CONSTRAINED MODEL
attrSim <- glm(Total ~ DestCodeNew+log(vi1_origpop)+log(dist)-1, na.action = na.exclude, family = poisson(link = "log"), data = cdatasub)
summary(attrSim)

# Examine how the constraints hold for destinations this time:
  
#first round the estimates
cdatasub$attrsimFitted <- round(fitted(attrSim),0)
#now we can create pivot table to turn paired list into matrix (and compute the margins as well)
cdatasubmat6 <- dcast(cdatasub, Orig ~ Dest, sum, value.var = "attrsimFitted", margins=c("Orig", "Dest"))
cdatasubmat6

#use the functions from the last practical to calculate some goodness-of-fit statistics
CalcRSquared(cdatasub$Total,cdatasub$attrsimFitted)
CalcRMSE(cdatasub$Total,cdatasub$attrsimFitted)

# DOUBLY CONSTRAINED MODEL

# calculation of Ai relies on knowing Bj and the calculation of Bj relies on knowing Ai. A conundrum!! 
# If I don’t know Ai how can I calcuate Bj and then in turn Ai and then Bj ad infinitum???!!

# there is an algorithm for iteratively rriving at values for Ai and Bj

# OR through R

#run a doubly constrained SIM
doubSim <- glm(Total ~ Orig+Dest+log(dist)-1, na.action = na.exclude, family = poisson(link = "log"), data = cdatasub)
#let's have a look at it's summary...
summary(doubSim)

# various flows and goodness-of-fit statistics?

#then round the estimates
cdatasub$doubsimFitted <- round(fitted(doubSim),0)
#now we can create pivot table to turn paired list into matrix (and compute the margins as well)
cdatasubmat7 <- dcast(cdatasub, Orig ~ Dest, sum, value.var = "doubsimFitted", margins=c("Orig", "Dest"))
cdatasubmat7

# use the functions from the last practical to calculate some goodness-of-fit statistics
CalcRSquared(cdatasub$Total,cdatasub$doubsimFitted)
CalcRMSE(cdatasub$Total,cdatasub$doubsimFitted)

# the model is now producing some good estimates. However, there are still some errors in the 
# flows, particularly for estimates between Barking and Dagenham and Bexley or Barnet and Camden.

# TWEAKING THE MODEL

# we have assumed that distance decay parameter follows a negative power law, BUT IT DOESN'T HAVE TO

xdistance <- seq(1,20,by=1)
# negative power
InvPower2 <- xdistance^-2
# negative exponential
NegExp0.3 <- exp(-0.3*xdistance)

df <- cbind(InvPower2,NegExp0.3)
meltdf <- melt(df)
ggplot(meltdf,aes(Var1,value, colour = Var2)) + geom_line()

# if observed flows drop very quickly with distance then the inverse power law is more appropriate
# if the effect of distance are less severe then the negative exponential function might be more appropriate

# test the negative exponential by substituting  −βlndij for −βdij in our model:

#run a production constrained SIM
doubSim1 <- glm(Total ~ Orig+Dest+dist-1, na.action = na.exclude, family = poisson(link = "log"), data = cdatasub)
#let's have a look at it's summary...
summary(doubSim1)

cdatasub$doubsimFitted1 <- round(fitted(doubSim1),0)

CalcRSquared(cdatasub$Total,cdatasub$doubsimFitted1)
CalcRMSE(cdatasub$Total,cdatasub$doubsimFitted1)

# negative exponential improves the fit!

# Add predictor variables and see whether they have an effect

# For example, instead of modelling total flows, we could try and model motorbike 
# commuters using information on car and underground commuters
kitchensinkSIM <- glm(Motobike ~ Orig+Dest+dist+CarDrive+Underground-1, na.action = na.exclude, family = poisson(link = "log"), data = cdatasub)
#let's have a look at it's summary...
summary(kitchensinkSIM)
# Some of the dummy / constraint origins become statistically 
# insignificant when car and tube commuters are added into the mix.



# Senior Algorithm to calculate Ai and Bj iteratively 


##########################################################################
#This block of code will calculate balancing factors for an entropy
#maximising model (doubly constrained)

#set beta to the appropriate value according to whether exponential or power
if(tail(names(coef(doubSim)),1)=="dist"){
  cdatasub$beta <- coef(doubSim)["dist"]
  disdecay = 0
} else {
  cdatasub$beta <- coef(doubSim)["log(dist)"]
  disdecay = 1
}

#Create some new Ai and Bj columns and fill them with starting values
cdatasub$Ai <- 1
cdatasub$Bj <- 1
cdatasub$OldAi <- 10
cdatasub$OldBj <- 10
cdatasub$diff <- abs((cdatasub$OldAi-cdatasub$Ai)/cdatasub$OldAi)

#create convergence and iteration variables and give them initial values
cnvg = 1
its = 0

#This is a while-loop which will calculate Orig and Dest balancing
#factors until the specified convergence criteria is met
while(cnvg > 0.001){
  print(paste0("iteration ", its))
  its = its + 1 #increment the iteration counter by 1
  #First some initial calculations for Ai...
  if(disdecay==0){
    cdatasub$Ai <- (cdatasub$Bj*cdatasub$D_j*exp(cdatasub$dist*cdatasub$beta))
  } else {
    cdatasub$Ai <- (cdatasub$Bj*cdatasub$D_j*exp(log(cdatasub$dist)*cdatasub$beta))
  }  
  #aggregate the results by your Origs and store in a new dataframe
  AiBF <- aggregate(Ai ~ Orig, data = cdatasub, sum)
  #now divide by 1
  AiBF$Ai <- 1/AiBF$Ai 
  #and replace the initial values with the new balancing factors
  updates = AiBF[match(cdatasub$Orig,AiBF$Orig),"Ai"]
  cdatasub$Ai = ifelse(!is.na(updates), updates, cdatasub$Ai)
  #now, if not the first iteration, calculate the difference between  the new Ai values and the old Ai values and once done, overwrite the old Ai values with the new ones. 
  if(its==1){
    cdatasub$OldAi <- cdatasub$Ai    
  } else {
    cdatasub$diff <- abs((cdatasub$OldAi-cdatasub$Ai)/cdatasub$OldAi)    
    cdatasub$OldAi <- cdatasub$Ai
  }
  
  #Now some similar calculations for Bj...
  if(disdecay==0){
    cdatasub$Bj <- (cdatasub$Ai*cdatasub$O_i*exp(cdatasub$dist*cdatasub$beta))
  } else {
    cdatasub$Bj <- (cdatasub$Ai*cdatasub$O_i*exp(log(cdatasub$dist)*cdatasub$beta))
  }
  #aggregate the results by your Dests and store in a new dataframe
  BjBF <- aggregate(Bj ~ Dest, data = cdatasub, sum)
  #now divide by 1
  BjBF$Bj <- 1/BjBF$Bj  
  #and replace the initial values by the balancing factor
  updates = BjBF[match(cdatasub$Dest,BjBF$Dest),"Bj"]
  cdatasub$Bj = ifelse(!is.na(updates), updates, cdatasub$Bj)
  #now, if not the first iteration, calculate the difference between the new Bj values and the old Bj values and once done, overwrite the old Bj values with the new ones.
  if(its==1){
    cdatasub$OldBj <- cdatasub$Bj
  } else {
    cdatasub$diff <- abs((cdatasub$OldBj-cdatasub$Bj)/cdatasub$OldBj)    
    cdatasub$OldBj <- cdatasub$Bj
  } 
  #overwrite the convergence variable with 
  cnvg = sum(cdatasub$diff)
}
## [1] "iteration 0"
## [1] "iteration 1"
## [1] "iteration 2"
## [1] "iteration 3"
## [1] "iteration 4"
## [1] "iteration 5"
## [1] "iteration 6"
## [1] "iteration 7"
## [1] "iteration 8"
# So, we’ve calculated our Ai and Bj values using out iterative routine - now we can plug them back into our model, to prove, once again, that the Poisson Model is exactly the same as the multiplicative Entropy Maximising Model…


# plug them back into our model, to prove, once again, that the Poisson Model is exactly the 
# same as the multiplicative Entropy Maximising Model
########################################################################
#Now create some SIM estimates
if(disdecay==0){
  cdatasub$SIM_Estimates <- (cdatasub$O_i*cdatasub$Ai*cdatasub$D_j*cdatasub$Bj*exp(cdatasub$dist*cdatasub$beta))
} else{
  cdatasub$SIM_Estimates_pow <- (cdatasub$O_i*cdatasub$Ai*cdatasub$D_j*cdatasub$Bj*exp(log(cdatasub$dist)*cdatasub$beta))
}
########################################################################

cdatasub$SIM_Estimates <- round(cdatasub$SIM_Estimates,0)
cdatasubmat8 <- dcast(cdatasub, Orig ~ Dest, sum, value.var = "SIM_Estimates", margins=c("Orig", "Dest"))
cdatasubmat8












