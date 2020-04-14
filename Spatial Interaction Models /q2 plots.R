library(tidyverse)
library(reshape2)

# to get flows reaching each Borough
city_salary <- cdata %>% group_by(Dest) %>%
                         summarise(intial_flows = sum(prodsimFitted),
                                   sal_reduce   = sum(prodsimest4_scenario))

#change column names
names(city_salary)[2] <- "Intitial Flows"
names(city_salary)[3] <- "New Flows"

# convert from wide to long format. This way we can add a legend
city_salary_long <- melt(city_salary, id.vars = c("Dest"))

# plot
p <- ggplot(data=city_salary_long, aes(x=Dest, y=value, fill=variable, color=variable, alpha=variable)) +
            geom_bar(stat="identity", position ="identity") +
            scale_colour_manual(values=c("lightblue4","red")) +
            scale_fill_manual(values=c("lightblue","pink")) +
            scale_alpha_manual(values=c(.8, .1)) + 
            theme(axis.text.x = element_text(angle=57, vjust = 1, hjust = 1),
                  axis.text.y = element_text(angle=20))
            #coord_flip()
p
# Change the titles  
p <-  p + ggtitle("Change in Flows - Salary Decrease in City of London") +
  ylab("Flow Arriving at Borough") 
# Remove y axis title
p <- p + theme(axis.title.x = element_blank(),
               legend.title=element_blank())

p

ggsave(path = "Plots", file="City_of_London.png", p, width = 10, height = 6)


# effect of changing Beta
# We will see the average commuting distance arriving at each borough

# to get flows reaching each Borough
# sum(distance * no. of people) / sum(no. of people)
avg_dist <- cdata %>% group_by(Dest) %>%
                  summarise(avg_dist_travelled        = ((sum(dist * prodsimFitted)) / sum(prodsimFitted)) / 1000,
                            avg_dist_travelled_3      = ((sum(dist * prodsimest5_scenario)) / sum(prodsimest5_scenario)) / 1000,
                            avg_dist_travelled_5      = ((sum(dist * prodsimest6_scenario)) / sum(prodsimest6_scenario)) / 1000,
                            perc_avg_dist_decrease_3  = (abs(avg_dist_travelled_3 - avg_dist_travelled)/avg_dist_travelled) *100,
                            perc_avg_dist_decrease_5  = (abs(avg_dist_travelled_5 - avg_dist_travelled)/avg_dist_travelled) *100,
                            tot_dist_travelled        = sum(dist * prodsimFitted),
                            tot_dist_travelled_3      = sum(dist * prodsimest5_scenario) / 1000,
                            tot_dist_travelled_5      = sum(dist * prodsimest6_scenario) / 1000,
                            perc_dist_decrease_3      = (abs(tot_dist_travelled_3 - tot_dist_travelled)/tot_dist_travelled) *100,
                            perc_dist_decrease_5      = (abs(tot_dist_travelled_5 - tot_dist_travelled)/tot_dist_travelled) *100)

#subset df for plotting
avg_dist_plot <- avg_dist %>% select(Dest, avg_dist_travelled, avg_dist_travelled_3, avg_dist_travelled_5)

# change names 
names(avg_dist_plot)[2] <- "Beta = -1.82"
names(avg_dist_plot)[3] <- "Beta = -3.64"
names(avg_dist_plot)[4] <- "Beta = -5.46"
# convert from wide to long format. This way we can add a legend
avg_dist_long <- melt(avg_dist_plot, id.vars = c("Dest"))

#plot

p <- ggplot(data=avg_dist_long, aes(x=Dest, y=value, fill=variable, color=variable, alpha=variable)) +
                                      geom_bar(stat="identity", position ="identity") +
                                      #scale_colour_manual(values=c("lightblue4","red", "#f7f7f7")) +
                                      #scale_fill_manual(values=c("lightblue","pink", "#f7f7f7")) +
                                      scale_colour_manual(values=c("lightblue4","red", "black")) +
                                      scale_fill_manual(values=c("lightblue","pink", "black")) +
                                      scale_alpha_manual(values=c(1, .5, .1)) + 
                                      theme(axis.text.x = element_text(angle=60, vjust = 1, hjust = 1),
                                            axis.text.y = element_text(angle=20)) 
p
# Change the titles  
p <-  p + ggtitle("Effect of Distance Parameter on Average Commuting Distance to Borough") +
  ylab("Average Distance Travelled To Borough (km)") 
# Remove y axis title
p <- p + theme(axis.title.x = element_blank(),
               legend.title=element_blank())
p

ggsave(path = "Plots", file="Avg_Distance_Commuting.png", p, width = 10, height = 6)

#% decrease in avg distance travelled
perc_avg_dist_plot <- avg_dist %>% select(Dest, perc_avg_dist_decrease_3, perc_avg_dist_decrease_5)

# change names 
names(perc_avg_dist_plot)[2] <- "Beta = -3.64"
names(perc_avg_dist_plot)[3] <- "Beta = -5.46"
# convert from wide to long format. This way we can add a legend
perc_avg_dist_long <- melt(perc_avg_dist_plot, id.vars = c("Dest"))


#plot

p <- ggplot(data=perc_avg_dist_long, aes(x=Dest, y=value, fill=variable, color=variable, alpha=variable)) +
            geom_bar(stat="identity", position ="identity") +
            scale_colour_manual(values=c("lightblue4","red")) +
            scale_fill_manual(values=c("lightblue","pink")) +
            scale_alpha_manual(values=c(.8, .2)) + 
            theme(axis.text.x = element_text(angle=60, vjust = 1, hjust = 1),
                  axis.text.y = element_text(angle=20)) 
p
# Change the titles  
p <-  p + ggtitle("Effect of Distance Parameter on Average Commuting Distance to Borough") +
          ylab("% Decrease in Avg Distance Travelled to Borough ") 
# Remove y axis title
p <- p + theme(axis.title.x = element_blank(),
               legend.title=element_blank())
p

ggsave(path = "Plots", file="Perc_Avg_Distance_Commuting.png", p, width = 10, height = 6)
