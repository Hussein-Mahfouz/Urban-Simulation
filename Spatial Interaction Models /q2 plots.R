library(tidyverse)
library(reshape2)

# to get flows reaching each Borough
city_salary <- cdata %>% group_by(Dest) %>%
                         summarise(intial_flows = sum(prodsimFitted),
                                   sal_reduce   = sum(prodsimest4_scenario))

# names(city_salary)[2] <- "Allocated"

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
