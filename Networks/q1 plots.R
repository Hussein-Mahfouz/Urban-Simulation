library(ggplot2)
library(tidyr)
library(ggthemes)


# convert to lon format (for plotting)
inv_shortest_path_lon <- 
            gather(data=inv_shortest_path, key= Method, value=Result, 
                   Normalized_Betweeness_Iterative, Normalized_Betweeness_AAO, # columns to be gathered
                   Normalized_Degree_Iterative, Normalized_Degree_AAO, Random_Removal,
                   factor_key = TRUE)

gcc_lon <- gather(data=gcc, key= Method, value=Result, 
                  Betweeness_Iterative, Betweeness_AAO, # columns to be gathered
                  Degree_Iterative, Degree_AAO, Random_Removal,
                  factor_key = TRUE)

# plot on same grid, colour argument groups by values in the column defined: here it is 'Method'
ggplot(data=inv_shortest_path_lon,
       aes(x=Perc_of_Nodes_Removed, y=Result, colour=Method)) +
            geom_line(aes(linetype=Method, color=Method)) +      #linetype argument added to define types for different methods
            scale_linetype_manual(values=c("solid", "dotted", "solid", "dotted", "dashed")) +
            scale_color_manual(values=c('#E69F00','#E69F00', "#009E73", "#009E73", "#808080")) +
            labs(y="Mean Inverse Shortest Path Length", x = "Nodes Removed (%)") +
            theme_minimal()  # theme_fivethirtyeight()

ggplot(data=gcc_lon,
       aes(x=Perc_of_Nodes_Removed, y=Result, colour=Method)) +
            geom_line(aes(linetype=Method, color=Method)) +      #linetype argument added to define types for different methods
            scale_linetype_manual(values=c("solid", "dotted", "solid", "dotted", "dashed")) +
            scale_color_manual(values=c('#E69F00','#E69F00', "#009E73", "#009E73", "#808080")) +
            labs(y="Size of GCC as Percentage of No. of Nodes", x = "Nodes Removed (%)") +
            theme_minimal()  # theme_fivethirtyeight()