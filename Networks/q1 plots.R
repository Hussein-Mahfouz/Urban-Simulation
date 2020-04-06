library(ggplot2)
library(tidyr)
library(ggthemes)


# convert to lon format (for plotting)
inv_shortest_path_lon <- 
            gather(data=inv_shortest_path, key= Method, value=Result, 
                   Betweeness_Iterative, Betweeness_AAO, # columns to be gathered
                   Degree_Iterative, Degree_AAO, 
                   Eigenvector_Iterative, Eigenvector_AAO,
                   Random_Removal,
                   factor_key = TRUE)

gcc_lon <- gather(data=gcc, key= Method, value=Result, 
                  Betweeness_Iterative, Betweeness_AAO, # columns to be gathered
                  Degree_Iterative, Degree_AAO, 
                  Eigenvector_Iterative, Eigenvector_AAO, 
                  Random_Removal,
                  factor_key = TRUE)

# plot on same grid, colour argument groups by values in the column defined: here it is 'Method'
p <- ggplot(data=inv_shortest_path_lon,
            aes(x=Perc_of_Nodes_Removed, y=Result, colour=Method)) +
                 geom_line(aes(linetype=Method, color=Method)) +      #linetype argument added to define types for different methods
                 scale_linetype_manual(values=c("solid", "dotted", "solid", "dotted", "solid", "dotted", "dashed")) +
                 scale_color_manual(values=c('#E69F00','#E69F00', "#009E73", "#009E73", "#800020", "#800020", "#808080")) +
                 labs(y= expression(l^-1), x = "Nodes Removed (%)") +
                 ylim(0, 1) +   # because the eigenvector shoots up to infinity
                 # theme_fivethirtyeight() 
                 theme(legend.position="bottom", 
                       legend.text=element_text(size=rel(0.5)),
                       legend.title = element_blank())

                 #theme_minimal(legend.position="bottom")  # theme_fivethirtyeight()

ggsave(path = "Plots", file="Inv_shortest_path.png", p)

p <- ggplot(data=gcc_lon,
            aes(x=Perc_of_Nodes_Removed, y=Result, colour=Method)) +
                 geom_line(aes(linetype=Method, color=Method)) +      #linetype argument added to define types for different methods
                 scale_linetype_manual(values=c("solid", "dotted", "solid", "dotted", "solid", "dotted", "dashed")) +
                 scale_color_manual(values=c('#E69F00','#E69F00', "#009E73", "#009E73", "#800020", "#800020", "#808080")) +
                 labs(y="S", x = "Nodes Removed (%)") +
                 theme(legend.position="bottom", 
                       legend.text=element_text(size=rel(0.5)),
                       legend.title = element_blank())

ggsave(path = "Plots", file="gcc.png", p)

