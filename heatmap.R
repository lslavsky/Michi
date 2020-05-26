# Trying Greasy's code for a heatmap!
# 5/25/2020

library(readr)
library(ggplot2)

evh_evt5 <- read_csv("evh_evt5.csv", col_types = cols(percent = col_number()))

# order EVHs
evh_evt5$evt  <- factor(evh_evt5$evt, levels = c("Northern Hardwoods Forest","Alkaline Conifer-Hardwood Swamp", "Conifer Acidic Swamp and Treed Poor Fen", "Mesic Balsam Fir-Spruce Forest", "Pine-Hemlock-Hardwood Forest"))

# I think line 10 is where you "lost" the Pine-Hemlock Forest?  It has a typo and should be "Pine-Hemlock-Hardwood Forest".  I changed it to try.  

evh_evt5[order(evh_evt5$evt), ]

#plot
evh_th1 <-  ggplot(evh_evt5 ,aes(x=height ,y=evt ,fill=percent))+
  geom_tile()

evh_th1 

# A start! Now to make it look a smidge better:
# Using Greasy's code for each graph (I tried to combine some lines to shorten and make fewer graphs but kept getting errors so just used them all.) 


evh_th2  <- ggplot(evh_evt5 ,aes(x=height ,y=evt ,fill=percent))+
  geom_tile(colour="white",size=0.25)+
  labs(x="Canopy height ()",y="")+
  scale_y_discrete(expand=c(0,0))+
  theme_grey(base_size=8)+
  theme(
    legend.text=element_text(face="bold"),
    axis.ticks=element_line(size=0.4),
    plot.background=element_blank(),
    panel.border=element_blank())
evh_th2 


evh_th3 <-
  evh_th2 + 
  scale_fill_gradient(low="white", high="purple4") +
  scale_x_continuous(breaks = seq(from = 0, to = 100, by = 10)) 
evh_th3


evh_th4 <-
  evh_th3 +
  theme_bw(base_size = 14) +
  #theme(legend.position = "none") +
  scale_y_discrete(limits = rev(unique(sort(evh_evt5$evt )))) +
  labs(title = "Tree canopy height for 5 most prevalent ecosystems",
       subtitle = "",
       caption = "LANDFIRE 2016 Existing Vegetation Cover data. ") +
  labs(fill = "Percent of \nEcosystem")
evh_th4 


evt_labels <- c("Pine-Hemlock Forest (35,820ac)",
                "Mesic Balsam Fir-Spruce Forest (36,791ac)",
                "Conifer Acidic Swamp and Treed Poor Fen (44,560ac)",
                "Alkaline Conifer-Hardwood Swamp (52,118ac)", 
                "Northern Hardwoods Forest (332,434ac)")


evh_th5 <- evh_th4 +
  scale_y_discrete(limits = rev(unique(sort(evh_evt5$evt ))), labels= evt_labels,) +
  theme(plot.caption = element_text(hjust = 0, face= "italic"), #Default is hjust=1
        plot.title.position = "plot", #NEW parameter. Apply for subtitle too.
        plot.caption.position =  "plot")

# didn't need to have "rev" in that last line...could've had labels in correct order to begin with
evh_th5


# the NA didn't graph but I left it with "top 5" for when it's fixed. 









