# Trying Greasy's code for a heatmap!
# 6/30/2020

# Round 2 Heatmap for EVH:


## complined EVH-EVT heatmap chart

library(readr)
library(ggplot2)

evh_evt5 <- read_csv("evh_evt5.csv", col_types = cols(percent = col_number()))


# order EVHs
evh_evt5$evt  <- factor(evh_evt5$evt, levels = c("Northern Hardwoods Forest","Alkaline Conifer-Hardwood Swamp", "Conifer Acidic Swamp and Treed Poor Fen", "Mesic Balsam Fir-Spruce Forest", "Pine-Hemlock-Hardwood Forest"))

evh_evt5[order(evh_evt5$evt), ]

# make lables
evh_labels <- c("Pine-Hemlock Forest (35,820ac)",
                "Mesic Balsam Fir-Spruce Forest (36,791ac)",
                "Conifer Acidic Swamp and Treed Poor Fen (44,560ac)",
                "Alkaline Conifer-Hardwood Swamp (52,118ac)", 
                "Northern Hardwoods Forest (332,434ac)")



#plot
evh_th1 <-  ggplot(evh_evt5 ,aes(x=height ,y=evt ,fill=percent))+
  #add border white colour of line thickness 0.25
  geom_tile(colour="white",size=0.25)+
  # nice colors
  scale_fill_gradient(low="white", high="purple4") +
  scale_x_continuous(breaks = seq(from = 0, to = 100, by = 10)) +
  #remove x and y axis labels
  labs(x="Canopy Height (Meters)",y="")+
  #remove extra space
  scale_y_discrete(expand=c(0,0))+
  #define new breaks on x-axis
  #scale_x_discrete(breaks=c("10", "20", "30", "40", "50", "60", "70", "80", "90")) +
  #set a base size for all fonts
  theme_bw(base_size = 14) +
  #theme(legend.position = "none") +
  scale_y_discrete(limits = rev(unique(sort(evh_evt5$evt ))), labels= evt_labels,) +
  labs(title = "Tree canopy height for 5 most prevalent ecosystems",
       subtitle = "",
       caption = "LANDFIRE 2016 Existing Vegetation Height data. ") +
  labs(fill = "Percent of \nEcosystem")
#theme options
theme(
  #bold font for legend text
  legend.text=element_text(face="bold"),
  #set thickness of axis ticks
  axis.ticks=element_line(size=0.4),
  #remove plot background
  plot.background=element_blank(),
  #remove plot border
  panel.border=element_blank())+
  theme(plot.caption = element_text(hjust = 0, face= "italic"), #Default is hjust=1
        plot.title.position = "plot", #NEW parameter. Apply for subtitle too.
        plot.caption.position =  "plot")

evh_th1






