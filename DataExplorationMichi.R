# Exploring Data for Michi 
#Libraries

library(readr)
library(ggplot2)
library(dplyr)
library(scales)
library(DT)
options(scipen = 999)

# Reading in the csv cleaned data
Michi <- read.csv("michiAllClean.csv")

Michi

#basic graphs 
ggplot(Michi, aes(x = BPS_GP_NAME, y = ACRES)) +
geom_bar(stat = "identity", fill = "grey")

# Let's make that in descending order
Michi_Arr <- arrange(Michi, desc(ACRES))

# The data frame is in desc order by acres

# And redo the basic ggplot
ggplot(Michi_Arr, aes(x = BPS_GP_NAME, y = ACRES)) +
  geom_bar(stat = "identity", fill = "grey")

# That didn't work...

# Ok, got a basic bar graph for the BPS Group Names. 
# Now to make it prettier using code from MIProj
ggplot(Michi_Arr, aes(x = reorder(BPS_GP_NAME, ACRES), y = ACRES)) + 
  geom_bar(stat = "identity", fill = "grey") +
  coord_flip() +
  ggtitle("Group BPS Acres") +
  labs(x = "", y = "Acres") +
  scale_y_continuous(labels = comma) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))

# Still does not go in desc. order. 
# Ok, after talking with Greasy we did the following: 

BPS_GP_ACRES <- Michi %>% #Creating new dataframe with just BPS_GP_NAME and ACRES
  group_by(BPS_GP_NAME) %>% 
  summarise(ACRES = sum(ACRES))
BPS_GP_ACRES

# Now graphing in desc. order
ggplot(BPS_GP_ACRES, aes(x = reorder(BPS_GP_NAME, ACRES), y = ACRES)) + 
  geom_bar(stat = "identity", fill = "grey") +
  coord_flip()
# This worked!


# Ok, let's take out Open Water and Barren-Rock/Sand/Clay
# First going to make GP_ACRES be in desc order
GP_ACRES_Arr <- arrange(BPS_GP_ACRES, desc(ACRES))

# Now, to only take the top 4 rows (IE not Open H20 or Barren)
GP_ACRES_CHOP <- GP_ACRES_Arr[c(1:4),]

# And graph with CHOP dataframe/add the slightly prettier stuff:

# BPS GP ACRES GRAPH

ggplot(GP_ACRES_CHOP, aes(x = reorder(BPS_GP_NAME, ACRES), y = ACRES)) + 
  geom_bar(stat = "identity", fill = "grey") +
  coord_flip() +

  ggtitle("Biophysical (Historical) Acres") +
  labs(x = "", y = "Acres") +
  scale_y_continuous(labels = comma) +
theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))

# Yay! It worked.


# Now the same with EVT.
# EVT 200 Group dataframe:
EVT_GP_ACRES <- Michi %>% #Creating new dataframe with just EVT_200_GP_NAME and ACRES
  group_by(EVT_200_GP_NAME) %>% 
  summarise(ACRES = sum(ACRES)) 
EVT_GP_ACRES
    
#now to arrange in desc order  
EVT_GP_Arr <- arrange(EVT_GP_ACRES, desc(ACRES))

# Ok, and a graph of all, to then be cut down
ggplot(EVT_GP_Arr, aes(x = reorder(EVT_200_GP_NAME, ACRES), y = ACRES)) + 
  geom_bar(stat = "identity", fill = "grey") +
  coord_flip() +
  
  ggtitle("EVT (Current) Acres") +
  labs(x = "", y = "Acres") +
  scale_y_continuous(labels = comma) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))


# Trim EVT to top 10
EVT_GP_ACRES_CHOP <- EVT_GP_Arr[c(1:10),]

# Graph again:
ggplot(EVT_GP_ACRES_CHOP, aes(x = reorder(EVT_200_GP_NAME, ACRES), y = ACRES)) + 
  geom_bar(stat = "identity", fill = "grey") +
  coord_flip() +
  
  ggtitle("EVT (Current) Acres") +
  labs(x = "", y = "Acres") +
  scale_y_continuous(labels = comma) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))





# GRAPHING EVT AND BPS NAME INSTEAD OF GROUP NAME
# Let's start with BPS
BPS_NAME <- Michi %>% #Creating new dataframe with just BPS_NAME and ACRES
  group_by(BPS_NAME) %>% 
  summarise(ACRES = sum(ACRES))
BPS_NAME

# And the descenidng order:
BPS_NAME_Desc <- arrange(BPS_NAME, desc(ACRES))
BPS_NAME_Desc

# Graph of all BPS Names in Descending Order:
ggplot(BPS_NAME_Desc, aes(x = reorder(BPS_NAME, ACRES), y = ACRES)) + 
  geom_bar(stat = "identity", fill = "grey") +
  coord_flip() +
  
  ggtitle("All BPS Names") +
  labs(x = "", y = "Acres") +
  scale_y_continuous(labels = comma) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))

# Top 11 (10 and 11 look equal so putting at 11 for now)
BPS_NAME_CHOP <- BPS_NAME_Desc[c(1:11),]

#First chopped, now graph again
ggplot(BPS_NAME_CHOP, aes(x = reorder(BPS_NAME, ACRES), y = ACRES)) + 
  geom_bar(stat = "identity", fill = "grey") +
  coord_flip() +
  
  ggtitle("Top 11 BPS Names") +
  labs(x = "", y = "Acres") +
  scale_y_continuous(labels = comma) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))

# EVT Same Thing - First Graph All EVT_NAMES in descenidng order:
EVT_200_NAME <- Michi %>% #Creating new dataframe with just EVT_200_NAME and ACRES
  group_by(EVT_200_NAME) %>% 
  summarise(ACRES = sum(ACRES))
EVT_200_NAME

# And the descenidng order:
EVT_200_NAME_Desc <- arrange(EVT_200_NAME, desc(ACRES))
EVT_200_NAME_Desc

# Ok, let's graph a bar graph with 75 bars (and then reduce)
ggplot(EVT_200_NAME_Desc, aes(x = reorder(EVT_200_NAME, ACRES), y = ACRES)) + 
  geom_bar(stat = "identity", fill = "grey") +
  coord_flip() +
  
  ggtitle("All EVT Names") +
  labs(x = "", y = "Acres") +
  scale_y_continuous(labels = comma) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))

# AHH! Too many to see! Let's narrow to 35 and see what we get. Might need to do more than that.
EVT_35 <- EVT_200_NAME_Desc[c(1:35),]

# And we graph again
ggplot(EVT_35, aes(x = reorder(EVT_200_NAME, ACRES), y = ACRES)) + 
  geom_bar(stat = "identity", fill = "grey") +
  coord_flip() +
  
  ggtitle("Top 35 EVT Names") +
  labs(x = "", y = "Acres") +
  scale_y_continuous(labels = comma) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))

# Let's make it top 12 (?) Seems somewhat arbitrary but at a dropoff point
EVT_NAME_CHOP <- EVT_35[c(1:12),]

# And another graph
ggplot(EVT_NAME_CHOP, aes(x = reorder(EVT_200_NAME, ACRES), y = ACRES)) + 
  geom_bar(stat = "identity", fill = "grey") +
  coord_flip() +
  
  ggtitle("Top 12 EVT Names") +
  labs(x = "", y = "Acres") +
  scale_y_continuous(labels = comma) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))

# Seems like it could be cut off before "Open Water" as well. Thoughts Greasy?








# Anna's code for reference - I couldn't get it to work this way so chopped things
# Which worked for me cause what I wanted to take out was in desc. order. 
#See below for what I attempted (at least one version of what I attempted)
mfri_acres <- select(filter(NE_mfri, LABEL != "NODATA", LABEL != "Indeterminate Fire Regime Characteristics", LABEL != "Water", LABEL != "Barren", LABEL != "Sparsely Vegetated"),
                     
                     c("VALUE", "ACRES", "LABEL", "R", "G", "B", "RED", "GREEN", "BLUE"))

arrange(mfri_acres, VALUE)

# Attempt to use Anna's code, got errorz
GP_ACRES %>%
  select(filter(GP_ACRES, BPS_GP_NAME !="Open Water", BPS_GP_NAME !="Barren-Rock/Sand/Clay"))



# Making % of Acres Columns for EVT and BPS
# Example from interwebz:
fruits = mutate(fruits, 
                weight_pct = weight / sum(weight),
                cost_pct = cost / sum(cost))

# Going to add percent column to various datasets:
GP_ACRES_CHOP = mutate(GP_ACRES_CHOP,
                           ChopPct = (ACRES / sum(ACRES)) *100)

GP_ACRES_Arr = mutate(GP_ACRES_Arr,
                      ArrPct = (ACRES / sum(ACRES)) *100)


# Why the percents aren't like the others is a mystery to me!
EVT_GP_Arr = mutate(EVT_GP_Arr,
                      Percent = (ACRES / sum(ACRES)) *100) 


# Graphing % for BPS GP Chop
ggplot(GP_ACRES_CHOP, aes(x = reorder(BPS_GP_NAME, ACRES), y = ChopPct)) + 
  geom_bar(stat = "identity", fill = "grey") +
  coord_flip() +
  
  ggtitle("BPS Acres of the Michigamme Highlands") +
  labs(x = "", y = "Acres (%)") +
  scale_y_continuous(labels = comma) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))


# For figuring out percentage incluced/excluded in report
BPS_NAME_Desc = mutate(BPS_NAME_Desc,
                       ChopPct = (ACRES / sum(ACRES)) *100)


