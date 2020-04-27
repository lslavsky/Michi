
library(ggplot2)
library(readr)
library(hrbrthemes)


MichiTreeEVC <- read_csv("MichiTreeEVC.csv")
View(MichiTreeEVC)

# simple of just cover for all tree categories
ggplot(MichiTreeEVC, aes(x=class, y=percent)) +
  geom_area( fill="#69b3a2", alpha=0.4) +
  geom_line(color="#69b3a2", size=2) +
  geom_point(size=3, color="#69b3a2") +
  theme_ipsum() +
  ggtitle("Evolution of something")

# with top 5 EVT groups

MichiEVC5 <- read.csv("MichiTreeEVCtype.csv")
View(MichiEVC5)

ggplot(MichiEVC5, aes(x=class,y=percent)) +
  geom_bar(stat="identity") +
  facet_grid(rows = vars(type))+
  theme(legend.position = "none") +
  theme_ipsum(  )




