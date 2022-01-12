#Library packages
library(ggplot2)
library(tidytuesdayR)
library(ggbeeswarm)
library(ggpomological)
library(showtext)

#Font .ttf downloaded from https://www.fontspace.com/b-bee-pollen-font-f62480 
font_add("b Bee Pollen","BeePollen-lgE8Z.otf")
showtext_auto()

#Pull in data set for TidyTuesday
tuesdata <- tidytuesdayR::tt_load('2022-01-11')

colony <- tuesdata$colony
colony$year <- factor(colony$year, levels = c(2015:2021))
colony$months<- factor(colony$months,levels = c("January-March","April-June",
                                                "July-September",
                                                "October-December"))
#Define color pallete
pallete <- c("#BD4932", "#C98A1C","#105B63", "#FFD34E")

#Build Plot
plot <- ggplot(colony,aes(x = year, y = colony_lost_pct, color = months)) + 
  geom_quasirandom(method = "pseudorandom",size = 2, pch = 8) +
  labs(title = "Bee Colony Losses in the US",
       caption = "@rppfeiffer | #TidyTuesday 2022 Week 2 | Data from USDA",
       x = NULL, y = NULL, colour = NULL) 

#Make it pretty 
plot <- plot +theme_pomological() +
  theme(text = element_text(family = "b Bee Pollen"),
        plot.caption = element_text(colour = "#a89985",size = 10),
        plot.title = element_text(colour = "#704d10", size = 15),
        legend.text = element_text(colour = "#704d10", size = 9.5),
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 9.5),
        legend.position = "bottom",
        legend.direction = "horizontal") +
  scale_y_continuous(labels = function(value) paste0(value,"%"))+
  scale_color_manual(values=pallete) +
  coord_flip()
plot 

ggsave("TTweek2.png", plot = plot, height = 5, width = 7, units = "in")

