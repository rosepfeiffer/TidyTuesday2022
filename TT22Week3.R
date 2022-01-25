#Library Packages
library(tidyverse)
library(tidytuesdayR)
library(maps)
library(extrafont)
library(patchwork)

loadfonts() #"Gloria Hallelujah available @ fonts.google.com

#Pull data
world_map <- map_data("world") 

tuesdata <- tidytuesdayR::tt_load('2022-01-18')
data <- tuesdata$chocolate %>% 
  select(-c(ref)) 

#Clean data
origin <- data %>% group_by(country_of_bean_origin) %>% 
  mutate(mean = mean(rating)) %>% 
  rename(region = country_of_bean_origin) %>% 
  select(region,mean) %>% 
  unique()
origin$region <- str_replace_all(origin$region,'U.S.A','USA')
origin <- left_join(origin,world_map, by = "region")

factory <- data %>% group_by(company_location) %>% 
  mutate(mean = mean(rating)) %>% 
  rename(region = company_location) %>% 
  select(region,mean) %>% 
  unique()
factory$region <- str_replace_all(factory$region,'U.S.A.','USA') 
factory <- left_join(factory,world_map, by = "region")


#Maps 
omap <- ggplot() +
  geom_polygon(world_map, 
               mapping = aes(long,lat, group = group),fill='#ffebd6')+
  geom_polygon(origin,mapping = aes(long,lat,fill=mean, group = group)) +
  scale_fill_gradientn(limits=c(2.5,4), colors=c("#d9666f","#c89492","#59372a")) +
  theme(text = element_text(family = "Gloria Hallelujah",
                            color = "#59372a"),
        axis.ticks = element_line(linetype = "blank"), 
        axis.text = element_text(colour = NA), 
        panel.background = element_rect(fill = "#89a7a7"),
        plot.background = element_rect(fill = "#ffebd6"),
        legend.background = element_rect(fill = "#ffebd6"),
        legend.title = element_text(hjust = 0.5),
        legend.position = "bottom",
        legend.text = element_text(size=10),
        panel.grid.major = element_line(linetype = "blank"),
        panel.grid.minor = element_line(linetype = "blank")) +
  labs(title = "Country of Bean Origin", x = NULL, 
       y = NULL, fill = "Average Rating \n Out of 4") 
omap 
fmap <- ggplot() +
  geom_polygon(world_map, 
               mapping = aes(long,lat, group = group),fill='#ffebd6')+
  geom_polygon(factory,mapping = aes(long,lat,fill=mean, group = group)) +
  scale_fill_gradientn(limits=c(2.5,4),colors=c("#d9666f","#c89492","#59372a")) +
  theme(text = element_text(family = "Gloria Hallelujah",
                            color = "#59372a"),
        axis.ticks = element_line(linetype = "blank"), 
        axis.text = element_text(colour = NA), 
        panel.background = element_rect(fill = "#89a7a7"),
        plot.background = element_rect(fill = "#ffebd6"),
        legend.background = element_rect(fill = "#ffebd6"),
        legend.title = element_text(hjust = 0.5),
        legend.position = "bottom",
        legend.text = element_text(size=10),
        panel.grid.major = element_line(linetype = "blank"),
        panel.grid.minor = element_line(linetype = "blank")) +
  labs(title = "Country of Bar Manuafacturer", x = NULL, 
       y = NULL, fill = "Average Rating \n Out of 4")



plot <- (omap + fmap) + plot_annotation(title = "Do Cocoa Producing Countries Make the Best Chocolate?",
 subtitle = "Chocolate Ratings by  Bean Origin and Manufacturer Location",
 caption = "@rppfeiffer | #TidyTuesday Week 3 2022 | Data:Flavors of Cacao",
 theme = theme(text = element_text(
              family = "Gloria Hallelujah",color = "#59372a"),
              rect = element_rect(fill="#ffebd6"))) +
  plot_layout(guides="collect") & theme(legend.position = "bottom")

#Add theme() to remove white lines from plot.                             
plot <- plot & theme(plot.background = element_rect(color=NA))

ggsave("TT22week3.png", plot = plot, height = 5, width = 7, units = "in")



