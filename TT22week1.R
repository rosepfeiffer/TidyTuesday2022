library(tidyverse) 
library(janitor)
library(zoo)
library(readxl)
library(patchwork)
library(artyfarty)

#Data Cleaning
df <- read_excel("data/YUEData11-21.xlsx")

df <- df %>% janitor::clean_names() %>% 
  subset(select = c(series_id,jan_2016:dec_2021)) %>% 
  pivot_longer(jan_2016:dec_2021, names_to="month") %>% 
  na.omit()

df$month <- df$month %>% str_to_title() %>% 
  as.yearmon("%b_%Y") 

df$series_id <- df$series_id%>% 
  str_replace("LNU04000048","Adults (25+)") %>% 
  str_replace("LNU04024887","Youth (16-24)") %>% 
  str_replace("LNU04024893","White") %>% 
  str_replace("LNU04024931","Black" ) %>% 
  str_replace("LNU04069594","Asian") %>% 
  str_replace("LNU04069595","Latinx")           

#Establish data frames for 2 plots
race <- c("White","Black","Asian","Latinx")
comp <- c("Youth (16-24)","Adults (25+)")
racedf <- df %>% filter(series_id %in% race)
compdf <- df %>% filter(series_id %in% comp)

youth <- df$value[]
#Build plots
raceplot <- ggplot(racedf, aes(x = month, y = value, color = series_id)) +
  geom_line(size = 0.8) +
  scale_y_continuous(limits = c(0,40),
                     labels = function(value) paste0(value,"%")) + 
  scale_x_yearmon(format ="%Y", n = 5) +
  labs(title = "Youth Unemployment by Race/Ethnicity", 
       x = NULL, y = NULL, color = NULL) +
  theme_monokai_full() +
  scale_color_manual(values = c("#f92672","#66d9ef","#f7e543","#7fb80d")) +
  theme(plot.title = element_text(size = 11, hjust = 0)) 

compplot <- ggplot(compdf, aes(x = month, y = value, color = series_id)) +
  geom_line(size = 0.8) +
  scale_y_continuous(limits = c(0,40),
                     labels = function(value) paste0(value,"%")) + 
  scale_x_yearmon(format ="%Y", n = 5) +
  labs(title = "Youth vs. Adult Unemployment", 
       x = NULL, y = NULL, color = NULL) +
  theme_monokai_full() + 
  scale_color_manual(values=c("#fd971f","#ae81ff")) +
  theme(plot.title = element_text(size = 11,hjust = 0))

# Use patchwork to stack the graphs together
patch <- compplot / raceplot

# Add title & captions to plot and fix background theme
final <- patch + plot_annotation(
  title = "Youth Unemployment in the US 2016-2021",
  subtitle = "*Unadjusted rates",
  caption = "@rppfeiffer | #TidyTuesday 2022 Week 1 | Data from bls.gov", 
  theme = theme(plot.background = element_rect(fill = "#272822"),
        plot.title = element_text(color = "#f8f8f2", face = "bold"),
        plot.subtitle =  element_text(color = "#f8f8f2", vjust = 0.53),
        plot.caption = element_text(color = "#f8f8f2"))) +
  plot_layout(guides = "collect") 

# save the image
ggsave("TT22W1.png", plot = final, height = 5, width = 7, units = "in")



