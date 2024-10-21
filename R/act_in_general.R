#*************** ACT  ******************************************************
#*    
#* Using ACT data to document disparities in readiness for college
#*
#*Source: https://blog.prepscholar.com/average-act-score-by-year
#*Saving and retrieving data: two options
#*.  # Save an object to a file
#*       saveRDS(object, file = "my_data.rds")
#    #Restore the object 
#*     readRDS(file = "my_data.rds")
#*     
#*  # Saving on object in RData format
#*  save(data1, file = "data.RData")
#*  
#*. To load the data again
#*. load("data.RData")   
#*
#*Tips about saving and retrieving data
#*https://cran.r-project.org/web/packages/expss/vignettes/labels-support.html
#*****************************************************************************

rm(list = ls()) #Clear environment

library(tidyverse)
library(gt)
library(gtExtras)
library(gtsummary)
library(gtools)
library(gfonts)
library(janitor)
library(Hmisc)
library(expss)


# Importing excel files ----------------------------------------------------

library(readxl)
act_scores_1993_2022 <- read_excel("act-scores_1993_2022.xlsx")
View(act_scores_1993_2022)


      
title <- "El **covid-19** aceleró la disminución en la preparación para la universidad según"
subtitle <- "lo evaluado por la prueba de ingreso ACT: 1993-2022"
caption <- "Fuente: Average ACT Score for 2022, 2021, 2020, 2019, 2018, and Earlier Years"

act_scores_1993_2022 %>% 
  ggplot(aes(x = year, y = composite)) +
  geom_line(color = "green", linewidth = 1.5) +
  geom_point(color = "grey21", shape = 21, fill = "white") +
  scale_x_continuous(breaks = seq(1993, 2022, 7)) +
  scale_y_continuous(breaks = seq(19.8,22,0.1)) +
  labs(title = title,
       subtitle = subtitle,
       caption = caption,
       y = element_blank(), 
       x = element_blank()) +
  theme_minimal(
    base_size = 18,
    base_family = "Source Sans Pro 3") +
  theme(
    plot.title = ggtext::element_markdown(family = "Merriweather", 
                                          size = 14, margin = margin(b = 0.1, unit = 'cm')),
    plot.title.position = "plot",
    plot.subtitle = element_text(family = "Merriweather", size = 14,
                                 margin = margin(b = 1, unit = 'cm')),
    plot.caption = element_text(family = "Merriweather", size = 8, hjust = 0),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.grid.minor.x = element_blank(),
    axis.line = element_line(color = "black"))


#* ACT overall English version

title_eng <- "**COVID-19** acelerated the decline in readiness for college as appraised by"
subtitle_eng <- "ACT: 1993-2022"
source_eng <- "Source: Average ACT Score for 2022, 2021, 2020, 2019, 2018, and Earlier Years"

act_scores_1993_2022 %>% 
  ggplot(aes(x = year, y = composite)) +
  geom_line(color = "green", linewidth = 1) +
  geom_point(color = "gray31", shape = 21, fill = "white") +
  scale_x_continuous(breaks = seq(1993, 2022, 7)) +
  scale_y_continuous(breaks = seq(19.8,22,0.1)) +
  labs(title = title_eng,
       subtitle = subtitle_eng,
       caption = source_eng ,
       y = element_blank(), 
       x = element_blank()) +
  theme_minimal(
    base_size = 18,
    base_family = "Source Sans Pro 3") +
  theme(
    plot.title = ggtext::element_markdown(family = "Merriweather", 
                                          size = 14, margin = margin(b = 0.1, unit = 'cm')),
    plot.title.position = "plot",
    plot.subtitle = element_text(family = "Merriweather", size = 14,
                                 margin = margin(b = 1.0, unit = 'cm')),
    plot.caption = element_text(family = "Merriweather", size = 8, hjust = 0),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.grid.minor.x = element_blank(),
    axis.line = element_line(color = "black"))



library(readxl)
act_ethnic_english <- read_excel("act_ethnic_english.xlsx")
View(act_ethnic_english)

act_disparities <-  act_ethnic_english %>% 
  pivot_longer(
    cols = 2:11,
    names_to = "year",
    values_to = "act"
  )



title_disp <- "Pronounced differences in readiness for college (ACT) across different "
subtitle_disp <- "populations: 2001 - 2022"
caption <- "Source: Average ACT Score for 2022, 2021, 2020, 2019, 2018, and Earlier Years"


act_disparities %>% 
  na.omit() %>% 
  filter(ethnicity %in% c('Native American','African American', 'Asian American',
                      'White', 'Latinx', 'Multiple')) %>%
  ggplot(aes(x = year, y = act, group = ethnicity, color = ethnicity, linetype = ethnicity)) +
  geom_point() +
  geom_line(linewidth = 1) +
  scale_y_continuous(breaks = seq(16,27)) +
  labs(title = title_disp,
       subtitle = subtitle_disp,
       caption = caption,
       y = element_blank(), 
       x = element_blank()) +
  scale_color_brewer(palette = "Dark2") +
  theme_minimal(
    base_size = 18,
    base_family = "Source Sans Pro 3") +
  theme(
    legend.title = element_blank(),
    plot.title = ggtext::element_markdown(family = "Merriweather", size = 14),
    plot.subtitle = element_text(family = "Merriweather", size = 14),
    plot.title.position = "plot",
    plot.caption = element_text(family = "Merriweather", size = 8, hjust = 0),
    panel.grid.minor.y = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor.x = element_blank(),
    axis.line = element_line(color = "black"),
    legend.position = "bottom") 

#* Spanish version

library(readxl)
disparities_ACT <- read_excel("disparities_ACT.xlsx")
View(disparities_ACT )


act_disparities <-  disparities_ACT  %>% 
  pivot_longer(
    cols = 2:11,
    names_to = "year",
    values_to = "act"
  )






act_disparities %>% 
  na.omit() %>% 
  filter(etnia %in% c('Aborigen','Africano', 'Asiatico',
                      'Europeo', 'Latinoamericano','Multiple')) %>%
  ggplot(aes(x = year, y = act, group = etnia, color = etnia, linetype = etnia)) +
  geom_point() +
  geom_line(linewidth = 1) +
  scale_y_continuous(breaks = seq(16,27)) +
  labs(title = title_disp,
       subtitle = subtitle_disp,
       caption = caption,
       y = element_blank(), 
       x = element_blank()) +
  scale_color_brewer(palette = "Dark2") +
  theme_minimal() +
  theme(
    legend.title = element_blank(),
    plot.title = ggtext::element_markdown(family = "Merriweather", size = 14),
    plot.subtitle = element_text(family = "Merriweather", size = 14),
    plot.title.position = "plot",
    plot.caption = element_text(family = "Merriweather", size = 8, hjust = 0),
    panel.grid.minor.y = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor.x = element_blank(),
    axis.line = element_line(color = "black"),
    legend.position = "bottom") 


# Wanting and enrolling in college ----------------------------------------


library(readxl)
planning_to_attend <- read_excel("planning-to_attend.xlsx") %>% 
  clean_names()

View(planning_to_attend)

wanting <-  planning_to_attend %>% 
  pivot_longer(
    cols = 3:4,
    names_to = "matricularse",
    values_to = "porcentage"
  )

caption <-  "Fuente: Carnevale et al., (2019), p. 12"

  
  
wanting %>% 
  ggplot(aes(x = fct_reorder(ingreso_familiar,order), y = porcentage, fill = matricularse)) +
  geom_col(position = "dodge") +
  geom_text(aes(y = porcentage, label = porcentage),
            vjust = 0.08, 
            color = "black", position = position_dodge(0.9)) +
  labs(x = element_blank(),
       fill = "Matricularse:",
       caption = caption) + 
  theme_bw() +
  theme(
    panel.grid.minor.y = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_blank(),
    axis.line = element_line(color = "black"),
    legend.position = "top")
  




