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
library(ggiraph)

#ACT Interactive


library(readxl)
act_populations_english <- read_excel("data/act_populations_english.xlsx")
View(act_populations_english)

act_disparities <-  act_populations_english %>% 
  pivot_longer(
    cols = 2:11,
    names_to = "year",
    values_to = "act"
  )



title_disp <- "Pronounced differences in readiness for college (**ACT**) across"
subtitle_disp <- "different populations: 2001 - 2022"
caption <- "Source: Average ACT Score for 2022, 2021, 2020, 2019, 2018, and Earlier Years"


act_inter <- act_disparities |> 
  na.omit() |> 
  filter(ethnicity %in% c('Native American','African American', 'Asian American',
                          'European', 'Latinx', 'Multiple')) %>%
  mutate(
    tooltip_label = glue::glue('Mean ACT in {year}<br>was {round(act,2)}')) %>% 
  ggplot(aes(x = year, y = act, group = ethnicity, color = ethnicity, linetype = ethnicity)) +
  geom_point_interactive(aes(tooltip = tooltip_label, data_id = ethnicity), size = 3.5) +
  geom_line_interactive(aes(data_id = ethnicity), linewidth = 2.5) +
  scale_y_continuous(breaks = seq(10,27)) +
  labs(title = title_disp,
       subtitle = subtitle_disp,
       caption = caption,
       y = element_blank(), 
       x = element_blank()) +
  scale_color_brewer(palette = "Dark2") +
  theme_minimal(
    base_size = 18,
    base_family = "Source Sans Pro") +
  theme(
    legend.title = element_blank(),
    legend.text = element_text(size = 10),
    plot.title = ggtext::element_markdown(family = "Merriweather", size = 18, margin = margin(b = 0.1, unit = 'cm')),
    plot.subtitle = element_text(family = "Merriweather", size = 18, margin = margin(b = 1, unit = 'cm')),
    plot.title.position = "plot",
    plot.caption = element_text(family = "Merriweather", size = 8, hjust = 0),
    panel.grid.minor.y = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor.x = element_blank(),
    axis.line = element_line(color = "black"),
    legend.position = "bottom")   


girafe(
  ggobj = act_inter,
  width_svg = 8,
  options = list(
    opts_hover(css = ''),
    opts_hover_inv(
      css = girafe_css(
        css = 'opacity:0.4;',
        line = 'opacity:0.075;'
      )
    ),
    opts_tooltip(
      css = htmltools::css(
        background = 'white',
        border = '2px solid black;',
        padding = '10px',
        font_weight = 600,
        font_size = '14p'))))



