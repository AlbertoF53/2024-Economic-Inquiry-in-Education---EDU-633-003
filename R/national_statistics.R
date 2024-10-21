#*************** Enrollment in postsecondary education *********************
#*    
#*Digest of Education Statistics
#*Current
#*
#*https://nces.ed.gov/programs/digest/current_tables.asp
#*
#*Table 303.10 Retrieved on October 10, 2024
#*Total fall enrollment in degree-granting postsecondary institutions, 
#*by attendance status, sex of student, and control of institution: 
#*Selected years, 1947 through 2031
#*The table was prepared on December 2023.
#*
#* For good tips about value labels and value labels see:
#* https://cran.r-project.org/web/packages/expss/vignettes/labels-support.html
#*
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
#*Tips about saving and retreiving data
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


# Importing Stata file ----------------------------------------------------

library(tidyverse)
library(janitor)
library(ggtext)
library(haven)
library(plotly)
library(haven)

Enrollment_PSE_1947_2023 <- read_dta("Documents/2024 Economic Inquiry in Education - EDU-633-003/Enrollment_PSE_1947-2023.dta") %>% 
  as_tibble()


title_color_eng <- "In 2020, <span style = 'color:red;'>**COVID-19**</span> accelerated 
the decline in </br>postsecondary enrollment"

subtitle_eng <- "1973 - 2023 (enrollment in thousands)"

caption_eng <- "Based on table 303.10 of ***2023 Digest of Education Statistics*** - October 16, 2024"



table302_1_hs_enrollment  |> 
  filter(as.numeric(enr_th) <= "2010") %>% 
  ggplot(aes(x = year, y = enr_th)) +
  geom_line(color = "blue", linewidth = 2) +
  geom_point(shape = 21, size = 3, alpha = 0.85, 
             stroke = 1.1, fill = "white") +
  #geom_point(color = "white", shape = 21, fill = "grey30") +
  scale_x_continuous(breaks = seq(1973, 2023, 5)) +
  scale_y_continuous(breaks = seq(10000, 21000, 1000)) +
  labs(title = title_color_eng ,
       subtitle = subtitle_eng,
       caption = caption_eng,
       y = element_blank(), 
       x = element_blank() ) +
  geom_text(x = 2017,
            y = 18500,
            label = "COVID-19",
            color = "red",
            size = 3.1,
            show.legend = FALSE) +
 geom_vline(xintercept = 2021, linetype = "dashed",
             color = "red", linewidth = 1)  +
  theme_minimal(
    base_size = 18,
    base_family = "Source Sans Pro 3") +
  theme(
    plot.title.position = 'plot',
    plot.title = element_markdown(family = "Merriweather", size = 16,
                                  margin = margin(b = 0.2, unit = 'cm')),
    plot.subtitle = element_text(family = "Merriweather", size = 14,
                                 margin = margin(b = 0.4, unit = 'cm')),
    plot.caption = element_markdown(family = "Merriweather", size = 7),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.grid.minor.x = element_blank())





# Percentage of HS graduates enrolling in PSE ---------------------

library(gt)
library(gtExtras)

library(readxl)
table302_1_hs_enrollment <- read_excel("~/Documents/2024 Economic Inquiry in Education - EDU-633-003/tabn302.1_hs_enrollment.xlsx") |> 
  as_tibble()


table302_1_hs_enrollment <-  table302_1_hs_enrollment |> 
  rename(All = per_tot) |> 
  rename('Two year' = per_2yr) |> 
  rename('Four year' = per_4yr)
  
#* Setting up color Okabe_Ito

thematic::okabe_ito(3)

# [1] "#E69F00" "#009E73" "#0072B2"

colors_ito <- c('#E69F00', '#009E73', "#0072B2")


names(colors_ito) <- c('All', 'Four year', 'Two year')




title <- glue::glue(
  'Percent of HS graduates enrolling in ',
  '<span style="color:{colors_ito["Four year"]};">**4-year**</span> ',
  'and ',
  '<span style="color:{colors_ito["Two year"]};">**2-year**</span> ',
  'colleges')

subtitle <-  "(1973 - 2022)"
caption <- "Based on table 302.10 Digest of Education Statistics"


table302_1_hs_enrollment |> 
  pivot_longer(cols = 2:4, 
               names_to = "institution",
               values_to = "percent") |> 
  mutate(year = as.numeric(year)) |> 
  ggplot(aes(x = year, y = percent, color = institution)) +
  geom_point() +
  geom_line() +
  geom_vline(xintercept = 2020, linetype = "dashed",
             color = '#E69F00', linewidth = 0.5)  +
  scale_x_continuous(breaks = seq(1973, 2023, 5)) +
  labs(title = title,
       subtitle = subtitle,
       caption = caption,
       x = element_blank(),
       y = element_blank(),
       color = " ") +
  geom_text(x = 2017.5,
            y = 50,
            label = "COVID-19",
            color = '#E69F00',
            size = 3.1) +
  geom_text(x = 1990,
            y = 65,
            label = "Overall",
            color = "red",
            size = 3.5) +
  theme_minimal(
    base_size = 12,
    base_family = "Source Sans Pro") +
  theme(
    legend.position = 'none',
    scale_color_manual(values = colors_ito),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(color = 'grey50'),
    plot.title = ggtext::element_markdown(
      family = 'Merriweather',
      size = rel(1.3),
      margin = margin(b = 0.3, unit = 'cm')
    ),
    plot.subtitle = ggtext::element_markdown(
      family = 'Merriweather',
      size = rel(1.1),
      margin = margin(b = 0.3, unit = 'cm')
    ),
    plot.caption = element_text(size = rel(0.6)) )



