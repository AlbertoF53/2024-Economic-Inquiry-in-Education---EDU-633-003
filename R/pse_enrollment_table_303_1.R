#*************** Enrollment in postsecondary education *********************
#*    
#*Digest of Education Statistics
#*Current
#*
#*https://nces.ed.gov/programs/digest/current_tables.asp
#*
#*Table 303.10 Retrieved on September 11, 2023
#*Total fall enrollment in degree-granting postsecondary institutions, 
#*by attendance status, sex of student, and control of institution: 
#*Selected years, 1947 through 2031
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


# Importing excel file ----------------------------------------------------


library(readxl)
tabn303_10 <- read_excel("tabn303.10.xlsx")
glimpse(tabn303_10)


# Cleaning the national database ------------------------------------------

tabn303_10 <- tabn303_10 %>% 
  clean_names()

names(tabn303_10)


# Assigning labels to the variables ---------------------------------------

library(expss)

tabn303_10 = apply_labels(tabn303_10,
                      tot_enrollment = "Enrollment",
                      ftime = "Full time",
                      ptime = "Part time",
                      ppart_t = "Percent part time",
                      male = "Male",
                      female = "Female",
                      pfemale = "Percent female",
                      public = "Public universities",
                      tot_pr = "Private universities",
                      nonpro = "Nonprofit",
                      for_prof = "For-profit")

glimpse(tabn303_10)

# Declaring year as factor and variables as numeric -----------------------

tabn303_10 <- tabn303_10 %>% 
  mutate(year = as.factor(year)) %>% 
  mutate_at(c("tot_enrollment", "tot_pr","ftime","ptime",
              "ppart_t","nonpro","for_prof", "male", "female",
              "pfemale", "public"), as.numeric) %>% 
  mutate(enr_th = tot_enrollment/1000)

library(expss)

tabn303_10 = apply_labels(tabn303_10,
                          enr_th = "Enrollment in thousands")

rm(tabn303_10)



glimpse(tabn303_10)

str(tabn303_10)

tabn303_10 %>% 
  summary()


# Displaying the variable label -------------------------------------------


var_lab(tabn303_10$ftime)


# Saving data in rds format -----------------------------------------------

saveRDS(tabn303_10, file = "tabn303_10.rds")

#Restoring data in rds


tabn303_10 <-  readRDS(file = "tabn303_10.rds")

tabn303_10 %>% 
  select(ftime) %>% 
  summary(ftime)


# Saving & retrieving data in RData format ---------------------------------------------


# Saving on object in RData format

save(tabn303_10, file = "data.RData")

# Retrieving RData

load("tabn303_10.RData")


tabn303_10 %>% 
  filter(year < 2024) %>% 
  ggplot(aes(x = year, y = enr_th)) +
  geom_line() +
  geom_point()



# Importing Stata file ----------------------------------------------------

library(tidyverse)
library(janitor)
library(ggtext)
library(haven)


Enrollment_PSE_1947_2023 <- read_dta("Enrollment_PSE_1947-2023.dta") %>% 
  clean_names()

library(haven)

Enrollment_PSE_1947_2023 <- read_dta("Enrollment_PSE_1947-2023.dta")

title_color <- "En 2020, <span style = 'color:red;'>**COVID-19**</span> aceleró
la caída de la matrícula en la educación superior"

subtitle <- "1973 - 2023 (matrícula en millares)"

caption <- "Basado en la tabla 303.10 del ***Compendio de Estadísticas Educativas*** - 16 de octubre de 2023"

Enrollment_PSE_1947_2023  %>% 
  filter(as.numeric(enr_th) <= "2010") %>% 
  ggplot(aes(x = year, y = enr_th)) +
  geom_line(color = "blue", linewidth = 2) +
  geom_point(shape = 21, size = 3, alpha = 0.85, fill = "white", stroke = 1.1) +
  scale_x_continuous(breaks = seq(1973, 2023, 5)) +
  scale_y_continuous(breaks = seq(10000, 21000, 1000)) +
  labs(title = title_color,
       subtitle = subtitle,
       caption = caption,
       y = element_blank(), 
       x = element_blank() ) +
  geom_text(x = 2017,
            y = 18500,
            label = "COVID-19",
            color = "red",
            size = 3.1,
            show.legend = FALSE) +
  geom_vline(xintercept = 2020, linetype = "dashed",
             color = "red", size = 1) +
  theme_minimal(
    base_size = 18,
    base_family = "Source Sans Pro 3") +
  theme(
    plot.title = element_markdown(family = "Merriweather", size = 15.5,
                                  margin = margin(b = 0.6, unit = 'cm')),
    plot.subtitle = element_text(family = "Merriweather", size = 14,
                                 margin = margin(b = 1, unit = 'cm')),
    plot.caption = element_markdown(family = "Merriweather", size = 8),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.grid.minor.x = element_blank(),
    axis.line = element_line(color = "black"))




#English version

title_color_eng <- "In 2020, <span style = 'color:red;'>**COVID-19**</span> accelerated 
the decline in postsecondary enrollment"

subtitle_eng <- "1973 - 2023 (enrollment in thousands)"

caption_eng <- "Based on table 303.10 of ***2023 Education Digest*** - October 16, 2023"



Enrollment_PSE_1947_2023  %>% 
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
  geom_vline(xintercept = 2020, linetype = "dashed",
             color = "red", size = 1) +
  theme_minimal(
    base_size = 18,
    base_family = "Source Sans Pro 3") +
  theme(
    plot.title = element_markdown(family = "Merriweather", size = 16,
                                  margin = margin(b = 0.6, unit = 'cm')),
    plot.subtitle = element_text(family = "Merriweather", size = 14,
                                 margin = margin(b = 1, unit = 'cm')),
    plot.caption = element_markdown(family = "Merriweather", size = 8),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.grid.minor.x = element_blank()) 




# Percent of high school completers enrolled in postsecondary educ --------


library(readxl)
tabn302_1_hs_enrollment <- read_excel("tabn302.1_hs_enrollment.xlsx")
View(tabn302_1_hs_enrollment)






  