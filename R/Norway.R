# library(lubridate)
# library(tidyverse)
# library(readr)
# library(readxl)
# rm (list = ls())
# 
# # db <- read_csv("https://raw.githubusercontent.com/folkehelseinstituttet/surveillance_data/master/covid19/data_covid19_msis_by_time_location_latest.csv")
# # write_csv(db, "data_input/norway.csv")
# 
# db <- read_csv("data_input/norway.csv")
# 
# db2 <- 
#   db %>% 
#   filter(location_name == "Oslo") %>% 
#   select(city = location_name, date, cases = n) %>% 
#   mutate(country = "Norway")

# TR redux
NO_url <- "https://raw.githubusercontent.com/thohan88/covid19-nor-data/master/data/01_infected/msis/municipality.csv"

download.file(NO_url, destfile = "Data/NO_orig.csv")
NO <- read_csv("Data/NO_orig.csv") 


all_dates <- seq(min(NO$date),max(NO$date),by="days")
NO %>% 
  filter(kommune_name == "Oslo") %>% 
  mutate(width = as.integer(lead(date) - date),
         cases = lead(cases)-cases) %>% 
  complete(date = all_dates, kommune_name) %>% 
  arrange(date) %>% 
  fill(width, cases) %>% 
  mutate(cases = cases / width,
         country = "Norway") %>% 
  select(city = kommune_name,
         date,
         cases,
         country) %>% 
  write_csv("data_output/norway.csv")
