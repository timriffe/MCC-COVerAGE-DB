library(lubridate)
library(tidyverse)
library(readr)
library(readxl)
rm (list = ls())

# db <- read_csv("https://raw.githubusercontent.com/folkehelseinstituttet/surveillance_data/master/covid19/data_covid19_msis_by_time_location_latest.csv")
# write_csv(db, "data_input/norway.csv")

db <- read_csv("data_input/norway.csv")

db2 <- 
  db %>% 
  filter(location_name == "Oslo") %>% 
  select(city = location_name, date, cases = n) %>% 
  mutate(country = "Norway")
