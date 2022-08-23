library(lubridate)
library(tidyverse)
library(readr)
library(readxl)
rm (list = ls())

db <- read_csv("data_input/czechia.csv")
# code for Prague: CZ010
db2 <- 
  db %>% 
  filter(kraj_nuts_kod == "CZ010") %>% 
  select(date = datum) %>% 
  group_by(date) %>% 
  summarise(cases = n()) %>% 
  ungroup() %>% 
  mutate(city = "Prague",
         country = "Czech Republic")

write_csv(db2, "data_output/czechia.csv")

