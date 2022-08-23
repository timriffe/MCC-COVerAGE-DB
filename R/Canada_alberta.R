library(lubridate)
library(tidyverse)
library(readr)
library(readxl)

can_cities <- 
  read_csv("data_input/mcc_cities_canada.csv") %>% 
  pull(city)

data_source <- "data_input/canada_alberta.csv"

# case and death data
url <- "https://www.alberta.ca/data/stats/covid-19-alberta-statistics-data.csv"
download.file(url, destfile = data_source)
# loading data
df <- read_csv(data_source)

df2 <- 
  df %>% 
  select(date = 2,
         city = 3) %>% 
  mutate(city = str_replace(city, " Zone", "")) %>% 
  filter(city %in% can_cities) %>% 
  group_by(city, date) %>% 
  summarise(cases = n()) %>% 
  ungroup() %>% 
  mutate(country = "Canada")

write_csv(df2, "data_inter/canada_alberta.csv")

  

  


