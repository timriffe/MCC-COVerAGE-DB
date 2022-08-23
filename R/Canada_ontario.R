library(lubridate)
library(tidyverse)
library(readr)
library(readxl)

can_cities <- 
  read_csv("data_input/mcc_cities_canada.csv") %>% 
  mutate(city = case_when(city == "London Ontario" ~ "London", 
                          city == "Kitchener-Waterloo" ~ "Waterloo", 
                          TRUE ~ city)) %>% 
  pull(city)

data_source <- "data_input/canada_ontario.csv"

# case and death data
url <- "https://data.ontario.ca/dataset/f4112442-bdc8-45d2-be3c-12efae72fb27/resource/455fd63b-603d-4608-8216-7d8647f43350/download/conposcovidloc.csv"
download.file(url, destfile = data_source)
# loading data
df <- read_csv(data_source)
unique(df$Reporting_PHU_City) %>% sort()

df2 <- 
  df %>% 
  select(date = 2,
         city = Reporting_PHU_City) %>% 
  group_by(city, date) %>% 
  summarise(cases = n()) %>% 
  ungroup() %>% 
  filter(city %in% can_cities) %>% 
  arrange(city, date) %>% 
  mutate(country = "Canada")

unique(df2$city)
write_csv(df2, "data_inter/canada_ontario.csv")
