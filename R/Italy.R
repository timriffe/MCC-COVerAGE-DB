library(lubridate)
library(tidyverse)
library(readr)
library(readxl)
rm (list = ls())


# data from: https://github.com/pcm-dpc/COVID-19
dates <- seq(ymd('2020-02-24'),ymd('2022-08-09'), by = '1 day') %>% str_replace_all("-", "")

links <- paste0("https://raw.githubusercontent.com/pcm-dpc/COVID-19/master/dati-province/dpc-covid19-ita-province-", dates, ".csv")

temp <- 
  read_csv(links[i])

db <- tibble()

for(i in 1:length(links)){
  temp <- 
    read_csv(links[i]) %>% 
    select(date = 1,
           city = 6,
           cum_cases = 10)
  
  db <- 
    db %>% 
    bind_rows(temp)
}

write_csv(db, "data_input/italy.csv")


db <- read_csv("data_input/italy.csv")

cities <- 
  read_xlsx("data_input/location_info.xlsx",
            sheet = "cities_info") %>% 
  filter(countryname == "Italy") %>% 
  pull(cityname)

db2 <- 
  db %>% 
  filter(city %in% cities) %>% 
  mutate(date = ymd(str_sub(date, 1, 10))) %>% 
  arrange(city, date) %>% 
  group_by(city) %>% 
  mutate(cases = cum_cases - lag(cum_cases)) %>% 
  ungroup() %>% 
  drop_na() %>% 
  mutate(cases = ifelse(cases < 0, 0, cases),
         country = "Italy") %>% 
  select(-cum_cases)

write_csv(db2, "data_output/italy.csv")  

