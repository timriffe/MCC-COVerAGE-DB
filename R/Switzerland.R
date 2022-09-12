library(lubridate)
library(tidyverse)
library(readr)
library(readxl)
rm (list = ls())

cities <- 
  read_xlsx("data_input/location_info.xlsx",
            sheet = "cities_info") %>% 
  filter(countryname == "Switzerland") %>% 
  pull(cityname)

city_codes <- read_csv("data_input/switzerland/city_codes.csv",
                       locale = locale(encoding = "UTF-8"))

# data from: https://www.covid19.admin.ch/en/overview
db <- read_csv("data_input/switzerland.csv")

db2 <- 
  db %>% 
  select(city_code = 1,
         date = 2,
         cases = 3) %>% 
  filter(city_code %in% c("BS", "BE", "GE", "LU", "SG", "TI", "VD", "ZH")) %>% 
  mutate(city = case_when(city_code == "BS" ~ "Basel-Stadt", 
                          city_code == "BE" ~ "Bern", 
                          city_code == "GE" ~ "Geneva", 
                          city_code == "LU" ~ "Lucerne", 
                          city_code == "SG" ~ "Sankt Gallen", 
                          city_code == "TI" ~ "Ticino", 
                          city_code == "VD" ~ "Vaud", 
                          city_code == "ZH" ~ "Zurich"),
         country = "Switzerland") %>% 
  select(-city_code)

write_csv(db2, "data_output/switzerland.csv")
