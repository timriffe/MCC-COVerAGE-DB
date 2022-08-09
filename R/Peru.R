library(lubridate)
library(tidyverse)
library(readr)
library(readxl)
library(xml2)
library(rvest)
rm (list = ls())

cities <- 
  read_xlsx("data_input/location_info.xlsx",
            sheet = "cities_info") %>% 
  filter(countryname == "Peru") %>% 
  pull(cityname)


# data from: https://www.datosabiertos.gob.pe/dataset/casos-positivos-por-covid-19-ministerio-de-salud-minsa
df <- read.csv("data_input/peru.csv", sep = ";")

unique(df$DEPARTAMENTO) %>% sort()
unique(df$PROVINCIA)
unique(df$DISTRITO)

df2 <- 
  df %>% 
  filter(DEPARTAMENTO %in% cities) %>% 
  select(city = DEPARTAMENTO,
         date = 8) %>% 
  mutate(city = str_to_title(city),
         date = ymd(date)) %>% 
  group_by(city, date) %>% 
  summarise(cases = n()) %>% 
  ungroup() %>% 
  complete(date, city, fill = list(cases = 0)) %>% 
  mutate(country = "Peru") %>% 
  arrange(city, date)
  
write_csv(df2, "data_output/peru.csv")  
