library(lubridate)
library(tidyverse)
library(readr)
library(readxl)
library(data.table)

can_cities <- 
  read_csv("data_input/mcc_cities_canada.csv") %>% 
  pull(city)

data_source <- "data_input/Canada_bc_in.csv"

url <- "http://www.bccdc.ca/Health-Info-Site/Documents/BCCDC_COVID19_Dashboard_Case_Details.csv"

download.file(url, destfile = data_source)
df <- fread(data_source)

df2 <- 
  df %>% 
  select(date = 1,
         region = 3,
         city = 4,
         cases = 5) %>% 
  filter(city %in% can_cities) %>% 
  group_by(city, date) %>% 
  summarise(cases = sum(cases)) %>% 
  ungroup() %>% 
  mutate(country = "Canada")

unique(df2$city)

write_csv(df2, "data_inter/canada_bc.csv")
