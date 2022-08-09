library(lubridate)
library(tidyverse)
library(readr)
library(readxl)

can_cities <- 
  read_csv("data_input/mcc_cities_canada.csv") %>% 
  pull(city)

data_source <- "data_input/canada_saskatchewan.csv"

# loading data
df <- read_csv(data_source)

unique(df$Region)
df2 <- 
  df %>% 
  select(date = 1,
         city = 2,
         cases = 9) %>% 
  filter(city %in% c("Saskatoon", "Regina")) %>% 
  mutate(country = "Canada") %>% 
  arrange(city, date)

write_csv(df2, "data_inter/canada_saskatchewan.csv")






