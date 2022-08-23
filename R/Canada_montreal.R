library(lubridate)
library(tidyverse)
library(readr)
library(readxl)

# manually downloaded from inspq:
# https://www.inspq.qc.ca/covid-19/donnees
data_source <- "data_input/canada_montreal.csv"

# loading data
df <- read_csv(data_source)

df2 <- 
  df %>% 
  select(date = 1,
         cum_cases = 2) %>% 
  mutate(date = ymd(date),
         cases = cum_cases - lag(cum_cases), 
         city = "Montreal",
         country = "Canada") %>% 
  select(country, city, cases)

write_csv(df2, "data_inter/canada_montreal.csv")






