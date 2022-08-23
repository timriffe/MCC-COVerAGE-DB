library(lubridate)
library(tidyverse)
library(readr)
library(readxl)

# manually downloaded from inspq:
# https://www.inspq.qc.ca/covid-19/donnees
data_source <- "data_input/canada_manitoba.csv"

# loading data
df <- read_csv(data_source)
unique(df$rha)


df2 <- 
  df %>% 
  select(date = 3,
         city = 4,
         cases = 6) %>% 
  filter(city == "Winnipeg RHA") %>%
  mutate(city = "Winnipeg",
         country = "Canada")
  
write_csv(df2, "data_inter/canada_manitoba.csv")






