# Harju County has Tallin

EE_url <- "https://opendata.digilugu.ee/opendata_covid19_test_county_all.csv"
download.file(EE_url, destfile = "Data/EE_orig.csv")

library(tidyverse)
EE <- read_csv("Data/EE_orig.csv")
all_dates <- seq(min(EE$StatisticsDate),max(EE$StatisticsDate),by="days")
EE %>% 
  filter(County == "Harju maakond",
         ResultValue == "P") %>% 
  mutate(city = "Tallinn",
         country = "Estonia") %>% 
  arrange(StatisticsDate) %>% 
    rename(date = StatisticsDate) %>% 
  select(date,city,country,DailyCases) %>% 
  complete(date = all_dates, 
           city, 
           country,
           fill = list(DailyCases = 0)) %>% 
  write_csv("data_output/estonia.csv")

