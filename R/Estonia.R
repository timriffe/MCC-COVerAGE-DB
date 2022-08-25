# Harju County has Tallin

EE_url <- "https://opendata.digilugu.ee/opendata_covid19_test_location.csv"
download.file(EE_url, destfile = "Data/EE_orig.csv")

library("tidyverse")
EE <- read_csv("Data/EE_orig.csv")
EE %>% 
  filter(Commune == "Tallin") %>% 
  mutate(city = "Tallinn",
         country = "Estonia") 
