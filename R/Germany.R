library(lubridate)
library(tidyverse)
library(readr)
library(readxl)
rm (list = ls())

cities <- 
  read_xlsx("data_input/location_info.xlsx",
            sheet = "cities_info") %>% 
  filter(countryname == "Germany") %>% 
  pull(cityname)

# cases_url <- "https://www.arcgis.com/sharing/rest/content/items/f10774f1c63e40168479a1feb6c7ca74/data"
data_source <- "data_input/germany.csv"
# download.file(cases_url, destfile = data_source, mode = "wb")

db <- 
  read_csv(data_source,
           locale = locale(encoding = "UTF-8"))

unique(db$Landkreis) %>% sort()
unique(db$Bundesland) %>% sort()

db2 <- 
  db %>% 
  filter(str_detect(Landkreis, "SK ")) %>% 
  mutate(city = str_replace(Landkreis, "SK ", ""),
         date = ymd(str_sub(Refdatum, 1, 10)),
         cases = ifelse(AnzahlFall < 0, 0, AnzahlFall),
         city = case_when(str_detect(city, "Berlin") ~ "Berlin",
                          city == "Frankfurt am Main" ~ "Frankfurt",
                          city == "Düsseldorf" ~ "Duesseldorf",
                          city == "Köln" ~ "Koeln",
                          city == "München" ~ "Muenchen",
                          TRUE ~ city)) %>% 
  group_by(city, date) %>% 
  summarise(cases = sum(cases)) %>% 
  ungroup() %>% 
  filter(city %in% cities) %>% 
  mutate(country = "Germany")
  
write_csv(db2, "data_output/germany.csv")  
