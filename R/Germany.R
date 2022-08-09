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

db2 <- db %>% 
  mutate(Sex = case_when(Geschlecht == "M" ~ "m",
                         Geschlecht == "W" ~ "f",
                         Geschlecht == "unbekannt" ~ "UNK"),
         Age = case_when(Altersgruppe == "A00-A04" ~ "0",
                         Altersgruppe == "A05-A14" ~ "5",
                         Altersgruppe == "A15-A34" ~ "15",
                         Altersgruppe == "A35-A59" ~ "35",
                         Altersgruppe == "A60-A79" ~ "60",
                         Altersgruppe == "A80+" ~ "80",
                         Altersgruppe == "unbekannt" ~ "UNK"),
         date_f = ymd(str_sub(Meldedatum, 1, 10)),
         Cases = ifelse(AnzahlFall < 0, 0, AnzahlFall),
         Deaths = ifelse(AnzahlTodesfall < 0, 0, AnzahlTodesfall),
         Region = Bundesland) %>% 
  select(date_f, Sex, Age, Cases, Deaths, Region) %>% 
  pivot_longer(Cases:Deaths, names_to = "Measure", values_to ="Value") %>% 
  group_by(Region, Sex, Measure, date_f, Age) %>% 
  summarize(Value = sum(Value)) %>% 
  ungroup()


unique(db2$Region)
