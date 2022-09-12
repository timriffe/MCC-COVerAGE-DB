library(lubridate)
library(tidyverse)
library(readr)
library(readxl)
rm (list = ls())

cities <- 
  read_xlsx("data_input/location_info.xlsx",
            sheet = "cities_info") %>% 
  filter(countryname == "Mexico") %>% 
  pull(cityname)

# cases_url <- "https://www.arcgis.com/sharing/rest/content/items/f10774f1c63e40168479a1feb6c7ca74/data"
data_source <- "data_input/mexico.csv"
# download.file(cases_url, destfile = data_source, mode = "wb")

df <- 
  read_csv(data_source,
           locale = locale(encoding = "UTF-8"))

df2 <- 
  df %>% 
  select(-cve_ent, -poblacion, city = nombre, everything())
