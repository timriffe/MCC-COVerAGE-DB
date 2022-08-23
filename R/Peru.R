<<<<<<< HEAD
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
=======

# load data
library(archive)
library(xml2)
library(rvest)
library(readr)
library(lubridate)
m_url1 <- "https://www.datosabiertos.gob.pe/dataset/casos-positivos-por-covid-19-ministerio-de-salud-minsa"
 
html1 <- read_html(m_url1)
cases_url <- html_nodes(html1, xpath = '//*[@id="data-and-resources"]/div/div/ul/li/div/span/a') %>%
  html_attr("href")
db_c <- data.table::fread(cases_url[1])
readr::write_csv(db_c, file = data_source_c)




##regions

data <- db_c %>% 
  filter(DEPARTAMENTO == "ANCASH" |
         DEPARTAMENTO == "APURIMAC" |
         DEPARTAMENTO == "AREQUIPA" |
         DEPARTAMENTO == "AYACUCHO" |
         DEPARTAMENTO == "CAJAMARCA" |
         DEPARTAMENTO == "CALLAO" |
         DEPARTAMENTO == "CUSCO" |
         DEPARTAMENTO == "HUANCAVELICA" |
         DEPARTAMENTO == "HUANUCO" |
         DEPARTAMENTO == "ICA" |
         DEPARTAMENTO == "JUNIN" |
         DEPARTAMENTO == "LA LIBERTAD" |
         DEPARTAMENTO == "LAMBAYEQUE" |
         DEPARTAMENTO == "LIMA" |
         DEPARTAMENTO == "LORETO" |
         DEPARTAMENTO == "MADRE DE DIOS" |
         DEPARTAMENTO == "MOQUEGUA" |
         DEPARTAMENTO == "PASCO" |
         DEPARTAMENTO == "PIURA" |
         DEPARTAMENTO == "PUNO" |
         DEPARTAMENTO == "SAN MARTIN" |
         DEPARTAMENTO == "TACNA" |
         DEPARTAMENTO == "TUMBES" |
         DEPARTAMENTO == "UCAYALI") %>% 
group_by(FECHA_RESULTADO, DEPARTAMENTO) %>% 
  summarise(cases = n()) %>% 
  filter(!is.na(FECHA_RESULTADO)) %>%  
  mutate(regionname = DEPARTAMENTO,
        date = ymd(FECHA_RESULTADO)) %>% 
  ungroup() %>% 
  select(date, regionname, cases)

ggplot(data, aes(x = date , y = cases)) +
  geom_line() +
  facet_wrap(~regionname) +
  theme_bw()


write.csv(data, "./dat/Peru_regions.csv", row.names = F)
>>>>>>> 653996d79a382f75ccc1495a5dff3d10bea8a84c
