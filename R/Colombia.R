library(lubridate)
library(tidyverse)
library(readr)
# library(jsonlite)
library(readxl)

cases_url <- 
  "https://www.datos.gov.co/api/views/gt2j-8ykr/rows.csv?accessType=DOWNLOAD"

# saving compressed data to N: drive
download.file(cases_url, destfile = "data_in/colombia.csv")

# Loading data in the session
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~
# cases and deaths database
db <- 
  read_csv("data_in/colombia.csv",
           locale = locale(encoding = "UTF-8"))


db2 <- db %>% 
  rename(Sex = Sexo,
         Region = 'Nombre departamento',
         City = 'Nombre municipio',
         status = 'Estado',
         unit = 'Unidad de medida de edad') %>% 
  mutate(Age = case_when(unit != 1 ~ 0,
                         unit == 1 & Edad <= 100 ~ Edad, 
                         unit == 1 & Edad > 100 ~ 100),
         Region = str_to_title(Region),
         Region = ifelse(Region == "Sta Marta D.e.", "Santa Marta", Region)) 

# unique(db2$Age)

cities <- c("MEDELLIN",
            "CALI")

db_city <- db2 %>% 
  filter(City %in% cities) %>% 
  mutate(Region = str_to_title(City))

db3 <- db2 %>% 
  bind_rows(db_city)

unique(db3$Age) %>% sort()

# cases ----------------------------------------------
# three dates for cases, preferred in this order: diagnosed, symptoms, reported to web
db_cases <- db3 %>% 
  rename(date_diag = 'Fecha de diagnóstico',
         date_repo = 'Fecha de notificación',
         date_sint = 'Fecha de inicio de síntomas') %>% 
  #separate(date_diag, c("date_diag", "trash1"), sep = " ") %>% 
  #separate(date_sint, c("date_sint", "trash2"), sep = " ") %>% 
  #separate(date_repo1, c("date_repo", "trash3"), sep = " ") %>% 
  mutate(date_f = case_when(!is.na(date_diag) ~ date_diag,
                            is.na(date_diag) & !is.na(date_sint) ~ date_sint,
                            is.na(date_diag) & is.na(date_sint) ~ date_repo),
         Measure = "Cases") %>% 
  mutate(date_f = as.character(date_f)) %>% 
  select(date_f, Age, Sex, Region, Measure)

# deaths -----------------------------------------------------------
db_deaths <- db3 %>% 
  filter(status == "Fallecido") %>% 
  rename(date = 'Fecha de muerte') %>% 
  separate(date, c("date_f", "trash1"), sep = " ") %>% 
  mutate(Measure = "Deaths") %>% 
  select(date_f, Age, Sex, Region, Measure)