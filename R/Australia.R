library(readxl)
library(tidyverse)
library(ISOweek)
library(lubridate)
library(here)
library(httr)
source("https://raw.githubusercontent.com/timriffe/covid_age/master/Automation/00_Functions_automation.R")

###australia mcc
##cities
##cases for new south wales australia, total region and sydney
nsw_regional_cases <- read.csv("https://data.nsw.gov.au/data/dataset/aefcde60-3b0c-4bc0-9af1-6fe652944ec2/resource/5d63b527-e2b8-4c42-ad6f-677f14433520/download/confirmed_cases_table1_location_agg.csv")
sydney <- nsw_regional_cases %>% 
  select(Date = notification_date, Postcode = postcode, Region = lhd_2010_name, Subregion = lga_name19, Test = confirmed_by_pcr, Value = confirmed_cases_count) %>% 
  filter(Region == "Sydney") %>% 
  group_by(Date,Region) %>% 
  summarize(Value = sum(Value)) %>% 
  ungroup() %>% 
  tidyr::complete(Date, Region, fill = list(Value = 0)) %>% 
  arrange(Date) %>% 
  group_by(Region) %>% 
  mutate(Value = cumsum(Value)) %>% 
  ungroup() 
  



##total regions

req <- GET("https://api.github.com/repos/CSSEGISandData/COVID-19/git/trees/master?recursive=1")



file_path <- data.frame(unlist(lapply(content(req)$tree, function(x) x$path)))
colnames(file_path) = c('Path')
head(file_path)

file_path <- file_path %>%
  separate(Path,c('base','folder','filename'),'/') %>%
  filter(folder == 'csse_covid_19_daily_reports') %>%
  filter(str_detect(filename,'.csv'))

head(file_path)

i<-1
dataset <- tibble()
for (i in seq(i,nrow(file_path))){
  path <- paste0('https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_daily_reports/',file_path$filename[i])
  daily_data = readr::read_csv(content(GET(path)))
  daily_data$filename <- file_path$filename[i]
  daily_data <- tibble(data.frame(lapply(daily_data, as.character)))
  dataset = bind_rows(dataset,daily_data)
  print(path)
  Sys.sleep(1)
} 




australia <- dataset %>% 
  filter(Country_Region == "Australia") %>% 
  select(Region = Province_State, Country = Country_Region, Date = Last_Update, Value = Confirmed) %>% 
  mutate (Date = substr(Date, 1,10),
          Measure = "Cases",
          Sex = "b") %>% 
  arrange(Date, Region) %>%  
  ungroup() %>% 
    mutate(Date = ymd(Date),
           Date = paste(sprintf("%02d",day(Date)),
                        sprintf("%02d",month(Date)),
                        year(Date),
                        sep=".")) %>% 
  filter(Date != "NA.NA.NA")
  






















