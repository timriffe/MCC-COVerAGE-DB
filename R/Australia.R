library(tidyverse)
library(reshape2)
library(lubridate)

data <- read.csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv")[,-c(3,4)] %>% 
  filter(Country.Region == "Australia") %>% 
  melt(id.vars = "Province.State") %>% 
  filter(value != "Australia") %>% 
  mutate(variable = as.character(variable),
         variable = gsub("X", "", variable),
         variable = mdy(variable),
         value = as.numeric(value)) %>% 
  select(regionname = Province.State, date = variable, cases = value)

write.csv(data, "./dat/Australia_regions.csv", row.names = F)




#cities

data <- read.csv("https://www.dhhs.vic.gov.au/sites/default/files/documents/202208/NCOV_COVID_Assumed_Positive_RAT_Cases_by_LGA_Postcode_20220819.csv")







