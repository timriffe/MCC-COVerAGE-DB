#china
library(tidyverse)
library(reshape2)

china <- read.csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv") [,-c(3,4)] %>% 
  filter(Country.Region == "China",
         Province.State != "Unknown") %>% 
  melt(id.vars= "Province.State") %>% 
  filter(value != "China") %>% 
  select(regionname = Province.State, date = variable, cases = value) %>% 
  mutate(cases = as.numeric(cases),
         date = as.character(date),
         date = gsub("X", "", date),
         date = as.Date(date, "%m.%d.%y"))
write.csv(china, "./dat/China_regions.csv", row.names = F)
