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
#Melbourne
mel <- read.csv("https://www.dhhs.vic.gov.au/sites/default/files/documents/202208/NCOV_COVID_Cases_by_LGA_20220825.csv")
#data <- read.csv("https://www.dhhs.vic.gov.au/sites/default/files/documents/202208/NCOV_COVID_Assumed_Positive_RAT_Cases_by_LGA_Postcode_20220819.csv")
mel <- data %>% 
  filter(Localgovernmentarea == "Melbourne (C)") %>% 
  select(date = diagnosis_date, mcccityname1 = Localgovernmentarea) %>% 
  group_by(date) %>% 
  summarise(cases = n()) %>% 
  mutate(mcccityname1 = "Melbourne") %>% 
  mutate(date = ymd(date))

ggplot(mel, aes(x = date , y = cases)) +
  geom_line() +
  #facet_wrap(~regionname) +
  theme_bw()


syd <- read.csv("https://data.nsw.gov.au/data/dataset/aefcde60-3b0c-4bc0-9af1-6fe652944ec2/resource/5d63b527-e2b8-4c42-ad6f-677f14433520/download/confirmed_cases_table1_location_agg.csv")
syd <- syd %>% 
  filter(lhd_2010_name == "Northern Sydney" | lhd_2010_name == "South Eastern Sydney" | lhd_2010_name == "South Western Sydney" |
           lhd_2010_name == "Sydney" | lhd_2010_name == "Western Sydney")




write.csv(data, "./dat/Australia_regions.csv", row.names = F)






