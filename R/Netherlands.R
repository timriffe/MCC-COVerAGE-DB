
download.file("https://opendata.arcgis.com/datasets/1365a2d9cb344b67999dd825c99cb1a5_0.csv", destfile = "Data/Netherlands_inpt.csv")

library(tidyverse)
library(lubridate)
NL <- read_csv("Data/Netherlands_inpt.csv")

NL_out <-
NL %>% 
  filter(Gemeentenaam %in% c("Amsterdam","'s-Gravenhage","Eindhoven","Rotterdam","Utrecht"))  %>% 
  mutate(city = case_when(Gemeentenaam == "'s-Gravenhage" ~ "Den Haag",
                          TRUE ~ Gemeentenaam),
         date = as_date(Datum),
         country = "Netherlands",
         cases_cum = Meldingen) %>% 
  select(country, city, date, cases_cum) %>% 
  arrange(country, city, date) %>% 
  group_by(city) %>% 
  mutate(cases = if_else(is.na(cases_cum), 0, cases_cum),
         cases = cases_cum - lag(cases_cum),
         cases = if_else(is.na(cases), 0, cases),
         cases = if_else(cases < 0, 0, cases)) %>% 
  ungroup()

NL_out %>% 
  ggplot(aes(x=date,y=cases,color=city))+
  geom_line()

NL_out %>% 
  filter(cases < 0)

NL_out %>% 
  write_csv("data_output/netherlands.csv")

#Amsterdam
#Den Haag "'s-Gravenhage"
#Eindhoven
#Rotterdam
#Utrecht