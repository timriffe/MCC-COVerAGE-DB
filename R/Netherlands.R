
download.file("https://opendata.arcgis.com/datasets/1365a2d9cb344b67999dd825c99cb1a5_0.csv", destfile = "Data/Netherlands_inpt.csv")

library(tidyverse)
library(lubridate)
NL <- read_csv("Data/Netherlands_inpt.csv")
NL %>% 
  filter(Gemeentenaam %in% c("Amsterdam","'s-Gravenhage","Eindhoven","Rotterdam","Utrecht"))  %>% 
  mutate(city = case_when(Gemeentenaam == "'s-Gravenhage" ~ "Den Haag",
                          TRUE ~ Gemeentenaam),
         date = as_date(Datum),
         country = "Netherlands",
         cases = Meldingen) %>% 
  select(country, city, date, cases) %>% 
  arrange(country, city, date)
#Amsterdam
#Den Haag "'s-Gravenhage"
#Eindhoven
#Rotterdam
#Utrecht