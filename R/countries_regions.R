#regions where region is the whole country
data <- read.csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv")[,-c(3,4)]  %>% 
  filter(Country.Region == "Costa Rica" | Country.Region == "Guatemala" | Country.Region == "Ireland" | Country.Region == "Kuweit" |
           Country.Region == "Panama" | Country.Region == "Singapore" | Country.Region == "Taiwan*") %>% 
  melt(id.vars = "Country.Region") %>% 
  mutate(variable = as.character(variable),
         variable = gsub("X", "", variable),
         variable = mdy(variable),
         value = as.numeric(value)) %>% 
  filter(!is.na(value)) %>% 
  select(regionname = Country.Region, date = variable, cases = value) %>% 
  mutate(regionname = case_when(
    regionname == "Taiwan*" ~ "Taiwan",
    TRUE ~ regionname
  ))

write.csv(data, "./dat/countries_regions.csv", row.names = F)
