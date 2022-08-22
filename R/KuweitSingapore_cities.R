#cities where city is the whole country
data <- read.csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv")[,-c(3,4)] %>% 
  filter(Country.Region == "Kuwait" | Country.Region == "Singapore") %>% 
melt(id.vars = "Country.Region") %>% 
  mutate(variable = as.character(variable),
         variable = gsub("X", "", variable),
         variable = mdy(variable),
         value = as.numeric(value)) %>% 
  filter(!is.na(value)) %>% 
  select(mcccityname1 = Country.Region, date = variable, cases = value)

write.csv(data, "./dat/KuweitSingapore_cities.csv", row.names = F)
