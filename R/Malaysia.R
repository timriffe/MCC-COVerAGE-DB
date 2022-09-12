library(tidyverse)

#malaysia regions

data <- read.csv("https://raw.githubusercontent.com/MoH-Malaysia/covid19-public/main/epidemic/cases_state.csv") %>% 
  mutate(regionname = case_when(
    state == "Johor" ~ "West Malaysia",             
    state == "Kedah" ~ "West Malaysia",          
    state == "Kelantan" ~ "West Malaysia",            
    state == "Melaka" ~ "West Malaysia",   
    state == "Negeri Sembilan" ~ "West Malaysia",            
    state == "Pahang" ~ "West Malaysia",             
    state == "Perak" ~ "West Malaysia",            
    state == "Perlis" ~ "West Malaysia", 
    state == "Pulau Pinang" ~ "West Malaysia",             
    state == "Sabah" ~ "East Malaysia",           
    state == "Sarawak" ~ "East Malaysia",          
    state == "Selangor" ~ "West Malaysia",        
    state == "Terengganu" ~ "West Malaysia", 
    state == "W.P. Kuala Lumpur" ~ "West Malaysia",       
    state == "W.P. Labuan" ~ "East Malaysia",    
    state == "W.P. Putrajaya" ~ "West Malaysia"
  )) %>% 
  group_by(date, regionname) %>% 
  summarise(cases = sum(cases_new)) %>% 
  mutate(date = as.Date(date))


ggplot(data, aes(x = date , y = cases)) +
  geom_line() +
  facet_wrap(~regionname) +
  theme_bw()

write.csv(data, "./dat/Malaysia_regions.csv", row.names = F)
