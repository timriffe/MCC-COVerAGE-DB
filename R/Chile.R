library(tidyverse)
library(reshape2)

data <- read.csv("https://raw.githubusercontent.com/MinCiencia/Datos-COVID19/master/output/producto1/Covid-19.csv") [,-c(2,3,4,5)] %>%   
  melt(id.vars="Region") %>% 
  filter(!is.na(value)) %>% 
  select(Region, date= variable, cases = value) %>% 
  mutate(date = gsub("X","", date),
         date = as.Date(date, "%Y.%m.%d")) %>% 
  filter(!is.na(date)) %>% 
  select(regionname = Region, date, cases) %>% 
  group_by(regionname, date) %>% 
  summarise(cases = sum(cases)) %>% 
  mutate(cases = cases - lag(cases, default = 0)) %>% 
  mutate(regionname = case_when(
    regionname == "Ã‘uble" ~ "Ñuble",        
    regionname == "Antofagasta" ~ "Antofagasta",         
    regionname == "AraucanÃ­a" ~ "Araucanía", 
    regionname == "Arica y Parinacota" ~ "Arica y Parinacota",            
    regionname == "Atacama" ~ "Atacama",             
    regionname == "AysÃ©n" ~ "Aysén",            
    regionname == "BiobÃ­o" ~ "Biobío",           
    regionname == "Coquimbo" ~ "Coquimbo", 
    regionname == "Los Lagos" ~ "Los Lagos",          
    regionname == "Los RÃ­os" ~ "Los Ríos",         
    regionname == "Magallanes" ~ "Magallanes",              
    regionname == "Maule" ~ "Maule",      
    regionname == "Metropolitana" ~ "Región Metropolitana",        
    regionname == "Oâ€™Higgins" ~ "O’Higgins",          
    regionname == "TarapacÃ¡" ~ "Tarapacá",        
    regionname == "ValparaÃ­so" ~ "Valparaíso" 
  ))

  
 ggplot(data, aes(x = date , y = cases)) +
  geom_line() +
  facet_wrap(~regionname) +
  theme_bw()
 
 write.csv(data, "./dat/Chile_regions.csv", row.names = F)
 

