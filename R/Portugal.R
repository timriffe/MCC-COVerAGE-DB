
download.file("https://raw.githubusercontent.com/dssg-pt/covid19pt-data/master/data_concelhos_new.csv",
              destfile= "Data/Portugal_input.csv")
library(tidyverse)
library(readxl)
library(lubridate)
library(stringr)
PT <- read_csv("Data/Portugal_input.csv")
PT %>% 
  mutate(date = dmy(data),
         cases = confirmados_1,
         city = str_to_title(concelho),
         country = "Portugal") %>% 
  filter(city %in% c("Beja","Coimbra","Castelo Branco","Faro","Lisboa","Porto")) %>% 
  select(country, city, date, cases) %>% 
  arrange(city, date) %>% 
  ggplot(aes(x = date, y = cases, color = city)) +
  geom_line()

PT$data %>% dmy() %>% unique() %>% sort()
Beja
Coimbra
Castelobranco
Faro
Lisboa
Porto