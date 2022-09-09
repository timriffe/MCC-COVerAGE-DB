library(tidyverse)
# for earlier cases
# https://raw.githubusercontent.com/asjadnaqvi/COVID19-European-Regional-Tracker/master/01_raw/France/donnees-tests-covid19-labo-quotidien-2020-05-29-19h00.csv

FR1 <- read_delim("https://raw.githubusercontent.com/asjadnaqvi/COVID19-European-Regional-Tracker/master/01_raw/France/donnees-tests-covid19-labo-quotidien-2020-05-29-19h00.csv",
                  delim=";",
                  show_col_types = FALSE) %>% 
  rename(cases = nb_pos,
         date = jour) %>% 
  select(dep,date,cases)
FR1$date %>% range()


FR2 <- read_delim("Data/sp-dep-jour-2022-09-08-19h01.csv",
                  delim=";",
                  show_col_types = FALSE) %>% 
  mutate(cases = parse_number(P) / 100) %>% 
  arrange(dep,jour) %>% 
  rename(date = jour)%>% 
  select(dep,date,cases)

FR2$date %>% min()

FR_Dep <-
FR1 %>% 
  filter(date < FR2$date %>% min()) %>% 
  bind_rows(FR1) %>% 
  arrange(dep, date)
FR_Dep$dep %>% unique()
city_dep <- tibble(city = c("Bordeaux","Clermont-Ferrand","Dijon","Grenoble","Le Havre","Lille","Lens-Douai","Lyon","Montpellier","Marseille","Nice","Nancy","Nantes","Paris","Rennes","Strasbourg","Toulouse"),
                   dep = c("33","63","21","38","76","59","62","69","34","13","06","54","44","75","35","67","31"))

FR_cities <-
  FR_Dep %>% 
  right_join(city_dep,by="dep") %>% 
  select(-dep) %>% 
  write_csv("data_output/france.csv")
