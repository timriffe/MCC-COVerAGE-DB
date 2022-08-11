library("tidyverse")
library("reshape2")

data <- read.csv("https://raw.githubusercontent.com/jmcastagnetto/covid-19-peru-data/main/datos/covid-19-peru-data.csv") %>% 
  filter(!is.na(region)) %>% 
  select(regionname = region, date, cases = confirmed) %>% 
  arrange(regionname, date) %>% 
  filter(!is.na(cases)) %>% 
  mutate(date = as.Date(date))


lima <- data %>% 
  filter(regionname == "Lima" | regionname == "Lima Metropolitana" | regionname == "Lima RegiÃ³n") %>% 
  dcast(date ~ regionname) %>% 
  select(date, var1 = "Lima", var2 = "Lima Metropolitana", var3 = "Lima RegiÃ³n" )

lima[is.na(lima)] <- 0
lima <- lima %>% 
  mutate(Lima2 = var1 + var2 + var3) %>% 
  mutate(regionname = "Lima") %>% 
  select(regionname, date, cases = Lima2)



 
 data <- data %>%   
   filter(regionname != "Lima",
          regionname != "Lima Metropolitana",
          regionname != "Lima RegiÃ³n") %>% 
   rbind(lima) %>% 
   mutate(cases = case_when( #data manipulation 
     cases == 51377 ~ 57377,
     cases == 109078 ~ 108078,
     cases == 22727 ~ 22127,
     cases == 7890 ~ 8890,
     TRUE ~ cases
   ))
#needs some further adaption, some negative values
 
 data2 <- data %>% 
   group_by(regionname) %>% 
   mutate(daily = cases - lag(cases, default = 0)) %>% 
   select(regionname, date, cases = daily)
table(data2$regionname) 

write.csv(data2, "./dat/Peru_regions.csv", row.names = F)


ggplot(data2, aes(x = date , y = cases)) +
  geom_line() +
  facet_wrap(~regionname) +
  theme_bw()


##other source
source(here::here("Automation/00_Functions_automation.R"))
#install.packages("archive")
library(archive)


# assigning Drive credentials in the case the script is verified manually  
if (!"email" %in% ls()){
  email <- "gatemonte@gmail.com"
}

# info country and N drive address
ctr <- "Peru"
dir_n <- "N:/COVerAGE-DB/Automation/Hydra/"

# Drive credentials
drive_auth(email = Sys.getenv("email"))
gs4_auth(email = Sys.getenv("email"))

# load data
library(archive)
library(xml2)
library(rvest)
library(readr)
library(lubridate)
m_url1 <- "https://www.datosabiertos.gob.pe/dataset/casos-positivos-por-covid-19-ministerio-de-salud-minsa"
 
html1 <- read_html(m_url1)
cases_url <- html_nodes(html1, xpath = '//*[@id="data-and-resources"]/div/div/ul/li/div/span/a') %>%
  html_attr("href")
db_c <- data.table::fread(cases_url[1])
readr::write_csv(db_c, file = data_source_c)




##cities

lima <- db_c %>% 
  filter(PROVINCIA == "LIMA") %>% 
  group_by(FECHA_RESULTADO) %>% 
  summarise(cases = n()) %>% 
  filter(!is.na(FECHA_RESULTADO)) %>% 
  mutate(mcccityname1 = "Lima",
        date = ymd(FECHA_RESULTADO)) %>% 
  select(date, mcccityname1, cases)

write.csv(lima, "./dat/Peru_cities.csv", row.names = F)
