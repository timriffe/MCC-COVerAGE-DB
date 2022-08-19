
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




##regions

data <- db_c %>% 
  filter(DEPARTAMENTO == "ANCASH" |
         DEPARTAMENTO == "APURIMAC" |
         DEPARTAMENTO == "AREQUIPA" |
         DEPARTAMENTO == "AYACUCHO" |
         DEPARTAMENTO == "CAJAMARCA" |
         DEPARTAMENTO == "CALLAO" |
         DEPARTAMENTO == "CUSCO" |
         DEPARTAMENTO == "HUANCAVELICA" |
         DEPARTAMENTO == "HUANUCO" |
         DEPARTAMENTO == "ICA" |
         DEPARTAMENTO == "JUNIN" |
         DEPARTAMENTO == "LA LIBERTAD" |
         DEPARTAMENTO == "LAMBAYEQUE" |
         DEPARTAMENTO == "LIMA" |
         DEPARTAMENTO == "LORETO" |
         DEPARTAMENTO == "MADRE DE DIOS" |
         DEPARTAMENTO == "MOQUEGUA" |
         DEPARTAMENTO == "PASCO" |
         DEPARTAMENTO == "PIURA" |
         DEPARTAMENTO == "PUNO" |
         DEPARTAMENTO == "SAN MARTIN" |
         DEPARTAMENTO == "TACNA" |
         DEPARTAMENTO == "TUMBES" |
         DEPARTAMENTO == "UCAYALI") %>% 
group_by(FECHA_RESULTADO, DEPARTAMENTO) %>% 
  summarise(cases = n()) %>% 
  filter(!is.na(FECHA_RESULTADO)) %>%  
  mutate(regionname = DEPARTAMENTO,
        date = ymd(FECHA_RESULTADO)) %>% 
  ungroup() %>% 
  select(date, regionname, cases)

ggplot(data, aes(x = date , y = cases)) +
  geom_line() +
  facet_wrap(~regionname) +
  theme_bw()


write.csv(data, "./dat/Peru_regions.csv", row.names = F)
