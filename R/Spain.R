

library(tidyverse)
library(readr)
# A Coruna   https://coronavirus.sergas.gal/datos/#
# Pontevedra
# Ourense
# Lugo
GALICIA <- read_csv("https://coronavirus.sergas.gal/infodatos/2022-02-24_COVID19_Web_InfectadosPorFecha_PDIA_Autotest.csv?_=1645791436049") 
GALICIAcities <- 
GALICIA %>% 
  dplyr::filter(Area_Sanitaria %in% c("A.S. A CORUÑA E CEE",
                                      "A.S. PONTEVEDRA E O SALNÉS",
                                      "A.S. LUGO, A MARIÑA E MONFORTE",
                                      "A.S. OURENSE, VERÍN E O BARCO")) %>% 
  complete(Fecha, Area_Sanitaria, fill = list(Personas_Infectadas = 0)) %>% 
  arrange(Area_Sanitaria, Fecha) %>% 
  rename(date = Fecha,
         cityname = Area_Sanitaria,
         cases = Personas_Infectadas) %>% 
  mutate(mcccityname1 = case_when(
    grepl(cityname, pattern = "CORUÑA") ~ "A Coruna",
    grepl(cityname, pattern = "PONTEVEDRA") ~ "Pontevedra",
    grepl(cityname, pattern = "LUGO") ~ "Lugo",
    grepl(cityname, pattern = "OURENSE") ~ "Ourense",
  )) %>% 
  select(-cityname) 
  # ggplot(aes(x = date, y = cases, color = city)) +
  # geom_line()



# Albacete
ALB  <- read_tsv("https://datawrapper.dwcdn.net/TApn3/102/dataset.csv") %>% 
  mutate(date = lubridate::dmy(Día), cases = `Casos Albacete`, mcccityname1 = "Albacete") %>% 
  select(date, cases, mcccityname1)
ALB

# Alicante
"https://dadesobertes.gva.es" # can't find

# Almeria
# Cordoba
# Granada
# Huelva
# Jaen
# Malaga
# Sevilla
AND  <- read_csv("https://raw.githubusercontent.com/Pakillo/COVID19-Andalucia/master/datos/municipios.csv") %>% 
  mutate(mcccityname1 = case_when(Municipio == "Almería (capital)" ~ "Almeria",
                                  Municipio == "Córdoba (capital)" ~ "Cordoba",
                                  Municipio == "Granada (capital)" ~ "Granada",
                                  Municipio == "Huelva (capital)" ~ "Huelva",
                                  Municipio == "Jaén (capital)" ~ "Jaen",
                                  Municipio == "Málaga (capital)" ~ "Malaga",
                                  Municipio == "Sevilla (capital)" ~ "Sevilla"),
         cases = Confirmados.PCR.TA - lag(Confirmados.PCR.TA),
         cases = ifelse(cases < 0, 0, cases),date = Fecha) %>% 
  select(date, cases, mcccityname1) %>% 
  dplyr::filter(!is.na(mcccityname1)) 


# Vitoria
# San Sebastian
# Bilbao
EUS <- readLines("https://opendata.euskadi.eus/contenidos/ds_informes_estudios/covid_19_2020/opendata/historico-situacion-epidemiologica.txt",
                       n = 1) %>% 
  rio::import(sheet = 5, skip = 1) 

# these would need to be grouped to Bilbao, Vitoria, and Donosti,
# groupings were determined using this tool:
# https://www.osakidetza.euskadi.eus/tu-centro-sanitario-buscador/-/buscador-de-centros-sanitarios-y-hospitales/

EUS %>% 
  pull(`OSASUN-EREMUAK/ZONAS DE SALUD`) %>% unique()

bilbao_centros <- c("Basauri-Ariz","Basurto","Bolueta","Basauri-Kareaga","Begoña","Bombero Etxaniz",
                    "Casco Viejo (Bilbao)","Gazteleku","Javier Sáenz de Buruaga","La Peña",
                    "Mina del Morro","Miribilla-La Merced","Otxarkoaga","San Adrián","San Ignacio",
                    "Santutxu-Solokoetxe","Txurdinaga","Zorroza","Zurbaran","Karmelo" )
vitoria_centros <- c("Olaguibel", "Aranbizkarra I","Aranbizkarra II","Abetxuko","Gazalbide Txagorritxu",
                     "Lakua - Arriaga","Lakuabizkarra","Olarizu","San Martín","Sansomendi",
                     "Zabalgana","Zaramaga","Salburua","Zabalgana","Casco Viejo (Vitoria)" )
donosti_centros <- c("Aiete","Alza-Roteta","Amara Centro", "Bidebieta" ,"Egia","Intxaurrondo",
                     "Loiola","Ondarreta","Parte Vieja")
EUS_cities <-
  EUS %>% 
  rename(centro = `OSASUN-EREMUAK/ZONAS DE SALUD`) %>% 
  pivot_longer(`2020/05/13`:ncol(.), names_to = "date", values_to = "cases") %>% 
  mutate(mcccityname1 = case_when(
    centro %in% bilbao_centros ~ "Bilbao",
    centro %in% vitoria_centros ~ "Vitoria",
    centro %in% donosti_centros ~ "San Sebastian",
    TRUE ~ NA_character_ 
  )) %>% 
  dplyr::filter(!is.na(mcccityname1)) %>% 
  group_by(mcccityname1, date) %>% 
  summarize(cases = sum(cases, na.rm=TRUE),.groups = "drop") %>% 
  mutate(date = lubridate::ymd(date)) %>% 
  dplyr::filter(!is.na(date))%>% 
  select(date, cases, mcccityname1)
# AND %>% 
#   ggplot(aes(x = date, y = cases, color = mcccityname1)) +
#   geom_line()

# Oviedo
# Avila
# Badajoz
# Palma Mallorca
# Barcelona
# Bilbao
# Burgos
# Caceres
# Cadiz
# Santander
# Castellon
# Ceuta
# Ciudad Real
# Cuenca
# Girona
# Guadalajara
# Huesca
# Logrono
# Palmas G. Canaria
# Leon
# Lleida
# Madrid
# Melilla
# Murcia
# Pamplona
# Palencia
# Salamanca
# Segovia
# Soria
# Tarragona
# Tenerife
# Teruel
# Toledo
# Valencia
# Valladolid
# Zamora
# Zaragoza










