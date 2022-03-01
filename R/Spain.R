

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

# CCAA Valencia
# Castellon
# Alicante
# Valencia 
val_url <- "https://dadesobertes.gva.es/dataset/ce195af2-39ec-4f44-bb77-b14235519b0d/resource/cb50e7d2-0c0e-46b8-a359-a0fa35998577/download/covid-19-serie-de-casos-con-pdia-positiva-en-la-comunitat-valenciana.csv"
valencia <- read_delim(val_url, delim = ";")
VAL <-
  valencia %>% 
  select(date = `Data diagnòstic laboratori/fecha diagnóstico laboratorio`,
         contains("DEPARTAMENT")) %>% 
  pivot_longer(contains("DEPARTAMENT"), names_to = "dep", values_to = "cases") %>% 
  mutate(mcccityname1 = case_when(
    grepl(dep, pattern = "VALENCIA") ~ "Valencia",
    grepl(dep, pattern = "ALACANT") ~ "Alicante",
    grepl(dep, pattern = "CASTELLO") ~ "Castellon",
    TRUE ~ NA_character_)) %>% 
  dplyr::filter(!is.na(mcccityname1)) %>% 
  group_by(mcccityname1, date) %>% 
  summarize(cases = sum(cases), .groups = "drop")

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
                                  Municipio == "Sevilla (capital)" ~ "Sevilla")) %>% 
  group_by(mcccityname1) %>% 
  mutate(cases = ConfirmadosTotal - lag(ConfirmadosTotal),
         cases = ifelse(cases < 0, 0, cases),
         date = Fecha) %>% 
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
# https://dgspasturias.shinyapps.io/panel_de_indicadores_asturias/
OV <- read_csv("https://dgspasturias.shinyapps.io/panel_de_indicadores_asturias/_w_fde98144/DATOS/TABLAS_RESUMEN/AREAS_SANITARIAS/area_sanit_IV_resumen.csv") %>% 
  mutate(date = fecha,
         cases = casos_diarios,
         mcccityname1 = "Oviedo") 

# (Catalunya)
# Barcelona
# Lleida
# Girona
# Tarragona
# from here: https://analisi.transparenciacatalunya.cat/ca/Salut/Registre-de-casos-de-COVID-19-a-Catalunya-per-muni/jj6z-iyrp
CAT <- read_csv("https://analisi.transparenciacatalunya.cat/api/views/jj6z-iyrp/rows.csv?accessType=DOWNLOAD&sorting=true") %>% 
  mutate(mcccityname1 = case_when(
    ComarcaDescripcio == "BARCELONES" ~ "Barcelona",
    ComarcaDescripcio == "GIRONES" ~ "Girona",
    ComarcaDescripcio == "TARRAGONES" ~ "Tarragona",
    ComarcaDescripcio == "SEGRIA" ~ "Lleida",
    TRUE ~ NA_character_),
  date = lubridate::dmy(TipusCasData)) %>% 
  dplyr::filter(!is.na(mcccityname1)) %>% 
  select(date, cases = NumCasos, mcccityname1) %>% 
  group_by(mcccityname1, date) %>% 
  summarize(cases =sum(cases), .groups = "drop") %>% 
  complete(date,mcccityname1, fill = list(cases=0)) %>% 
  arrange(mcccityname1, date) 

# (Castilla y Leon)
# Avila
# Burgos
# Zamora  looking for: DOCTOR FLEMING
# Palencia
# Segovia
# Soria
# Valladolid
# Salamanca
# Leon
# https://datosabiertos.jcyl.es/web/jcyl/set/es/salud/tasa-coronavirus-zonas-basicas-salud/1284942912395
CastillaLeon_url <- "https://datosabiertos.jcyl.es/web/jcyl/risp/es/salud/tasa-coronavirus-zonas-basicas-salud/1284942912395.csv"
CL <- read_delim(CastillaLeon_url, delim = ";") %>% 
  mutate(mcccityname1 = case_when(
    MUNICIPIO == "BURGOS" ~ "Burgos",
    MUNICIPIO == "ZAMORA" ~ "Zamora",
    MUNICIPIO == "PALENCIA" ~ "Palencia",
    MUNICIPIO == "SEGOVIA" ~ "Segovia",
    MUNICIPIO == "SORIA" ~ "Soria",
    MUNICIPIO == "VALLADOLID" ~ "Valladolid",
    MUNICIPIO == "SALAMANCA" ~ "Salamanca",
    MUNICIPIO == "LEON" ~ "Leon",
    TRUE ~ NA_character_
  ),
  cases = PCR_POSITIVOS,
  date = FECHA) %>% 
  filter(!is.na(mcccityname1)) %>% 
  select(date, cases, mcccityname1) %>% 
  group_by(mcccityname1,date) %>% 
  summarize(cases = sum(cases), .groups = "drop") %>% 
  arrange(mcccityname1,date)

# Castilla La Mancha

# Ciudad Real
# Cuenca
# Guadalajara
# Toledo

# Albacete
ALB  <- read_tsv("https://datawrapper.dwcdn.net/TApn3/102/dataset.csv") %>% 
  mutate(date = lubridate::dmy(Día), cases = `Casos Albacete`, mcccityname1 = "Albacete") %>% 
  select(date, cases, mcccityname1)
ALB



# Badajoz
# Palma Mallorca
# Caceres
# Cadiz
# Santander
# Ceuta
# Logrono
# Palmas G. Canaria
# Madrid
# Melilla
# Murcia
# Pamplona
# Tenerife
# Valencia

# Zaragoza
# Teruel
# Huesca

# readLines("https://datacovid.salud.aragon.es/covid/session/ff26e22ebc4437d4e133449772a52b5f/dataobj/tablaCom?w=&nonce=5fd12e9fd9a5e5fb")
# fromJSON("https://datacovid.salud.aragon.es/covid/session/ff26e22ebc4437d4e133449772a52b5f/dataobj/tablaComm?w=&nonce=aba690e6ab16f87c")

"https://transparencia.aragon.es/sites/default/files/documents/20220227_casos_confirmados_zbs.xlsx"
library(lubridate)
dates <- seq(dmy("01.02.2020"),today(),by="days")
maybe_files <- paste0(year(dates),sprintf("%02d",month(dates)),sprintf("%02d",day(dates)),"_casos_confirmados_zbs.xlsx")

maybe_urls <- paste0("https://transparencia.aragon.es/sites/default/files/documents/",maybe_files)
try_download <- RCurl::url.exists(maybe_urls)
plot(try_download)

dates[!try_download]
# 2020-08-22,
#  2020-09-15, 2020-12-14, 2020-12-24, 2020-12-27, 2020-12-31, 2021-01-05, 2021-01-19, 2021-01-26,
#  2021-04-01, 2021-04-03, 2021-06-21, 2021-07-06, 2021-08-12, 2021-08-13, 2021-08-14, 2021-08-15,
#  2021-08-20, 2021-08-21, 2021-09-02, 2021-12-18, 2021-12-25