
library(lubridate)
library(tidyverse)
library(readr)
library(jsonlite)
# A Coruna   https://coronavirus.sergas.gal/datos/#
# go to "Número de persoas diagnosticadas a través de PDIA* e Autotest, por data"
# figure out file date and change below.
# Pontevedra
# Ourense
# Lugo
GALICIA <- read_csv("https://coronavirus.sergas.gal/infodatos/2022-04-03_COVID19_Web_InfectadosPorFecha_PDIA_Autotest.csv") 
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
# Cadiz
AND  <- read_csv("https://raw.githubusercontent.com/Pakillo/COVID19-Andalucia/master/datos/municipios.csv") %>% 
  mutate(mcccityname1 = case_when(Municipio == "Almería (capital)" ~ "Almeria",
                                  Municipio == "Córdoba (capital)" ~ "Cordoba",
                                  Municipio == "Granada (capital)" ~ "Granada",
                                  Municipio == "Huelva (capital)" ~ "Huelva",
                                  Municipio == "Jaén (capital)" ~ "Jaen",
                                  Municipio == "Málaga (capital)" ~ "Malaga",
                                  Municipio == "Sevilla (capital)" ~ "Sevilla",
                                  Municipio == "Cádiz (capital)" ~ "Cadiz" )) %>% 
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
         mcccityname1 = "Oviedo") %>% 
  select(date,cases,mcccityname1)

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
  mutate(date = lubridate::dmy(Día), 
         cases = `Casos Albacete`, 
         mcccityname1 = "Albacete") %>% 
  select(date, cases, mcccityname1)
ALB

##################
# Extremadura doesn't publish daily stats?
# emailed 	informacion.covid19@juntaex.es 4 april, 2022
# re-sent via a transparency request...
# Badajoz
# Caceres

##################
# Palma Mallorca
# data available in powerbi table:
# https://www.ibsalut.es/coronavirus-covid-19/situacio-actual-de-la-covid-19-a-les-illes-balears
# but table does not have selectable text. request sent to atenciousuari@ibsalut.caib.es  4 april, 2022
##################


# Canarias:
# Palmas G. Canaria
# Tenerife
options(download.file.method="curl", download.file.extra="-k -L")
download.file("https://opendata.sitcan.es/upload/sanidad/cv19_municipio-residencia_casos.csv",
              destfile = "Data/canarias.csv")
Canarias_cities<-
  read_csv("canarias.csv") %>% 
  dplyr::filter(municipio %in% c("Santa Cruz de Tenerife","Las Palmas de Gran Canaria")) %>% 
  mutate(date = lubridate::dmy(fecha_caso)) %>% 
  group_by(municipio, date) %>% 
  summarize(cases = n(), .groups = "drop") %>% 
  mutate(mcccityname1 = if_else(municipio == "Santa Cruz de Tenerife", "Tenerife", "Palmas G. Canaria")) %>% 
  select(date, mcccityname1, cases)
  

##################
# Santander (Cantabria)
sant_url <- "https://covid19can-data.firebaseio.com/saludcantabria/municipios/39075%20-%20SANTANDER/casos-diarios.json"
IN    <- jsonlite::fromJSON(sant_url)
cases <- IN$dimension$Fecha$category$index %>% unlist()
Santander <- tibble(date = names(cases) %>% lubridate::dmy(),
                    cases = cases,
                    mcccityname1 = "Santander")
rm(IN)
##################
# Navarra:
zbs_codes<- c(66,18,19,20,21,61,22,23,24,63)
# includes:
# Buztintxuri, Chantrea, Casco Viejo-I Ensanche, II Ensanche, 
# Milagrosa, Azpilagaña, Iturrama, San Juan , Ermitagaña, Mendillorri

NAV <- read_csv("https://datosabiertos.navarra.es/es/datastore/dump/79057b0e-055f-4a80-8c5a-f4a7260359f0?format=csv&bom=True",
         show_col_types = FALSE) 

correct_negatives <- function(chunk){
  chunk <- chunk %>% arrange(date)
  cases <- chunk$cases
  i <- 0
  while (any(cases < 0)){
    i    <- i + 1
    ind  <- which(cases < 0) %>% min()
    rest <- cases[ind]
    if ((ind - 1) >= 1){
      cases[ind - 1] <-  cases[ind - 1] - rest
    }
    cases[ind] <- 0
  }
  chunk$cases <- cases
  chunk
}
Pamplona <- 
  NAV %>% 
  dplyr::filter(CodZBS %in% zbs_codes) %>% 
  mutate(date = lubridate::as_date(Fecha)) %>% 
    arrange(CodZBS,date) %>% 
    group_by(CodZBS) %>% 
    mutate(cases = diff(c(0,`Casos Acumulados`))) %>% 
    ungroup() %>% 
    mutate(
         cases = if_else(CodZBS == 18 & date == dmy("29.01.2021"),0,cases),
         cases = if_else(CodZBS == 18 & date == dmy("09.02.2021"),9,cases),
         cases = if_else(CodZBS == 23 & date == dmy("29.01.2021"),0,cases),
         cases = if_else(CodZBS == 23 & date == dmy("09.02.2021"),15,cases),
         cases = if_else(CodZBS == 20 & date == dmy("29.01.2021"),0,cases),
         cases = if_else(CodZBS == 20 & date == dmy("09.02.2021"),17,cases),
         cases = if_else(CodZBS == 19 & date == dmy("29.01.2021"),0,cases),
         cases = if_else(CodZBS == 19 & date == dmy("09.02.2021"),12,cases),
         cases = if_else(CodZBS == 21 & date == dmy("29.01.2021"),0,cases),
         cases = if_else(CodZBS == 21 & date == dmy("09.02.2021"),7,cases),
         cases = if_else(CodZBS == 22 & date == dmy("29.01.2021"),0,cases),
         cases = if_else(CodZBS == 22 & date == dmy("09.02.2021"),6,cases),
         cases = if_else(CodZBS == 24 & date == dmy("29.01.2021"),0,cases),
         cases = if_else(CodZBS == 24 & date == dmy("09.02.2021"),2,cases),
         cases = if_else(CodZBS == 61 & date == dmy("29.01.2021"),0,cases),
         cases = if_else(CodZBS == 61 & date == dmy("09.02.2021"),12,cases),
         cases = if_else(CodZBS == 66 & date == dmy("29.01.2021"),0,cases),
         cases = if_else(CodZBS == 66 & date == dmy("09.02.2021"),8,cases),
         cases = if_else(CodZBS == 61 & date == dmy("27.05.2020"),0,cases),
         cases = if_else(CodZBS == 61 & date == dmy("28.05.2020"),6,cases),
         cases = if_else(CodZBS == 23 & date %in% c(dmy("06.04.2020"),dmy("07.04.2020")),0,cases),
         cases = if_else(CodZBS == 23 & date == dmy("05.04.2020"),32,cases)) %>% 
  do(correct_negatives(chunk = .data)) %>% 
  ungroup() %>% 
  group_by(date) %>% 
  summarize(cases = sum(cases), .groups = "drop") %>% 
  mutate(mcccityname1 = "Pamplona")
  # ggplot(aes(x=date,y=cases,color = CodZBS,group=CodZBS)) +geom_line()
  
# ---------------------
# Logrono (la rioja)

read_csv("https://ias1.larioja.org/opendata/download?r=Y2Q9ODU2fGNmPTAz")


# Ceuta

# Madrid
# Melilla
# Murcia




# Aragon transparencia wrote back with the missing dates!
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