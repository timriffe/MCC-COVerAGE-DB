library(tidyverse)
#brazil cities

data2022 <- read.csv("U://cases-brazil-cities-time_2022.csv") %>% 
  filter(city == "BelÃ©m/PA" | city == "Belo Horizonte/MG" | city == "BrasilÃ©ia/AC" | city == "CuiabÃ¡/MT" | city == "Curitiba/PR" | city == "Fortaleza/CE" | 
           city == "GoianÃ©sia/GO" | city == "JoÃ£o Pessoa/PB" | city == "MaceiÃ³/AL" | city == "Manaus/AM" | city == "Natal/RN" | 
           city == "Porto Alegre/RS" | city == "Recife/PE" | city == "Salvador/BA" | city == "SÃ£o Luiz/RR" | city == "SÃ£o Paulo/SP"| city == "Teresina/PI" | city == "VitÃ³ria/ES" )



data2021 <- read.csv("U://cases-brazil-cities-time_2021.csv") %>% 
  filter(city == "BelÃ©m/PA" | city == "Belo Horizonte/MG" | city == "BrasilÃ©ia/AC" | city == "CuiabÃ¡/MT" | city == "Curitiba/PR" | city == "Fortaleza/CE" | 
           city == "GoianÃ©sia/GO" | city == "JoÃ£o Pessoa/PB" | city == "MaceiÃ³/AL" | city == "Manaus/AM" | city == "Natal/RN" | 
           city == "Porto Alegre/RS" | city == "Recife/PE" | city == "Salvador/BA" | city == "SÃ£o Luiz/RR" | city == "SÃ£o Paulo/SP"| city == "Teresina/PI" | city == "VitÃ³ria/ES" )



data2020 <- read.csv("U://cases-brazil-cities-time_2020.csv") %>% 
  filter(city == "BelÃ©m/PA" | city == "Belo Horizonte/MG" | city == "BrasilÃ©ia/AC" | city == "CuiabÃ¡/MT" | city == "Curitiba/PR" | city == "Fortaleza/CE" | 
           city == "GoianÃ©sia/GO" | city == "JoÃ£o Pessoa/PB" | city == "MaceiÃ³/AL" | city == "Manaus/AM" | city == "Natal/RN" | 
           city == "Porto Alegre/RS" | city == "Recife/PE" | city == "Salvador/BA" | city == "SÃ£o Luiz/RR" | city == "SÃ£o Paulo/SP"| city == "Teresina/PI" | city == "VitÃ³ria/ES" )



data <- rbind(data2020, data2021, data2022) %>% 
  select(date, city, newCases, totalCases) %>% 
  mutate(mcccityname1 = case_when(
    city == "BelÃ©m/PA" ~ "Belem",
    city == "Belo Horizonte/MG" ~ "Belo Horizonte",
    city == "BrasilÃ©ia/AC" ~ "Brasilia",
    city == "CuiabÃ¡/MT" ~ "Curitiba",
    city == "Curitiba/PR" ~ "Cuiaba",
    city == "Fortaleza/CE" ~ "Fortaleza",
    city == "GoianÃ©sia/GO" ~ "Goiania",
    city == "JoÃ£o Pessoa/PB" ~ "Joao Pessoa",
    city == "MaceiÃ³/AL" ~ "Maceio",
    city == "Manaus/AM" ~ "Manaus",
    city == "Natal/RN" ~ "Natal",
    city == "Porto Alegre/RS" ~ "Porto Alegre",
    city == "Recife/PE" ~ "Recife",
    city == "Salvador/BA" ~ "Salvador",
    city == "SÃ£o Luiz/RR" ~ "Sao Luis",
    city == "SÃ£o Paulo/SP" ~ "Sao Paulo",
    city == "Teresina/PI" ~ "Teresina",
    city == "VitÃ³ria/ES" ~ "Vitoria"
      ),
    date = as.Date(date))


ggplot(data, aes(x = date , y = newCases)) +
     geom_line() +
     facet_wrap(~mcccityname1) +
     theme_bw()
#a few cases with negative values


data <- data %>% 
  select(date, mcccityname, cases=newCases)

write.csv(data, "./dat/Brazil_cities.csv", row.names = F)

#brazil regions

data <- read.csv("https://raw.githubusercontent.com/wcota/covid19br/master/cases-brazil-states.csv") %>% 
  select(date, regionname = state, cases = newCases) %>% 
  mutate(regionname = case_when(
    regionname == "AC" ~ "Acre",
    regionname == "AL" ~ "Alagoas",
    regionname == "AM" ~ "Amazonas",
    regionname == "AP" ~ "Amapá",
    regionname == "BA" ~ "Bahia",
    regionname == "CE" ~ "Ceará",
    regionname == "DF" ~ "Distrito Federal",
    regionname == "ES" ~ "Espírito Santo",
    regionname == "GO" ~ "Goiás",
    regionname == "MA" ~ "Maranhão",
    regionname == "MG" ~ "Minas Gerais",
    regionname == "MS" ~ "Mato Grosso do Sul",
    regionname == "MT" ~ "Mato Grosso",
    regionname == "PA" ~ "Pará",
    regionname == "PB" ~ "Paraíba",
    regionname == "PE" ~ "Pernambuco",
    regionname == "PI" ~ "Piauí",
    regionname == "PR" ~ "Paraná",
    regionname == "RJ" ~ "Rio de Janeiro",
    regionname == "RN" ~ "Rio Grande do Norte",
    regionname == "RO" ~ "Rondônia",
    regionname == "RR" ~ "Roraima",
    regionname == "RS" ~ "Rio Grande do Sul",
    regionname == "SC" ~ "Santa Catarina",
    regionname == "SE" ~ "Sergipe",
    regionname == "SP" ~ "São Paulo",
    regionname == "TO" ~ "Tocantins"
    ),
    date = as.Date(date)) %>% 
  filter(!is.na(regionname))

ggplot(data, aes(x = date , y = cases)) +
  geom_line() +
  facet_wrap(~regionname) +
  theme_bw()

write.csv(data, "./dat/Brazil_regions.csv", row.names = F)

