library(readxl)
library(tidyverse)
library(arrow)

# #cities
data1=read_parquet("K:/CDC_Covid/covid_case_restricted_detailed-master_06_06_2022/COVID_Cases_Restricted_Detailed_06062022_Part_1.parquet")
data2=read_parquet("K:/CDC_Covid/covid_case_restricted_detailed-master_06_06_2022/COVID_Cases_Restricted_Detailed_06062022_Part_2.parquet")
data3=read_parquet("K:/CDC_Covid/covid_case_restricted_detailed-master_06_06_2022/COVID_Cases_Restricted_Detailed_06062022_Part_3.parquet")
data4=read_parquet("K:/CDC_Covid/covid_case_restricted_detailed-master_06_06_2022/COVID_Cases_Restricted_Detailed_06062022_Part_4.parquet")


data1.2 <- data1 %>%
  group_by(cdc_case_earliest_dt, res_county, res_state) %>%
  summarise( cases = n() )

data2.2 <- data2 %>%
  group_by(cdc_case_earliest_dt, res_county, res_state) %>%
  summarise( cases = n())

data3.2 <- data3 %>%
  group_by(cdc_case_earliest_dt, res_county, res_state) %>%
  summarise( cases = n())

data4.2 <- data4 %>%
  group_by(cdc_case_earliest_dt, res_county, res_state) %>%
  summarise( cases = n())

us_cities <- rbind(data1.2, data2.2, data3.2, data4.2) %>%
  group_by(cdc_case_earliest_dt, res_county, res_state) %>%
  summarise( cases = sum(cases)) %>% 
  ungroup() %>% 
  mutate(res_county = case_when(
    res_county == "LAPORTE" ~ "LA PORTE",
    res_county == "LASALLE" ~ "LA SALLE",
    res_county == "PRINCE GEORGE'S" ~ "PRINCE GEORGES",
    res_county == "ST. CHARLES" ~ "SAINT CHARLES",
    res_county == "ST. CLAIR" ~ "SAINT CLAIR",
    res_county == "ST. LOUIS" ~ "SAINT LOUIS",
    res_county == "ST. LOUIS CITY" ~ "SAINT LOUIS CITY",
    res_county == "ST. LUCIE" ~ "SAINT LUCIE",
    res_county == "ST. JOSEPH" ~ "ST JOSEPH",
    TRUE ~ res_county)) 

us_cities2 <- us_cities %>%   
mutate(res_county = case_when(
    res_state == "GA" & (res_county == "DEKALB" | res_county == "FULTON" | res_county == "COBB") ~ "DEKALB/FULTON/COBB",
    res_state == "TX" & (res_county == "HAYS" | res_county == "WILLIAMSON" | res_county == "TRAVIS") ~ "HAYS/WILLIAMSON/TRAVIS",
    res_state == "GA" & (res_county == "COLUMBIA" | res_county == "RICHMOND") ~ "COLUMBIA/RICHMOND",
    res_state == "MD" & (res_county == "BALTIMORE CITY" | res_county == "ANNE ARUNDEL" | res_county == "BALTIMORE") ~ "BALTIMORE CITY/ANNE ARUNDEL/BALTIMORE",
    res_state == "AL" & (res_county == "SHELBY" | res_county == "JEFFERSON") ~ "SHELBY/JEFFERSON",
    res_state == "SC" & (res_county == "BERKELEY" | res_county == "CHARLESTON") ~ "BERKELEY/CHARLESTON",
    res_state == "OH" & (res_county == "FRANKLIN" | res_county == "DELAWARE") ~ "FRANKLIN/DELAWARE",
    res_state == "SC" & (res_county == "RICHLAND" | res_county == "LEXINGTON") ~ "RICHLAND/LEXINGTON",
    res_state == "OH" & (res_county == "HAMILTON" | res_county == "CLERMONT") ~ "HAMILTON/CLERMONT",
    res_state == "TX" & (res_county == "COLLIN" | res_county == "DALLAS") ~ "COLLIN/DALLAS",
    res_state == "OH" & (res_county == "GREENE" | res_county == "MONTGOMERY") ~ "GREENE/MONTGOMERY",
    res_state == "CO" & (res_county == "JEFFERSON" | res_county == "ADAMS" | res_county == "ARAPAHOE" | res_county == "DENVER") ~ "JEFFERSON/ADAMS/ARAPAHOE/DENVER",
    res_state == "FL" & (res_county == "SAINT JOHNS" | res_county == "DUVAL") ~ "SAINT JOHNS/DUVAL",
    res_state == "IN" & (res_county == "HAMILTON" | res_county == "MARION") ~ "HAMILTON/MARION",
    res_state == "TX" & (res_county == "FORT BEND" | res_county == "HARRIS") ~ "FORT BEND/HARRIS",
    res_state == "MI" & (res_county == "CLINTON" | res_county == "EATON" | res_county == "INGHAM") ~ "CLINTON/EATON/INGHAM",
    res_state == "MN" & (res_county == "ANOKA" | res_county == "HENNEPIN") ~ "ANOKA/HENNEPIN",
    res_state == "LA" & (res_county == "ORLEANS" | res_county == "JEFFERSON") ~ "ORLEANS/JEFFERSON",
    res_state == "OK" & (res_county == "OKLAHOMA" | res_county == "CLEVELAND") ~ "OKLAHOMA/CLEVELAND",
    res_state == "NE" & (res_county == "DOUGLAS" | res_county == "SARPY") ~ "DOUGLAS/SARPY",
    res_state == "FL" & (res_county == "BREVARD" | res_county == "ORANGE") ~ "BREVARD/ORANGE",
    res_state == "PA" & (res_county == "DELAWARE" | res_county == "PHILADELPHIA") ~ "DELAWARE/PHILADELPHIA",
    res_state == "OR" & (res_county == "WASHINGTON" | res_county == "CLACKAMAS" | res_county == "MULTNOMAH") ~ "WASHINGTON/CLACKAMAS/MULTNOMAH",
    res_state == "VA" & (res_county == "RICHMOND CITY" | res_county == "HENRICO" | res_county == "CHESTERFIELD") ~ "RICHMOND CITY/HENRICO/CHESTERFIELD",
    res_state == "TX" & (res_county == "COMAL" | res_county == "BEXAR") ~ "COMAL/BEXAR",
    res_state == "MO" & (res_county == "SAINT LOUIS" | res_county == "SAINT LOUIS CITY") ~ "SAINT LOUIS/SAINT LOUIS CITY",
    res_state == "CA" & (res_county == "SAN FRANCISCO" | res_county == "SAN MATEO") ~ "SAN FRANCISCO/SAN MATEO",
    res_state == "FL" & (res_county == "SARASOTA" | res_county == "MANATEE") ~ "SARASOTA/MANATEE",
    res_state == "OH" & (res_county == "WOOD" | res_county == "LUCAS") ~ "WOOD/LUCAS",
    res_state == "NJ" & (res_county == "BURLINGTON" | res_county == "MERCER") ~ "BURLINGTON/MERCER",
    res_state == "OK" & (res_county == "TULSA" | res_county == "CREEK") ~ "TULSA/CREEK")) %>% 
  group_by(cdc_case_earliest_dt, res_county, res_state) %>%
  summarise( cases = sum(cases)) %>% 
  filter(!is.na(res_county))


us_cities3 <- rbind(us_cities, us_cities2)
#setwd(".")
match <- read.csv("https://raw.githubusercontent.com/grammakov/USA-cities-and-states/master/us_cities_states_counties.csv", sep = "|")[,-c(3,5)] %>% 
  unique() %>% 
  select(City, State = State.short, County)

cities <- read_xlsx("./dat/US_city_names.xlsx", col_names = FALSE) %>% 
  select(cityname = ...1, mcccityname1 = ...2, mcccityname2 = ...3) %>% 
  mutate(mcccityname3 = mcccityname2) %>% 
  separate(mcccityname3, c("trash", "State"), sep = "\\s*\\(|\\)") %>% 
  mutate(City = case_when(
  cityname ==  "bath" ~ "Bath", 
  cityname ==  "Boise City" ~ "Boise", 
  cityname ==  "boulder" ~ "Boulder", 
  cityname ==  "carlisle" ~ "Carlisle", 
  cityname ==  "charleston (WV)" ~ "Charleston", 
  cityname ==  "chattanooga" ~ "Chattanooga", 
  cityname ==  "corpus christi" ~ "Corpus Christi", 
  cityname ==  "East St. Louis" ~ "East Saint Louis", 
  cityname ==  "El centro" ~ "El Centro", 
  cityname ==  "flint" ~ "Flint", 
  cityname ==  "fort lauderdale" ~ "Fort Lauderdale", 
  cityname ==  "fort myers" ~ "Fort Myers", 
  cityname ==  "fort pierce" ~ "Fort Pierce", 
  cityname ==  "fort worth" ~ "Fort Worth", 
  cityname ==  "Grand haven" ~ "Grand Haven", 
  cityname ==  "green bay" ~ "Green Bay", 
  cityname ==  "greensburg" ~ "Greensburg", 
  cityname ==  "greenville" ~ "Greenville", 
  cityname ==  "honolulu" ~ "Honolulu", 
  cityname ==  "Iowa city" ~ "Iowa City", 
  cityname ==  "Jersey city" ~ "Jersey City", 
  cityname ==  "kenosha" ~ "Kenosha", 
  cityname ==  "lafayette (IN)" ~ "Lafayette", 
  cityname ==  "Lake charles" ~ "Lake Charles", 
  cityname ==  "lakeland" ~ "Lakeland", 
  cityname ==  "lancaster" ~ "Lancaster", 
  cityname ==  "lansing" ~ "Lansing", 
  cityname ==  "las vegas" ~ "Las Vegas", 
  cityname ==  "louisville" ~ "Louisville", 
  cityname ==  "madison (WI)" ~ "Madison", 
  cityname ==  "melbourne" ~ "Melbourne",
  cityname ==  "mcallen" ~ "Mcallen", 
  cityname ==  "memphis" ~ "Memphis", 
  cityname ==  "mercer" ~ "Mercer", 
  cityname ==  "middlesex" ~ "Middlesex", 
  cityname ==  "milwaukee" ~ "Milwaukee", 
  cityname ==  "minneapolis" ~ "Minneapolis", 
  cityname ==  "nashville" ~ "Nashville", 
  cityname ==  "new haven" ~ "New Haven", 
  cityname ==  "new london" ~ "New London", 
  cityname ==  "newark" ~ "Newark", 
  cityname ==  "newburgh" ~ "Newburgh", 
  cityname ==  "ocala" ~ "Ocala", 
  cityname ==  "Palm beach" ~ "Palm Beach", 
  cityname ==  "philadelphia" ~ "Philadelphia", 
  cityname ==  "pittsburgh" ~ "Pittsburgh", 
  cityname ==  "portland (ME)" ~ "Portland", 
  cityname ==  "raleigh" ~ "Raleigh", 
  cityname ==  "reading" ~ "Reading", 
  cityname ==  "rochester" ~ "Rochester", 
  cityname ==  "san antonio" ~ "San Antonio", 
  cityname ==  "san diego" ~ "San Diego", 
  cityname ==  "san francisco" ~ "San Francisco", 
  cityname ==  "sarasota" ~ "Sarasota", 
  cityname ==  "South bend" ~ "South Bend", 
  cityname ==  "springfield (MA)" ~ "Springfield", 
  cityname ==  "St. Charles" ~ "Saint Charles", 
  cityname ==  "St. Louis" ~ "Saint Louis", 
  cityname ==  "St. Petersburg" ~ "Saint Petersburg", 
  cityname ==  "stamford" ~ "Stamford", 
  cityname ==  "stockton" ~ "Stockton", 
  cityname ==  "trenton" ~ "Trenton", 
  cityname ==  "tucson" ~ "Tucson", 
  cityname ==  "ventura" ~ "Ventura", 
  cityname ==  "washington (DC)" ~ "Washington", 
  cityname ==  "wichita" ~ "Wichita", 
  cityname ==  "Winston-Salem" ~ "Winston Salem", 
  cityname ==  "york" ~ "York",
 TRUE ~ cityname
  ),
 State = case_when(
   cityname == "Santa Barbara" ~ "CA",
   TRUE ~ State
 ))


match2 <- merge(cities, match, by = c("City", "State") ,all = TRUE) %>% 
  filter(!is.na(mcccityname1)) %>% 
  select(City, res_state = State, cityname, mcccityname1, mcccityname2, res_county = County) %>% 
  mutate(res_county = case_when(
    City == "Atlanta" & (res_county == "DEKALB" | res_county == "FULTON" | res_county == "COBB") ~ "DEKALB/FULTON/COBB",
    City == "Austin" & (res_county == "HAYS" | res_county == "WILLIAMSON" | res_county == "TRAVIS") ~ "HAYS/WILLIAMSON/TRAVIS",
    City == "Augusta" & (res_county == "COLUMBIA" | res_county == "RICHMOND") ~ "COLUMBIA/RICHMOND",
    City == "Baltimore" & (res_county == "BALTIMORE CITY" | res_county == "ANNE ARUNDEL" | res_county == "BALTIMORE") ~ "BALTIMORE CITY/ANNE ARUNDEL/BALTIMORE",
    City == "Birmingham" & (res_county == "SHELBY" | res_county == "JEFFERSON") ~ "SHELBY/JEFFERSON",
    City == "Charleston" & (res_county == "BERKELEY" | res_county == "CHARLESTON") ~ "BERKELEY/CHARLESTON",
    City == "Columbus" & (res_county == "FRANKLIN" | res_county == "DELAWARE") ~ "FRANKLIN/DELAWARE",
    City == "Columbia" & (res_county == "RICHLAND" | res_county == "LEXINGTON") ~ "RICHLAND/LEXINGTON",
    City == "Cincinnati" & (res_county == "HAMILTON" | res_county == "CLERMONT") ~ "HAMILTON/CLERMONT",
    City == "Dallas" & (res_county == "COLLIN" | res_county == "DALLAS") ~ "COLLIN/DALLAS",
    City == "Dayton" & (res_county == "GREENE" | res_county == "MONTGOMERY") ~ "GREENE/MONTGOMERY",
    City == "Denver" & (res_county == "JEFFERSON" | res_county == "ADAMS" | res_county == "ARAPAHOE" | res_county == "DENVER") ~ "JEFFERSON/ADAMS/ARAPAHOE/DENVER",
    City == "Jacksonville" & (res_county == "SAINT JOHNS" | res_county == "DUVAL") ~ "SAINT JOHNS/DUVAL",
    City == "Indianapolis" & (res_county == "HAMILTON" | res_county == "MARION") ~ "HAMILTON/MARION",
    City == "Houston" & (res_county == "FORT BEND" | res_county == "HARRIS") ~ "FORT BEND/HARRIS",
    City == "Lansing" & (res_county == "CLINTON" | res_county == "EATON" | res_county == "INGHAM") ~ "CLINTON/EATON/INGHAM",
    City == "Minneapolis" & (res_county == "ANOKA" | res_county == "HENNEPIN") ~ "ANOKA/HENNEPIN",
    City == "New Orleans" & (res_county == "ORLEANS" | res_county == "JEFFERSON") ~ "ORLEANS/JEFFERSON",
    City == "Oklahoma City" & (res_county == "OKLAHOMA" | res_county == "CLEVELAND") ~ "OKLAHOMA/CLEVELAND",
    City == "Omaha" & (res_county == "DOUGLAS" | res_county == "SARPY") ~ "DOUGLAS/SARPY",
    City == "Orlando" & (res_county == "BREVARD" | res_county == "ORANGE") ~ "BREVARD/ORANGE",
    City == "Philadelphia" & (res_county == "DELAWARE" | res_county == "PHILADELPHIA") ~ "DELAWARE/PHILADELPHIA",
    City == "Portland" & (res_county == "WASHINGTON" | res_county == "CLACKAMAS" | res_county == "MULTNOMAH") ~ "WASHINGTON/CLACKAMAS/MULTNOMAH",
    City == "Richmond" & (res_county == "RICHMOND CITY" | res_county == "HENRICO" | res_county == "CHESTERFIELD") ~ "RICHMOND CITY/HENRICO/CHESTERFIELD",
    City == "San Antonio" & (res_county == "COMAL" | res_county == "BEXAR") ~ "COMAL/BEXAR",
    City == "Saint Louis" & (res_county == "SAINT LOUIS" | res_county == "SAINT LOUIS CITY") ~ "SAINT LOUIS/SAINT LOUIS CITY",
    City == "San Francisco" & (res_county == "SAN FRANCISCO" | res_county == "SAN MATEO") ~ "SAN FRANCISCO/SAN MATEO",
    City == "Sarasota" & (res_county == "SARASOTA" | res_county == "MANATEE") ~ "SARASOTA/MANATEE",
    City == "Toledo" & (res_county == "WOOD" | res_county == "LUCAS") ~ "WOOD/LUCAS",
    City == "Trenton" & (res_county == "BURLINGTON" | res_county == "MERCER") ~ "BURLINGTON/MERCER",
    City == "Tulsa" & (res_county == "TULSA" | res_county == "CREEK") ~ "TULSA/CREEK",
    TRUE  ~ res_county)) %>% 
  unique()








#Atlanta DEKALB FULTON COBB
#Augusta COLUMBIA RICHMOND
#Austin HAYS WILLIAMSON TRAVIS
#Baltimore BALTIMORE CITY, ANNE ARUNDEL, BALTIMORE
#Birmingham SHELBY JEFFERSON
#Charleston BERKELEY CHARLESTON
#Columbus FRANKLIN DELAWARE
#Columbia RICHLAND LEXINGTON
#Cincinnati HAMILTON CLERMONT
#Dallas COLLIN DALLAS
#Dayton GREENE MONTGOMERY
#Denver JEFFERSON ADAMS ARAPAHOE DENVER
#Jacksonville SAINT JOHNS, DUVAL
#Indianapolis HAMILTON MARION
#Houston FORT BEND, HARRIS
#Lansing CLINTON EATON INGHAM
#Minneapolis ANOKA HENNEPIN
#New Orleans ORLEANS JEFFERSON
#Oklahoma City OKLAHOMA CLEVELAND
#Omaha DOUGLAS SARPY
#Orlando BREVARD ORANGE
#Philadelphia DELAWARE PHILADELPHIA
#Portland WASHINGTON CLACKAMAS MULTNOMAH
#Richmond RICHMOND CITY, HENRICO, CHESTERFIELD
#San Antonio COMAL BEXAR
#Saint Louis SAINT LOUIS, SAINT LOUIS CITY
#San Francisco SAN FRANCISCO, SAN MATEO
#Sarasota SARASOTA, MANATEE
#Toledo WOOD LUCAS
#Trenton BURLINGTON MERCER
#Tulsa TULSA CREEK

us_cities4 <- merge(us_cities3, match2, by = c("res_county", "res_state") ,all = TRUE)
us_cities4 <- us_cities4 %>% 
  filter(!is.na(City))
write.csv(us_cities4, "./dat/US_cities.csv", row.names = F)













