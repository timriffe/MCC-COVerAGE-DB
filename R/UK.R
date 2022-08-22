#cities
data <- read.csv("https://coronavirus.data.gov.uk/api/v2/data?areaType=ltla&metric=newCasesByPublishDate&format=csv") %>% 
  filter(areaName == "Barnsley" |
         areaName == "Basildon" |
         areaName == "Basingstoke and Deane" |
         areaName == "Bedford" |
         areaName == "Birmingham" |
         areaName == "Blackburn with Darwen" |
         areaName == "Blackpool" |
         areaName == "Brighton and Hove" |
         areaName == "Bristol, City of" |
         areaName == "Burnley" |
         areaName == "Cambridge" |
         areaName == "Chelmsford" |
         areaName == "Cheltenham" |
         areaName == "Chesterfield" |
         areaName == "Colchester" |
         areaName == "Coventry" |
         areaName == "Crawley" |
         areaName == "Derby" |
         areaName == "Doncaster" |
         areaName == "Eastbourne" |
         areaName == "Exeter" |
         areaName == "Gloucester" |
         areaName == "Hastings" |
         areaName == "Ipswich" |
         areaName == "Kingston upon Hull, City of" |
         areaName == "Leicester" |
         areaName == "Lincoln" |
         areaName == "Liverpool" |
         areaName == "Luton" |
         areaName == "Maidstone" |
         areaName == "Manchester" |
         areaName == "Mansfield" |
         areaName == "Medway" |
         areaName == "Milton Keynes" |
         areaName == "Northampton" |
         areaName == "Norwich" |
         areaName == "Nottingham" |
         areaName == "Oxford" |
         areaName == "Peterborough" |
         areaName == "Plymouth" |
         areaName == "Preston" |
         areaName == "Reading" |
         areaName == "Sheffield" |
         areaName == "Slough" |
         areaName == "Southend-on-Sea" |
         areaName == "Stoke-on-Trent" |
         areaName == "Sunderland" |
         areaName == "Swindon" |
         areaName == "Thanet" |
         areaName == "Warrington" |
         areaName == "Wigan" |
         areaName == "Worcester" |
         areaName == "York" ) %>% 
    select(date, mcccityname1 = areaName, cases =newCasesByPublishDate)
  
london <- read.csv("https://coronavirus.data.gov.uk/api/v2/data?areaType=region&areaCode=E12000007&metric=newCasesByPublishDate&format=csv") %>% 
select(date, mcccityname1 = areaName, cases =newCasesByPublishDate)

data <- rbind(data, london) %>% 
  mutate(date = as.Date(date))

ggplot(data, aes(x = date , y = cases)) +
  geom_line() +
  facet_wrap(~cityname) +
  theme_bw()

write.csv(data, "./dat/UK_cities.csv", row.names = F)

#regions

data <- read.csv("https://coronavirus.data.gov.uk/api/v2/data?areaType=region&metric=newCasesByPublishDate&format=csv") %>% 
  select(date, regionname = areaName, cases = newCasesByPublishDate) %>% 
  mutate(date = as.Date(date))

ggplot(data, aes(x = date , y = cases)) +
  geom_line() +
  facet_wrap(~regionname) +
  theme_bw()

write.csv(data, "./dat/UK_regions.csv", row.names = F)

#nations
data <- read.csv("https://coronavirus.data.gov.uk/api/v2/data?areaType=nation&metric=newCasesByPublishDate&format=csv") %>% 
  filter(areaName == "Scotland" | areaName == "Wales") %>% 
  select(date, regionname = areaName, cases = newCasesByPublishDate) %>% 
  mutate(date = as.Date(date))

write.csv(data, "./data/WalesScotland_regions.csv", row.names = F)

