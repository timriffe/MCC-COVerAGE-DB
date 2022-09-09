
# Brisbane covid data:
# https://www.data.qld.gov.au/dataset/queensland-covid-19-case-line-list-location-source-of-infection/resource/1dbae506-d73c-4c19-b727-e8654b8be95a
library(lubridate)
QL <- read_csv("Data/opendata_qld_covidcase_loc.csv")

Brisbane <-
  QL %>% 
  filter(LGA_NAME == "Brisbane City") %>% 
  mutate(date = lubridate::dmy(NOTIFICATION_DATE)) %>% 
  count(date, name = "cases")

Brisbane %>% pull(date) %>% range()

all_dates <- seq(ymd("2020-02-22"),ymd("2022-08-30"), by = "days")

Brisbane %>% 
  complete(date = all_dates, fill = list(cases = 0)) %>% 
  mutate(city = "Brisbane",
         country = "Australia") %>% 
  write_csv("data_output/Australia_Brisbane.csv")
