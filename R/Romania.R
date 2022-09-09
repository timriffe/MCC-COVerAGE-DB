
RO_url <- 
"https://raw.githubusercontent.com/denesdata/roem/master/data/time_series_ro_counties_daily.csv"

RO <- 
read_csv(RO_url,
         show_col_types = FALSE) %>% 
  select(date,cases,county) %>% 
  mutate(city = case_when(county == "Brașov" ~ "Brasov",
                          county == "București" ~ "Bucharest",
                          county == "Cluj" ~ "Cluj-Napoca",
                          county == "Constanța" ~ "Constanta",
                          county == "Dolj" ~ "Craiova",
                          county == "Galați" ~ "Galati",
                          county == "Iași" ~ "Iasi",
                          county == "Timiș" ~ "Timisoara",
                          TRUE ~ NA_character_)) %>% 
  filter(!is.na(city))

# RO %>% 
#   filter(between(date,dmy("01.02.2022"),dmy("01.03.2022"))) %>% 
#   arrange(city,date) %>% 
#   View()

bad_day <- RO %>% 
  filter(date == dmy("21.02.2022")) %>% 
  mutate(date = dmy("22.02.2022") )

RO2 <- 
  RO %>% 
  select(-county) %>% 
  # filter(date != dmy("22.02.2022")) %>% 
  # bind_rows(bad_day) %>% 
  arrange(city, date) %>% 
  rename(cases_cum = cases) %>% 
  mutate(country = "Romania") %>% 
  group_by(city) %>% 
  mutate(
    cases_cum = if_else(cases_cum < lag(cases_cum), lag(cases_cum), cases_cum),
    cases = lead(cases_cum) - cases_cum) %>% 
  ungroup()

RO2 %>% 
  filter(!is.na(cases),
         !is.na(cases_cum)) %>% 
write_csv("data_output/Romania.csv")

RO2 %>% 
  ggplot(aes(x = date,y=cases,color = city)) + 
  geom_line()
RO2 %>% 
  filter(is.na(cases))
# need to match cities to counties
# Brasov
# Bucharest
# Cluj-Napoca
# Constanta
# Craiova
# Galati
# Iasi
# Timisoara