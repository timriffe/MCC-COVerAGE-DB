
cities <- 
  read_xlsx("data_input/location_info.xlsx",
            sheet = "cities_info") %>% 
  filter(countryname == "Chile") %>% 
  pull(cityname)


df <- read_csv("https://raw.githubusercontent.com/MinCiencia/Datos-COVID19/master/output/producto1/Covid-19.csv")

df2 <- 
  df %>% 
  filter(Comuna %in% c("Santiago", "Valparaiso", "Temuco")) %>% 
  select(-Region, -'Codigo region', -'Codigo comuna', -Poblacion) %>% 
  gather(-Comuna, key = date, value = cum_cases) %>% 
  mutate(date = ymd(date)) %>% 
  drop_na() %>% 
  arrange(Comuna, date) %>% 
  rename(city = Comuna)

dates <- seq(min(df2$date),max(df2$date), by = '1 day')

df3 <- 
  df2 %>% 
  complete(city, date = dates, fill = list(cum_cases = NA)) %>% 
  group_by(city) %>% 
  mutate(t = 1:n())


# interpolation of cumulative cases
chunk <- df3 %>% 
  filter(city == "Santiago")

interp <- function(chunk){
  ys <- 
    chunk %>% 
    drop_na() %>% 
    pull(cum_cases)
  
  xs <- 
    chunk %>% 
    drop_na() %>% 
    pull(t)
  
  # smoothing using cubic splines
  ts <- chunk %>% pull(t)
  ds <- chunk %>% pull(date)
  inter_cases <- 
    tibble(t = ts,
           date = ds,
           cum_cases_smt = spline(xs, ys, xout = ts)$y)
}

df4 <- 
  df3 %>% 
  group_by(city) %>% 
  do(interp(chunk = .data)) %>% 
  ungroup() %>% 
  mutate(cum_cases_smt = round(cum_cases_smt)) %>% 
  group_by(city) %>% 
  mutate(cases = cum_cases_smt - lag(cum_cases_smt)) %>% 
  drop_na() %>% 
  select(city, date, cases) %>% 
  mutate(country = "Chile")

write_csv(df4, "data_output/chile.csv")  
  