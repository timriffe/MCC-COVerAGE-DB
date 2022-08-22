data <- read.csv("./Data/chart.csv") [,c(1,4)] %>% 
select(date = ï..Specimen.received.date, cases = City.Of.Cape.Town.Metro) %>% 
  mutate(mcccityname1 = "Cape Town",
         date = as.Date(date)) %>% 
         mutate(cases = cases - lag(cases, default = 0))

         

ggplot(data, aes(x = date , y = cases)) +
  geom_line() +
 # facet_wrap(~regionname) +
  theme_bw()

write.csv(data, "./dat/South_Africa_city.csv", row.names = F)

data <- data %>% 
  select(date, regionname = mcccityname1, cases)

write.csv(data, "./dat/South_Africa_regions.csv", row.names = F)
