library(tidyverse)

mb <- read_csv("data_inter/canada_manitoba.csv")
ml <- read_csv("data_inter/canada_montreal.csv")
on <- read_csv("data_inter/canada_ontario.csv")
bc <- read_csv("data_inter/canada_bc.csv")
ab <- read_csv("data_inter/canada_alberta.csv")
sk <- read_csv("data_inter/canada_saskatchewan.csv")

can <- 
  bind_rows(mb, ml, on, bc, ab, sk) %>% 
  drop_na()

unique(can$city)
write_csv(can, "data_output/canada.csv")
