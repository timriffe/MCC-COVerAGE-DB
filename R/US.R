
library(arrow)
library(tidyverse)
# Read in data 

data1=read_parquet("K:/CDC_Covid/covid_case_restricted_detailed-master_06_06_2022/COVID_Cases_Restricted_Detailed_06062022_Part_1.parquet")
data2=read_parquet("K:/CDC_Covid/covid_case_restricted_detailed-master_06_06_2022/COVID_Cases_Restricted_Detailed_06062022_Part_2.parquet")
data3=read_parquet("K:/CDC_Covid/covid_case_restricted_detailed-master_06_06_2022/COVID_Cases_Restricted_Detailed_06062022_Part_3.parquet")
data4=read_parquet("K:/CDC_Covid/covid_case_restricted_detailed-master_06_06_2022/COVID_Cases_Restricted_Detailed_06062022_Part_4.parquet")
#IN <- rbind(data1, data2, data3, data4)
#remove(data1, data2, data3, data4)

data1.2 <- data1 %>% 
  group_by(cdc_case_earliest_dt, res_state) %>% 
  summarise( cases = n())

data2.2 <- data2 %>% 
  group_by(cdc_case_earliest_dt, res_state) %>% 
  summarise( cases = n())

data3.2 <- data3 %>% 
  group_by(cdc_case_earliest_dt, res_state) %>% 
  summarise( cases = n())

data4.2 <- data4 %>% 
  mutate(res_state = case_when(
    res_state == "CA." ~ "CA",
TRUE ~ res_state)) %>% 
  group_by(cdc_case_earliest_dt, res_state) %>% 
  summarise( cases = n())

us_regions <- rbind(data1.2, data2.2, data3.2, data4.2) %>% 
  group_by(cdc_case_earliest_dt, res_state) %>% 
  summarise( cases = sum(cases)) %>% 
mutate(regioncode= recode(res_state, 
                      `AL` = "Alabama",
                      `AK` = "Alaska",
                      `CA` = "California",
                      `CO` = "Colorado",
                      `CT` = "Connecticut",
                      `FL` = "Florida",
                      `GA` = "Georgia",
                      `HI` = "Hawaii",
                      `IL` = "Illinois",
                      `IN` = "Indiana",
                      `IA` = "Iowa",
                      `KY` = "Kentucky",
                      `LA` = "Louisiana",
                      `MD` = "Maryland",
               `AZ` = "Arizona",
               `AR`= "Arkansas",	
               `DE`= "Delaware",
               `DC` = "District of Columbia",
               `GU`=  "Guam",
               `ID`= "Idaho",
               `KS`= "Kansas",	
               `ME`=  "Maine",	
               `MA`=  "Massachusetts",
               `MI` = "Michigan",
               `MN`=  "Minnesota",
               `MS` = "Mississippi",
               `MO` = "Missouri",
               `MT`=  "Montana",
               `NE` = "Nebraska",
               `NV`=  "Nevada",
               `NH` = "New Hampshire",
               `NJ`=  "New Jersey",	
               `NM` = "New Mexico",
               `NY` = "New York",
               `NC`=  "North Carolina",
               `ND` = "North Dakota",
               `OH` = "Ohio",
               `OK`=  "Oklahoma",	
               `OR`=  "Oregon",	
               `PA`=  "Pennsylvania",	
               `RI` = "Rhode Island",
               `SC`= "South Carolina",
               `SD` = "South Dakota",
               `TN`= "Tennessee",	
               `TX` = "Texas",
               `UT` = "Utah",
               `VT` = "Vermont",
               `VA`=  "Virginia",
               `WA` = "Washington",
               `WV` = "West Virginia",
               `WI` = "Wisconsin",
               `WY` = "Wyoming" )) %>% 
  filter(regioncode != "NA",
         regioncode != "MP",
         regioncode != "VI") %>% 
  select(regioncode, date = cdc_case_earliest_dt, cases)


write.csv(us_regions, "./dat/US_regions.csv", row.names = F)


#testing
# ggplot(data, aes(x = cdc_case_earliest_dt , y = cases)) +
#   geom_line() +
#   facet_wrap(~res_state) +
#   theme_bw()
# 
# cts <- us_regions %>%
#   mutate(id = 1:n(),
#          gr = floor(id/12) + 1)
# 
# 
# for(i in 1:max(cts$gr)){
#   cts_t <- 
#     cts %>% 
#     filter(gr == i) %>% 
#     dplyr::pull(regioncode)
#   
#   us_regions %>% 
#     filter(regioncode %in% cts_t) %>% 
#     ggplot()+
#     geom_point(aes(date, cases), size = 0.3)+
#     facet_wrap(~regioncode, scales = "free")+
#     theme(
#       axis.text.x = element_text(size = 5)
#     )
#   
#   ggsave(paste0("uschecks", i, ".png")) 
# }

