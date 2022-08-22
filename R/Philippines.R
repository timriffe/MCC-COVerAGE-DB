
library(tidyverse)
library(googlesheets4)
library(googledrive)
library(lubridate)
library(readr)
library(rvest)
library(longurl)
library(pdftools)

bit.ly_url <- "bit.ly/DataDropPH"

drive_readme_url <- longurl::expand_urls(bit.ly_url)

drive_id <- drive_readme_url$expanded_url %>% 
  gsub(pattern = "https://drive.google.com/drive/folders/",
       replacement = "")%>% 
  gsub(pattern = "?usp=sharing",
       replacement = "")%>% 
  gsub(pattern = "\\?",
       replacement = "")%>%
  googledrive::as_id()%>% 
  drive_ls() %>% 
  filter(grepl(name,pattern = "READ ME FIRST"))


#select one with most recent date

drive_id_unique= drive_id%>% 
  mutate(date=stringr::str_extract(string = name,
                                   pattern = "(?<=\\().*(?=\\))"))%>%
  mutate(Date= paste0(date,"_",year(today())))%>%
  mutate(Date = mdy(Date))%>% 
  filter(Date==max(Date))

#read data 

drive_id_shorter= drive_id_unique%>% 
  drive_download(path = "Data/PH_README.pdf",
                 overwrite=TRUE)


# read as text ()
PDF_TEXT <- pdf_text("Data/PH_README.pdf")
PAGE     <- PDF_TEXT[grepl(PDF_TEXT,pattern = "bit.ly/")] 
LINES <- capture.output(cat(PAGE)) %>% 
  gsub(pattern =" ", replacement = "") %>% 
  gsub(pattern = "\r", replacement = "")
ind <- which(grepl(LINES, pattern = "LinktoDOHDataDrop"))+ 1

folder_bitly_url <- LINES[ind]

drive_folder_url <- longurl::expand_urls(folder_bitly_url)


# Drive info for all folder contents
drive_contents <-
  drive_folder_url$expanded_url %>% 
  gsub(pattern = "https://drive.google.com/drive/folders/",
       replacement = "") %>% 
  gsub(pattern = "?usp=sharing",
       replacement = "") %>% 
  gsub(pattern = "\\?",
       replacement = "")%>%
  googledrive::as_id() %>% 
  drive_ls() 

# Drive info for Case file part 1
case_url <-
  drive_contents %>% 
  filter(grepl(name, pattern="04 Case Information_batch_0.csv"))

# Download Cases part 1 == from 30.01.2020 to 26.04.2021
case_url %>% 
  drive_download(path = "Data/PH_Cases1.csv",
                 overwrite = TRUE)

IN1 <- read_csv("Data/PH_Cases1.csv",
                col_types = "ccccDDDDDccccccccccDcc")

# Drive info for Case file part 2 == from 26.04.2021- 01.09.2021
case_url <-
  drive_contents %>% 
  filter(grepl(name, pattern="04 Case Information_batch_1.csv"))

# Download Cases part 2
case_url %>% 
  drive_download(path = "Data/PH_Cases2.csv",
                 overwrite = TRUE)

IN2 <- read_csv("Data/PH_Cases2.csv",
                col_types = "ccccDDDDDccccccccccDcc")

# Drive info for Case file part 3 == from 01.09.2021 to 11.01.2022
case_url <-
  drive_contents %>% 
  filter(grepl(name, pattern="04 Case Information_batch_2.csv"))

# Download Cases part 3
case_url %>% 
  drive_download(path = "Data/PH_Cases3.csv",
                 overwrite = TRUE)

IN3 <- read_csv("Data/PH_Cases3.csv",
                col_types = "ccccDDDDDccccccccccDcc")

case_url <-
  drive_contents %>% 
  filter(grepl(name, pattern="04 Case Information_batch_3.csv"))

# Download Cases part 4
case_url %>% 
  drive_download(path = "Data/PH_Cases4.csv",
                 overwrite = TRUE)

IN4 <- read_csv("Data/PH_Cases4.csv",
                col_types = "ccccDDDDDccccccccccDcc")


IN <- rbind(IN1, IN2, IN3, IN4)

remove(IN1, IN2, IN3, IN4, case_url, drive_id_unique, drive_contents, drive_id, drive_id_shorter, drive_readme_url)

IN <- IN %>% 
filter( CityMunRes == "CEBU CITY (CAPITAL)" |  CityMunRes == "DAVAO CITY" |  CityMunRes == "CITY OF MANILA" |  CityMunRes == "QUEZON CITY") 

IN2 <- IN %>% 
  select(date = DateRepConf, mcccityname1 = CityMunRes) %>% 
  group_by(date, mcccityname1) %>% 
  summarise(cases = n()) %>% 
  mutate(date = as.Date(date),
         mcccityname1 = case_when(
           mcccityname1 == "CEBU CITY (CAPITAL)" ~ "Cebu",
           mcccityname1 == "CITY OF MANILA" ~ "Davao",
           mcccityname1 == "DAVAO CITY" ~ "Manila",
           mcccityname1 == "QUEZON CITY" ~ "Quezon")) 


ggplot(IN2, aes(x = date , y = cases)) +
  geom_line() +
  facet_wrap(~mcccityname1) +
  theme_bw()

write.csv(IN2, "./dat/Philippines_cities.csv", row.names = F)

IN3 <- IN %>% 
  select(date = DateRepConf, regionname = CityMunRes) %>% 
  group_by(date, regionname) %>% 
  summarise(cases = n()) %>% 
  mutate(date = as.Date(date),
         regionname = case_when(
           regionname == "CEBU CITY (CAPITAL)" ~ "Cebu",
           regionname == "CITY OF MANILA" ~ "Davao",
           regionname == "DAVAO CITY" ~ "Manila",
           regionname == "QUEZON CITY" ~ "Quezon")) 
write.csv(IN3, "./dat/Philippines_regions.csv", row.names = F)

