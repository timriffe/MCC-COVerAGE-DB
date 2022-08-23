# Load the package required to read JSON files.
library("rjson")

# Give the input file name to the function.
result <- fromJSON(file = "./data_input/romania.json")


library(jsonlite)

df <- stream_in(file("./data_input/romania.json"))

install.packages("RJSONIO")
library(RJSONIO)
t2 <- RJSONIO::fromJSON("./data_input/romania.json")


data_covid19_msis_by_time_location_latest.csv


db <- read_csv("https://raw.githubusercontent.com/folkehelseinstituttet/surveillance_data/master/covid19/data_covid19_msis_by_time_location_latest.csv")
