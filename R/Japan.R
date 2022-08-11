#japan
#source: https://www3.nhk.or.jp/news/special/coronavirus/data/

#data <- read.csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv")

data <- read.csv("https://www3.nhk.or.jp/n-data/opendata/coronavirus/nhk_news_covid19_prefectures_daily_data.csv", encoding="UTF-8")
names(data)[1] <- "date"
names(data)[3] <- "regionname"
names(data)[4] <- "cases"
data <- data %>% 
  select(date, regionname, cases) %>% 
  mutate(regionname = case_when(
    regionname == "三重県" ~ "Mie",
    regionname == "京都府" ~ "Kyoto",
    regionname == "佐賀県" ~ "Saga",
    regionname == "兵庫県" ~ "Hyogo",
    regionname == "北海道" ~ "Hokkaido",
    regionname == "千葉県" ~ "Chiba",
    regionname == "大阪府" ~ "Osaka",
    regionname == "奈良県" ~ "Nara",
    regionname == "宮城県" ~ "Miyagi",
    regionname == "宮崎県" ~ "Miyazaki",
    regionname == "富山県" ~ "Toyama",
    regionname == "山口県" ~ "Yamaguchi",
    regionname == "山形県" ~ "Yamagata",
    regionname == "山梨県" ~ "Yamanashi",
    regionname == "岐阜県" ~ "Gifu",
    regionname == "岡山県" ~ "Okayama",
    regionname == "岩手県" ~ "Iwate",
    regionname == "島根県" ~ "Shimane",
    regionname == "広島県" ~ "Hiroshima",
    regionname == "徳島県" ~ "Tokushima",
    regionname == "愛媛県" ~ "Ehime",
    regionname == "愛知県" ~ "Aichi",
    regionname == "新潟県" ~ "Niigata",
    regionname == "東京都" ~ "Tokyo",
    regionname == "栃木県" ~ "Tochigi",
    regionname == "沖縄県" ~ "Okinawa",
    regionname == "滋賀県" ~ "Shiga",
    regionname == "熊本県" ~ "Kumamoto",
    regionname == "石川県" ~ "Ishikawa",
    regionname == "神奈川県" ~ "Kanagawa",
    regionname == "福井県" ~ "Fukui",
    regionname == "福岡県" ~ "Fukuoka",
    regionname == "福島県" ~ "Fukushima",
    regionname == "秋田県" ~ "Akita",
    regionname == "群馬県" ~ "Gunma",
    regionname == "茨城県" ~ "Ibaraki",
    regionname == "長崎県" ~ "Nagasaki",
    regionname == "長野県" ~ "Nagano",
    regionname == "青森県" ~ "Aomori",
    regionname == "静岡県" ~ "Shizuoka",
    regionname == "香川県" ~ "Kagawa",
    regionname == "高知県" ~ "Kochi",
    regionname == "鳥取県" ~ "Tottori",
    regionname == "鹿児島県" ~ "Kagoshima",
    regionname == "和歌山県" ~ "Wakayama",
    regionname == "埼玉県" ~ "Saitama",
    regionname == "大分県" ~ "Oita",
    TRUE ~ regionname
 ),
 date = as.Date(date, "%Y/%m/%d"))

write.csv(data, "./dat/Japan_regions.csv", row.names = F)



tokyo <- read.csv("https://stopcovid19.metro.tokyo.lg.jp/data/130001_tokyo_covid19_positive_cases_by_day_of_symptom_onset.csv", encoding="UTF-8")
