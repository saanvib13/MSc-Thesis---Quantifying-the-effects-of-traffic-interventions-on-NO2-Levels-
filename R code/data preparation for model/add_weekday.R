library(dplyr)
library(lubridate)
dd <-read.csv("D:\\MSc- Data science and analytics\\sem 3 (dissertation)\\mydata\\datasets_for_model\\cleaned\\UCC_23_24_cleaned.csv")
dd$date <- as.Date(dd$date, tz='Europe/Dublin', format = '%d-%m-%Y')
dd <- dd %>%
  mutate(weekday_iso = as.integer(format(date, "%u"))) 
head(dd)
write.csv(dd,"D:\\MSc- Data science and analytics\\sem 3 (dissertation)\\mydata\\datasets_for_model\\cleaned\\UCC_23_24_cleaned.csv")



