library(dplyr)
library(changepoint)
library(lubridate)
library(forecast)
data<- read.csv("D:\\MSc- Data science and analytics\\sem 3 (dissertation)\\mydata\\hourly_NO2_streets\\UCC_hourly_NO2.csv")

data<-data[!is.na(data$NO2),]
data$Timestamp<- as.POSIXct(data$Timestamp, format="%d-%m-%Y %H:%M", tz='utc')
# Convert to Irish time zone (Europe/Dublin)
data$Timestamp_Irish <- format(data$Timestamp, tz = "Europe/Dublin", usetz = TRUE)

# Optional: convert it back to POSIXct if needed for plotting or analysis
data$Timestamp_Irish <- as.POSIXct(data$Timestamp_Irish, tz = "Europe/Dublin")
data$Weekday<- weekdays(data$Timestamp_Irish, abbreviate = FALSE)

weekdays_only <- data[!((data$Weekday=="Saturday") | (data$Weekday=="Sunday")), ]

weekdays_only$date=as.Date(weekdays_only$Timestamp_Irish)

weekdays_only=weekdays_only[!((weekdays_only$date==as.Date('2024-02-05', format='%Y-%m-%d')) |
                                (weekdays_only$date==as.Date('2024-01-01', format='%Y-%m-%d')) |
                                (weekdays_only$date==as.Date('2024-03-17', format='%Y-%m-%d')) |
                                (weekdays_only$date==as.Date('2024-04-01', format='%Y-%m-%d')) |
                                (weekdays_only$date==as.Date('2024-05-06', format='%Y-%m-%d')) |
                                (weekdays_only$date==as.Date('2024-06-03', format='%Y-%m-%d')) |
                                (weekdays_only$date==as.Date('2024-08-05', format='%Y-%m-%d')) |
                                (weekdays_only$date==as.Date('2024-10-28', format='%Y-%m-%d')) |
                                (weekdays_only$date==as.Date('2024-12-25', format='%Y-%m-%d')) |
                                (weekdays_only$date==as.Date('2024-12-26', format='%Y-%m-%d')) |
                                (weekdays_only$date==as.Date('2023-01-01', format='%Y-%m-%d')) |
                                (weekdays_only$date==as.Date('2023-02-06', format='%Y-%m-%d')) |
                                (weekdays_only$date==as.Date('2023-03-17', format='%Y-%m-%d')) |
                                (weekdays_only$date==as.Date('2023-04-10', format='%Y-%m-%d')) |
                                (weekdays_only$date==as.Date('2023-05-01', format='%Y-%m-%d')) |
                                (weekdays_only$date==as.Date('2023-06-05', format='%Y-%m-%d')) |
                                (weekdays_only$date==as.Date('2023-08-07', format='%Y-%m-%d')) |
                                (weekdays_only$date==as.Date('2023-10-30', format='%Y-%m-%d')) |
                                (weekdays_only$date==as.Date('2023-12-25', format='%Y-%m-%d')) |
                                (weekdays_only$date==as.Date('2023-12-26', format='%Y-%m-%d')) |
                                (weekdays_only$date==as.Date('2022-01-01', format='%Y-%m-%d')) |
                                (weekdays_only$date==as.Date('2022-01-03', format='%Y-%m-%d')) |
                                (weekdays_only$date==as.Date('2022-03-17', format='%Y-%m-%d')) |
                                (weekdays_only$date==as.Date('2022-03-18', format='%Y-%m-%d')) |
                                (weekdays_only$date==as.Date('2022-04-18', format='%Y-%m-%d')) |
                                (weekdays_only$date==as.Date('2022-05-02', format='%Y-%m-%d')) |
                                (weekdays_only$date==as.Date('2022-06-06', format='%Y-%m-%d')) |
                                (weekdays_only$date==as.Date('2022-08-01', format='%Y-%m-%d')) |
                                (weekdays_only$date==as.Date('2022-10-31', format='%Y-%m-%d')) |
                                (weekdays_only$date==as.Date('2022-12-25', format='%Y-%m-%d')) |
                                (weekdays_only$date==as.Date('2022-12-26', format='%Y-%m-%d')) 
), ]
weekdays_only <- weekdays_only[weekdays_only$date>as.Date('2021-12-31', format="%Y-%m-%d"), ]


weekdays_only$hour <- hour(weekdays_only$Timestamp_Irish)


##############################################################################################################################

# DIVIDING DATASET ACCORDING TO DIFFERENT SEASONS #

summers_start_2022=as.Date('2022-06-01',format='%Y-%m-%d')
summers_end_2022=as.Date('2022-08-31',format='%Y-%m-%d')

summers_start_2023=as.Date('2023-06-01',format='%Y-%m-%d')
summers_end_2023=as.Date('2023-08-31',format='%Y-%m-%d')

summers_start_2024=as.Date('2024-06-01',format='%Y-%m-%d')
summers_end_2024=as.Date('2024-08-31',format='%Y-%m-%d')

######## winter ###########
winters_start_2022=as.Date('2022-12-01',format='%Y-%m-%d')
winters_end_2022=as.Date('2023-02-28',format='%Y-%m-%d')

winters_start_2023=as.Date('2023-12-01',format='%Y-%m-%d')
winters_end_2023=as.Date('2024-02-29',format='%Y-%m-%d')

winters_start_2024=as.Date('2024-12-01',format='%Y-%m-%d')
winters_end_2024=as.Date('2024-12-31',format='%Y-%m-%d')

winters_2022_2_start=as.Date('2022-01-01',format='%Y-%m-%d')
winters_2022_2_end=as.Date('2022-02-28',format='%Y-%m-%d')


########## autumn ############
autumn_start_2022=as.Date('2022-09-01',format='%Y-%m-%d')
autumn_end_2022=as.Date('2022-11-30',format='%Y-%m-%d')

autumn_start_2023=as.Date('2023-09-01',format='%Y-%m-%d')
autumn_end_2023=as.Date('2023-11-30',format='%Y-%m-%d')

autumn_start_2024=as.Date('2024-09-01',format='%Y-%m-%d')
autumn_end_2024=as.Date('2024-11-30',format='%Y-%m-%d')

############# spring ##############
spring_start_2022=as.Date('2022-03-01',format='%Y-%m-%d')
spring_end_2022=as.Date('2022-05-31',format='%Y-%m-%d')

spring_start_2023=as.Date('2023-03-01',format='%Y-%m-%d')
spring_end_2023=as.Date('2023-05-31',format='%Y-%m-%d')

spring_start_2024=as.Date('2024-03-01',format='%Y-%m-%d')
spring_end_2024=as.Date('2024-05-31',format='%Y-%m-%d')


summer_months=weekdays_only[
  (((weekdays_only$date>=summers_start_2022) & (weekdays_only$date<=summers_end_2022)) |
     ((weekdays_only$date>=summers_start_2023) & (weekdays_only$date<=summers_end_2023)) |
     ((weekdays_only$date>=summers_start_2024) & (weekdays_only$date<=summers_end_2024))) &
    (weekdays_only$date>=as.Date("2021-12-31",format="%Y-%m-%d")),
]

winter_months=weekdays_only[
  (((weekdays_only$date>=winters_start_2022) & (weekdays_only$date<=winters_end_2022)) |
     ((weekdays_only$date>=winters_start_2023) & (weekdays_only$date<=winters_end_2023)) |
     ((weekdays_only$date>=winters_start_2024) & (weekdays_only$date<=winters_end_2024)) |
     ((weekdays_only$date>=winters_2022_2_start) & (weekdays_only$date<=winters_2022_2_end))) &
    (weekdays_only$date>=as.Date("2021-12-31", format= "%Y-%m-%d")),
]

autumn_months=weekdays_only[
  (((weekdays_only$date>=autumn_start_2022) & (weekdays_only$date<=autumn_end_2022)) |
     ((weekdays_only$date>=autumn_start_2023) & (weekdays_only$date<=autumn_end_2023)) |
     ((weekdays_only$date>=autumn_start_2024) & (weekdays_only$date<=autumn_end_2024))) &
    (weekdays_only$date>=as.Date("2021-12-31", format="%Y-%m-%d")),
]

spring_months=weekdays_only[
  (((weekdays_only$date>=spring_start_2022) & (weekdays_only$date<=spring_end_2022)) |
     ((weekdays_only$date>=spring_start_2023) & (weekdays_only$date<=spring_end_2023)) |
     ((weekdays_only$date>=spring_start_2024) & (weekdays_only$date<=spring_end_2024))) &
    (weekdays_only$date>=as.Date("2021-12-31", format="%Y-%m-%d")),
]

summer_months['Season']='Summers'
winter_months['Season']='Winters'
autumn_months['Season']='Autumn'
spring_months['Season']='Spring'

comb_df <- rbind(summer_months, winter_months, autumn_months, spring_months)

comb_df$Timestamp <- as.POSIXct(comb_df$Timestamp, format = "%Y-%m-%d %H:%M:%S")

# Sort the data frame by Timestamp
comb_df_sorted <- comb_df[order(comb_df$Timestamp), ]

