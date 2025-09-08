library(dplyr)
library(lubridate)
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ NO2 SENSORS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

df1<-read.csv("D:\\MSc- Data science and analytics\\sem 3 (dissertation)\\mydata\\UCC_NO2(in).csv")
df2<-read.csv("D:\\MSc- Data science and analytics\\sem 3 (dissertation)\\mydata\\LG_NO2(in).csv")
df3<-read.csv("D:\\MSc- Data science and analytics\\sem 3 (dissertation)\\mydata\\St_Patricks_2min_FullDataSet.csv")
df4<- read.csv("D:\\MSc- Data science and analytics\\sem 3 (dissertation)\\mydata\\Oliver_Plunkett_FullDataSet.csv")
df5<-read.csv("D:\\MSc- Data science and analytics\\sem 3 (dissertation)\\mydata\\Beasley_Street_FullDataSet.csv")
df6<-read.csv("D:\\MSc- Data science and analytics\\sem 3 (dissertation)\\mydata\\Grand_Parade_FullDataSet.csv")

######################################################################################################################
df1 <- df1[,c('date','NO2')]
df1 <- df1 %>%
  rename(Timestamp=date)

# df1$Timestamp <- as.POSIXct(df1$Timestamp, format = "%d-%m-%Y %H:%M", tz = "UTC")
# df1$Timestamp_Irish <- as.POSIXct(format(df1$Timestamp, tz = "Europe/Dublin", usetz = TRUE), tz = "Europe/Dublin")

df1$Timestamp <- parse_date_time(df1$Timestamp, orders = "dmy HM", tz = "Europe/Dublin")
head(df1)
df1
######################################################################################################################
df2 <- df2[,c('date','NO2')]
df2 <- df2 %>%
  rename(Timestamp=date)

# df2$Timestamp <- as.POSIXct(df2$Timestamp, format = "%d-%m-%Y %H:%M", tz = "UTC")
# df2$Timestamp <- as.POSIXct(format(df2$Timestamp, tz = "Europe/Dublin", usetz = TRUE), tz = "Europe/Dublin")
df2$Timestamp <- parse_date_time(df2$Timestamp, orders = "dmy HM", tz = "Europe/Dublin")
df2
######################################################################################################################
df3[(is.na(df3$NO2)),]
head(df3)
# df3$Timestamp <- as.POSIXct(df3$Timestamp, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
# df3$Timestamp <- as.POSIXct(format(df3$Timestamp, tz = "Europe/Dublin", usetz = TRUE), tz = "Europe/Dublin")
df3$Timestamp <- parse_date_time(df3$Timestamp, orders = "ymd HMS", tz = "Europe/Dublin")
df3[is.na(df3$Timestamp),]

df3_hourly <- df3 %>%
  mutate(Hour = floor_date(Timestamp, unit = "hour")) %>%
  group_by(Hour) %>%
  summarise(NO2 = mean(NO2, na.rm = TRUE)) %>%
  ungroup()
head(df3_hourly)
df3 <- as.data.frame(df3_hourly)
head(df3)

df3[is.na(df3$Hour),]
######################################################################################################################
df4[(df4$NO2==""),]
df4[(is.na(df4$Timestamp)),]
df4[(is.na(df4$NO2)),]

# df4$Timestamp <- as.POSIXct(df4$Timestamp, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
# df4$Timestamp <- as.POSIXct(format(df4$Timestamp, tz = "Europe/Dublin", usetz = TRUE), tz = "Europe/Dublin")

df4$Timestamp <- parse_date_time(df4$Timestamp, orders = "ymd HMS", tz = "Europe/Dublin")
df4[is.na(df4$Timestamp),]
df4_hourly <- df4 %>%
  mutate(Hour = floor_date(Timestamp, unit = "hour")) %>%
  group_by(Hour) %>%
  summarise(NO2 = mean(NO2, na.rm = TRUE)) %>%
  ungroup()

df4 <- as.data.frame(df4_hourly)

df4
######################################################################################################################
df5[(is.na(df5$NO2)),]
df5[(is.na(df5$Timestamp)),]
# df5$Timestamp <- as.POSIXct(df5$Timestamp, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
# df5$Timestamp <- as.POSIXct(format(df5$Timestamp, tz = "Europe/Dublin", usetz = TRUE), tz = "Europe/Dublin")

df5$Timestamp <- parse_date_time(df5$Timestamp, orders = "ymd HMS", tz = "Europe/Dublin")
df5_hourly <- df5 %>%
  mutate(Hour = floor_date(Timestamp, unit = "hour")) %>%
  group_by(Hour) %>%
  summarise(NO2 = mean(NO2, na.rm = TRUE)) %>%
  ungroup()

df5 <- as.data.frame(df5_hourly)

df5
######################################################################################################################
df6[(is.na(df6$NO2)),]
df6[(is.na(df6$Timestamp)),]

# df6$Timestamp <- as.POSIXct(df6$Timestamp, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
# df6$Timestamp <- as.POSIXct(format(df6$Timestamp, tz = "Europe/Dublin", usetz = TRUE), tz = "Europe/Dublin")

df6$Timestamp <- parse_date_time(df6$Timestamp, orders = "ymd HMS", tz = "Europe/Dublin")
df6_hourly <- df6 %>%
  mutate(Hour = floor_date(Timestamp, unit = "hour")) %>%
  group_by(Hour) %>%
  summarise(NO2 = mean(NO2, na.rm = TRUE)) %>%
  ungroup()

df6 <- as.data.frame(df6_hourly)

head(df6)
######################################################################################################################
# df1$date <- as.Date(df1$Timestamp)
df1$date <- as.Date(df1$Timestamp, tz = "Europe/Dublin")
df1 <- df1 %>% mutate(hour=hour(df1$Timestamp))
df1 <- df1[((df1$date>=as.Date('2023-01-01')) & (df1$date<=as.Date('2024-12-31'))),]

df1[df1$date==as.Date('2024-03-31'),]

# df1[!((df1$date==as.Date('2024-03-31')) & (df1$hour==1)),]
df1[(is.na(df1$Timestamp)),]
df1<- df1[!(is.na(df1$Timestamp)),]
head(df1)
tail(df1)

nrow(df1)

df1$NO2[(df1$NO2<0)] <- NA

######################################################################################################################
# df2$date <- as.Date(df2$Timestamp)
df2$date <- as.Date(df2$Timestamp, tz = "Europe/Dublin")
df2 <- df2 %>% mutate(hour=hour(df2$Timestamp))
df2 <- df2[((df2$date>=as.Date('2023-01-01')) & (df2$date<=as.Date('2024-12-31'))),]

df2[df2$date==as.Date('2024-03-31'),]
df2<- df2[!(is.na(df2$Timestamp)),]
head(df2)
tail(df2)
df2
nrow(df2)


######################################################################################################################
df3 <- df3 %>%
  rename(Timestamp=Hour)
head(df3)
# df3$date <- as.Date(df3$Timestamp)
df3$date <- as.Date(df3$Timestamp, tz = "Europe/Dublin")
head(df3)
df3[(df3$date==as.Date('2024-03-31')) | (df3$date==as.Date('2024-03-30')) | (df3$date==as.Date('2024-04-01')),]

df3[is.na(df3$date),]

df3 <- df3 %>% mutate(hour=hour(df3$Timestamp))
df3 <- df3[((df3$date>=as.Date('2023-08-06')) & (df3$date<=as.Date('2024-06-30'))),]

df3[df3$date==as.Date('2023-10-27'),]

df3[(is.na(df3$Timestamp)),]
head(df3)
tail(df3)
df3
nrow(df3)


######################################################################################################################
df4 <- df4 %>%
  rename(Timestamp=Hour)
# df4$date <- as.Date(df4$Timestamp)
df4$date <- as.Date(df4$Timestamp, tz = "Europe/Dublin")
df4 <- df4 %>% mutate(hour=hour(df4$Timestamp))
df4 <- df4[((df4$date>=as.Date('2023-08-06')) & (df4$date<=as.Date('2024-06-30'))),]
head(df4)
df4[(df4$date==as.Date('2024-03-31')) | (df4$date==as.Date('2024-03-30')) | (df4$date==as.Date('2024-04-01')),]
head(df4)
tail(df4)
df4
nrow(df4)

######################################################################################################################
df5 <- df5 %>%
  rename(Timestamp=Hour)
# df5$date <- as.Date(df5$Timestamp)
df5$date <- as.Date(df5$Timestamp, tz = "Europe/Dublin")
df5 <- df5 %>% mutate(hour=hour(df5$Timestamp))
df5[((df5$date==as.Date('2024-03-31')) | (df5$date==as.Date('2024-03-30')) | (df5$date==as.Date('2024-04-01'))),]
df5
df5 <- df5[((df5$date>=as.Date('2023-08-06')) & (df5$date<=as.Date('2024-06-30'))),]

df5[df5$date==as.Date('2024-03-31'),]
head(df5)
tail(df5)
df5
nrow(df5)

######################################################################################################################
df6 <- df6 %>%
  rename(Timestamp=Hour)
# df6$date <- as.Date(df6$Timestamp)
df6$date <- as.Date(df6$Timestamp, tz = "Europe/Dublin")
df6 <- df6 %>% mutate(hour=hour(df6$Timestamp))
df6 <- df6[((df6$date>=as.Date('2023-08-06')) & (df6$date<=as.Date('2024-06-30'))),]
df6[((df6$date==as.Date('2024-03-31')) | (df6$date==as.Date('2024-03-30')) | (df6$date==as.Date('2024-04-01'))),]
head(df6)
tail(df6)
df6
nrow(df6)
###################################################################################

head(df1)
head(df1[19:nrow(df1),])
tail(df1[19:nrow(df1),])

df1 <- df1[19:nrow(df1),]
nrow(df1)
###################################################################################

head(df2)
head(df2[19:nrow(df2),])
tail(df2[19:nrow(df2),])

df2 <- df2[19:nrow(df2),]
nrow(df2)
###################################################################################

head(df3)
head(df3[19:nrow(df3),])
tail(df3[19:nrow(df3),])
df3
df3 <- df3[19:nrow(df3),]

###################################################################################

head(df4)
head(df4[3:nrow(df4),])
tail(df4[3:nrow(df4),])

df4 <- df4[3:nrow(df4),]

###################################################################################

head(df5)
tail(df5)

###################################################################################

head(df6)
head(df6[19:nrow(df6),])
tail(df6[19:nrow(df6),])

df6 <- df6[19:nrow(df6),]
###################################################################################
nrow(df1)
nrow(df2)
nrow(df3)
nrow(df4)
nrow(df5)
nrow(df6)
# head(df5)
###################################################################################
# ! PROBLEM !
# df1 and df2 contain nan values
nrow(df1[(df1$NO2=='#N/A'),])
nrow(df2[(df2$NO2=='#N/A'),])

df4[df4$NO2=="",]
df4[is.na(df4$NO2),]
df4[df4$NO2=="NA",]
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ~~~~~~~~~~~~~~~~~ ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ WEATHER DATAFRAME ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
wdf <- read.csv("D:\\MSc- Data science and analytics\\sem 3 (dissertation)\\mydata\\hourly_cork_airport_since_2022.csv")
wdf <- wdf %>%
  rename(Timestamp=date)
wdf[(is.na(wdf$Timestamp)),]
wdf[(wdf$Timestamp=='#N/A'),]
head(wdf)
nrow(wdf)
wdf$Timestamp <- as.POSIXct(wdf$Timestamp, format = "%d-%m-%Y %H:%M", tz = "UTC")
wdf$Timestamp <- as.POSIXct(format(wdf$Timestamp, tz = "Europe/Dublin", usetz = TRUE), tz = "Europe/Dublin")
wdf
wdf$date <- as.Date(wdf$Timestamp, tz = "Europe/Dublin")

wdf[(wdf$date==as.Date('2024-03-31')) | (wdf$date==as.Date('2024-03-30')) | (wdf$date==as.Date('2024-04-01')),]

wdf <- wdf[((wdf$date>=as.Date('2023-01-01')) & (wdf$date<=as.Date('2024-12-31'))),]

wdf[wdf$date==as.Date('2024-03-31'),]

head(wdf)
tail(wdf)
nrow(wdf)

head(wdf[19:nrow(wdf),])
tail(wdf[19:nrow(wdf),])
wdf <- wdf[19:nrow(wdf),]

nrow(wdf)
wdf[is.na(wdf$Timestamp),]
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ~~~~~~~~~~~~~~~~~ ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ TRAFFIC DATAFRAME ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
tdf <- read.csv("D:\\MSc- Data science and analytics\\sem 3 (dissertation)\\mydata\\traffic\\combined_traffic_ct_1_2_2023-24.csv")
head(tdf)
tdf <- tdf %>%
  rename(Timestamp=datetime)

tdf[(is.na(tdf$Timestamp)),]
tdf[tdf$Timestamp=='#N/A',]
tdf[tdf$Timestamp=='',]
head(tdf)
tdf$Timestamp[tdf$X==2016]
as.POSIXct(tdf$Timestamp[tdf$X==2016], format='%Y-%m-%d %H:%M:%S')

tdf$Timestamp[tdf$X==2018]
as.POSIXct(tdf$Timestamp[tdf$X==2018], format='%Y-%m-%d %H:%M:%S')

tdf$Timestamp[tdf$X==2017]
as.POSIXct(tdf$Timestamp[tdf$X==2017], format='%Y-%m-%d %H:%M:%S')

tdf$Timestamp[tdf$X==10921]

as.POSIXct(tdf$Timestamp[tdf$X==10921], format='%Y-%m-%d %H:%M:%S')

tdf$Timestamp_Irish <- as.POSIXct(tdf$Timestamp, format='%Y-%m-%d %H:%M:%S', tz = "Europe/Dublin")
tdf$date <- as.Date(tdf$Timestamp, tz = "Europe/Dublin")
head(tdf)
tdf
# tdf$date <- as.Date(tdf$Timestamp)

tdf[(is.na(tdf$Timestamp_Irish)),]

tdf <- tdf[(!is.na(tdf$Timestamp)),]


tdf <- tdf[((tdf$date>=as.Date('2023-08-06')) & (tdf$date<=as.Date('2024-06-30'))),]
tdf
head(tdf)
tail(tdf)
tdf[(tdf$date==as.Date('2023-10-29'))| (tdf$date==as.Date('2023-10-30')),]
tdf
head(tdf[19:nrow(tdf),])
tail(tdf[19:nrow(tdf),])
tdf <- tdf[19:nrow(tdf),]

nrow(tdf)


########################################################################################

#checking negative values for no2
df1$NO2[((df1$NO2<0) & (df1$NO2!='#N/A'))] = 0
df1[((df1$NO2<0) & (df1$NO2!='#N/A')),]

df2$NO2[df2$NO2==""]<- NA
df2$NO2[df2$NO2<0]<- NA

df2$NO2[((df2$NO2<0) & (df2$NO2!='#N/A'))] = 0
df2[((df2$NO2<0) & (df2$NO2!='#N/A')),]

df3[((df3$NO2<0) & (df3$NO2!='#N/A')),]

df4[((df4$NO2<0) & (df4$NO2!='#N/A')),]

df5[((df5$NO2<0)),]

df6[((df6$NO2<0)),]

###########################################################################################

write.csv(df1, "D:\\MSc- Data science and analytics\\sem 3 (dissertation)\\mydata\\datasets_for_model\\UCC.csv", row.names = FALSE)
write.csv(df2, "D:\\MSc- Data science and analytics\\sem 3 (dissertation)\\mydata\\datasets_for_model\\LG.csv", row.names = FALSE)
write.csv(df3, "D:\\MSc- Data science and analytics\\sem 3 (dissertation)\\mydata\\datasets_for_model\\SPS.csv", row.names = FALSE)
write.csv(df4, "D:\\MSc- Data science and analytics\\sem 3 (dissertation)\\mydata\\datasets_for_model\\OP.csv", row.names = FALSE)
write.csv(df5, "D:\\MSc- Data science and analytics\\sem 3 (dissertation)\\mydata\\datasets_for_model\\BS.csv", row.names = FALSE)
write.csv(df6, "D:\\MSc- Data science and analytics\\sem 3 (dissertation)\\mydata\\datasets_for_model\\GP.csv", row.names = FALSE)
write.csv(wdf, "D:\\MSc- Data science and analytics\\sem 3 (dissertation)\\mydata\\datasets_for_model\\weather_23_24.csv", row.names = FALSE)
write.csv(tdf, "D:\\MSc- Data science and analytics\\sem 3 (dissertation)\\mydata\\datasets_for_model\\traffic.csv", row.names = FALSE)

wdf=read.csv("D:\\MSc- Data science and analytics\\sem 3 (dissertation)\\mydata\\datasets_for_model\\weather_23_24.csv")
head(wdf)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~` ~~~~~~~~~~~~~~~~~` ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# merging datasets
full_timestamps <- data.frame(
  Timestamp = seq(
    from = as.POSIXct("2023-01-01 00:00:00", tz = "Europe/Dublin"),
    to   = as.POSIXct("2024-12-31 23:00:00", tz = "Europe/Dublin"),
    by = "1 hour"
  )
)

df1$original_Timestamp <- df1$Timestamp
df1_full <- full_timestamps %>%
  left_join(df1, by = "Timestamp")

df1_full[is.na(df1_full$original_Timestamp),]
df1_full


# # full_timestamps$date <- as.Date(full_timestamps$Timestamp, tz="Europe/Dublin")
# head(full_timestamps)
# # full_timestamps[full_timestamps$date==as.Date('2024-03-31'),]
# full_timestamps <- full_timestamps[-2000,]
# # full_timestamps<-full_timestamps[,]
# 
# full_timestamps <- as.data.frame(full_timestamps)
# full_timestamps[2000,]
# # full_timestamps=as.data.frame(full_timestamps[,c('Timestamp')])
# library(dplyr)
# head(full_timestamps)
# 
# full_timestamps <- full_timestamps %>%
#   rename(Timestamp=full_timestamps)
# # UCC
# df1[2000,]
# df1$original_Timestamp <- df1$Timestamp
# 
# df1_full <- full_timestamps %>%
#   left_join(df1, by = "Timestamp")
# 
# df1_full[is.na(df1_full$original_Timestamp),]
# df1_full[2000,]
####################################
# LG
df2$original_Timestamp <- df2$Timestamp
df2_full <- full_timestamps %>%
  left_join(df2, by = "Timestamp")
df2_full[is.na(df2_full$original_Timestamp),]
df2_full

####################################
# sps
df3$original_Timestamp <- df3$Timestamp
df3_full <- full_timestamps %>%
  left_join(df3, by = "Timestamp")

df3_full[is.na(df3_full$original_Timestamp),]
df3_full

####################################
# op
df4$original_Timestamp <- df4$Timestamp
df4_full <- full_timestamps %>%
  left_join(df4, by = "Timestamp")

df4_full[is.na(df4_full$original_Timestamp),]
df4_full


####################################
# bs
df5$original_Timestamp <- df5$Timestamp
df5_full <- full_timestamps %>%
  left_join(df5, by = "Timestamp")

missing<-df5_full[is.na(df5_full$original_Timestamp),]
tail(missing,133)


####################################
# gp
df6$original_Timestamp <- df6$Timestamp
df6_full <- full_timestamps %>%
  left_join(df6, by = "Timestamp")
df6_full[is.na(df6_full$original_Timestamp),]
missing<-df6_full[is.na(df6_full$original_Timestamp),]

####################################
# weather
head(wdf)
wdf$original_Timestamp <- wdf$Timestamp
# wdf_full <- full_timestamps %>%
#   left_join(wdf, by = "Timestamp")
# wdf_full[is.na(wdf_full$original_Timestamp),]
wdf

# Parse to POSIXct in local time (Europe/Dublin), handling DST
wdf$Timestamp <- parse_date_time(wdf$Timestamp, orders = "ymd HMS", tz = "Europe/Dublin")
head(tdf)
t_w_df <- tdf %>%
    left_join(wdf, by = "Timestamp")
head(t_w_df)
nrow(t_w_df)
####################################
# traffic
tdf$original_Timestamp <- tdf$Timestamp
tdf_full <- full_timestamps %>%
  left_join(tdf, by = "Timestamp")
tdf_full[is.na(tdf_full$original_Timestamp),]

tdf_full$original_Timestamp[2000]<- tdf_full$Timestamp[2000]
tdf_full$date[2000]<- as.Date('2023-10-29')
tdf_full$counter.1[2000]<- 335
tdf_full$counter.2[2000]<- 890
tdf_full[2000,]
full_timestamps[1950:2050,]


##############################
# weather_traffic_combo
w_t_df <- wdf_full %>%
  left_join(tdf_full,by='Timestamp')
w_t_df<- w_t_df[ , !(names(w_t_df) %in% c("original_Timestamp.y", "date.y","X")) ]
head(w_t_df)
nrow(w_t_df)

#################################################################################

df1_final <- w_t_df %>% left_join(df1_full,by='Timestamp')
df1_final<- df1_final[ , !(names(df1_final) %in% c("original_Timestamp.x", "date.x","X")) ]

head(df1)
df1[df1$date=='2024-03-31',]
# df3$original_ts<- df3$Timestamp
# df3['Timestamp'] = parse_date_time(df3$Timestamp, orders = "ymd HMS", tz = "Europe/Dublin")
write.csv(df1, "D:\\MSc- Data science and analytics\\sem 3 (dissertation)\\mydata\\datasets_for_model\\UCC_23_24_for_correction.csv")

df1=read.csv("D:\\MSc- Data science and analytics\\sem 3 (dissertation)\\mydata\\datasets_for_model\\UCC_23_24_corrected.csv")
df1_final <- t_w_df %>% left_join(df1,by='Timestamp')
df1_final<- df1_final[ , !(names(df1_final) %in% c("Unnamed..0", "date.x","X.x","X.y","X",'date.y','Timestamp_Irish')) ]
df1_final[is.na(df1_final$date),]

df1_final[df1_final$date=="",]
as.Date(df1_final$Timestamp)
df1_final$date <- as.Date(df1_final$Timestamp)
head(df1)
head(df1_final)
nrow(df1_final)




# df2_final <- w_t_df %>% left_join(df2_full,by='Timestamp')
head(df2)
df2[df2$date=='2024-03-31',]
# df3$original_ts<- df3$Timestamp
# df3['Timestamp'] = parse_date_time(df3$Timestamp, orders = "ymd HMS", tz = "Europe/Dublin")
write.csv(df2, "D:\\MSc- Data science and analytics\\sem 3 (dissertation)\\mydata\\datasets_for_model\\LG_23_24_for_correction.csv")

df2=read.csv("D:\\MSc- Data science and analytics\\sem 3 (dissertation)\\mydata\\datasets_for_model\\LG_23_24_corrected.csv")
head(df2)
df2_final <- t_w_df %>% left_join(df2,by='Timestamp')
df2_final<- df2_final[ , !(names(df2_final) %in% c("Unnamed..0", "date.x","X.x","X.y","X",'date.y','Timestamp_Irish')) ]
df2_final[is.na(df2_final$date),]
head(df2_final)
tail(df2_final)
df2_final[df2_final$date=="",]
as.Date(df2_final$Timestamp)
df2_final$date <- as.Date(df2_final$Timestamp)
head(df2)
nrow(df2_final)


head(df2_final)

# df3_final <- w_t_df %>% left_join(df3_full,by='Timestamp')
head(df3)
df3[df3$date=='2024-03-31',]
# df3$original_ts<- df3$Timestamp
# df3['Timestamp'] = parse_date_time(df3$Timestamp, orders = "ymd HMS", tz = "Europe/Dublin")
write.csv(df3, "D:\\MSc- Data science and analytics\\sem 3 (dissertation)\\mydata\\datasets_for_model\\SPS_for_correction.csv")

df3=read.csv("D:\\MSc- Data science and analytics\\sem 3 (dissertation)\\mydata\\datasets_for_model\\SPS_corrected.csv")
df3_final <- t_w_df %>% left_join(df3,by='Timestamp')
df3_final<- df3_final[ , !(names(df3_final) %in% c("Unnamed..0", "date.x","X.x","X.y","X",'date.y','Timestamp_Irish')) ]
df3_final[is.na(df3_final$date),]

df3_final[df3_final$date=="",]
as.Date(df3_final$Timestamp)
df3_final$date <- as.Date(df3_final$Timestamp)
head(df3)
head(df3_final)
nrow(df3_final)

##################################################################3

# df4_final <- w_t_df %>% left_join(df4_full,by='Timestamp')
head(df4)
df4[df4$date=='2024-03-31',]
# df3$original_ts<- df3$Timestamp
# df3['Timestamp'] = parse_date_time(df3$Timestamp, orders = "ymd HMS", tz = "Europe/Dublin")
write.csv(df4, "D:\\MSc- Data science and analytics\\sem 3 (dissertation)\\mydata\\datasets_for_model\\OP_for_correction.csv")

df4=read.csv("D:\\MSc- Data science and analytics\\sem 3 (dissertation)\\mydata\\datasets_for_model\\OP_corrected.csv")
df4_final <- t_w_df %>% left_join(df4,by='Timestamp')
df4_final<- df4_final[ , !(names(df4_final) %in% c("Unnamed..0", "date.x","X.x","X.y","X",'date.y','Timestamp_Irish')) ]
df4_final[is.na(df4_final$date),]

df4_final[df4_final$date=="",]
as.Date(df4_final$Timestamp)
df4_final$date <- as.Date(df4_final$Timestamp)
head(df4)
nrow(df4_final)
df4_final[is.na(df4_final$date),]
head(df4_final)

############################################################################

# df5_final <- w_t_df %>% left_join(df5_full,by='Timestamp')

head(df5)
df5[df5$date=='2024-03-31',]
# df3$original_ts<- df3$Timestamp
# df3['Timestamp'] = parse_date_time(df3$Timestamp, orders = "ymd HMS", tz = "Europe/Dublin")
write.csv(df5, "D:\\MSc- Data science and analytics\\sem 3 (dissertation)\\mydata\\datasets_for_model\\BS_for_correction.csv")

df5=read.csv("D:\\MSc- Data science and analytics\\sem 3 (dissertation)\\mydata\\datasets_for_model\\BS_corrected.csv")
df5_final <- t_w_df %>% left_join(df5,by='Timestamp')
df5_final<- df5_final[ , !(names(df5_final) %in% c("Unnamed..0", "date.x","X.x","X.y","X",'date.y','Timestamp_Irish')) ]

head(df5_final)
df5_final[is.na(df5_final$date),]

df5_final[df5_final$date=="",]
as.Date(df5_final$Timestamp)
df5_final$date <- as.Date(df5_final$Timestamp)
head(df5)
nrow(df5_final)
df5_final[is.na(df5_final$date),]
head(df5_final)


head(df5_final)

###########################################################

head(df6)
df6[df6$date=='2024-03-31',]
# df3$original_ts<- df3$Timestamp
# df3['Timestamp'] = parse_date_time(df3$Timestamp, orders = "ymd HMS", tz = "Europe/Dublin")
write.csv(df6, "D:\\MSc- Data science and analytics\\sem 3 (dissertation)\\mydata\\datasets_for_model\\GP_for_correction.csv")

df6=read.csv("D:\\MSc- Data science and analytics\\sem 3 (dissertation)\\mydata\\datasets_for_model\\GP_corrected.csv")
df6_final <- t_w_df %>% left_join(df6,by='Timestamp')
df6_final<- df6_final[ , !(names(df6_final) %in% c("Unnamed..0", "date.x","X.x","X.y","X",'date.y','Timestamp_Irish')) ]

head(df6_final)
df6_final[is.na(df6_final$date),]

df6_final[df6_final$date=="",]
as.Date(df6_final$Timestamp)
df6_final$date <- as.Date(df6_final$Timestamp)
head(df6)
nrow(df6_final)
df6_final[is.na(df6_final$date),]
head(df6_final)


head(df6_final)

write.csv(df1_final, "D:\\MSc- Data science and analytics\\sem 3 (dissertation)\\mydata\\datasets_for_model\\combined\\UCC_23_24_combined.csv", row.names = FALSE)
write.csv(df2_final, "D:\\MSc- Data science and analytics\\sem 3 (dissertation)\\mydata\\datasets_for_model\\combined\\LG_23_24_combined.csv", row.names = FALSE)
write.csv(df3_final, "D:\\MSc- Data science and analytics\\sem 3 (dissertation)\\mydata\\datasets_for_model\\combined\\SPS_combined.csv", row.names = FALSE)
write.csv(df4_final, "D:\\MSc- Data science and analytics\\sem 3 (dissertation)\\mydata\\datasets_for_model\\combined\\OP_combined.csv", row.names = FALSE)
write.csv(df5_final, "D:\\MSc- Data science and analytics\\sem 3 (dissertation)\\mydata\\datasets_for_model\\combined\\BS_combined.csv", row.names = FALSE)
write.csv(df6_final, "D:\\MSc- Data science and analytics\\sem 3 (dissertation)\\mydata\\datasets_for_model\\combined\\GP_combined.csv", row.names = FALSE)
#######################################################################################################################################################################
df1_final=read.csv("D:\\MSc- Data science and analytics\\sem 3 (dissertation)\\mydata\\datasets_for_model\\cleaned\\UCC_23_24_cleaned.csv")
df2_final=read.csv("D:\\MSc- Data science and analytics\\sem 3 (dissertation)\\mydata\\datasets_for_model\\cleaned\\LG_cleaned.csv")
df3_final=read.csv("D:\\MSc- Data science and analytics\\sem 3 (dissertation)\\mydata\\datasets_for_model\\cleaned\\SPS_cleaned.csv")
df4_final=read.csv("D:\\MSc- Data science and analytics\\sem 3 (dissertation)\\mydata\\datasets_for_model\\cleaned\\OP_cleaned.csv")
df5_final=read.csv("D:\\MSc- Data science and analytics\\sem 3 (dissertation)\\mydata\\datasets_for_model\\cleaned\\BS_cleaned.csv")
df5_final=read.csv("D:\\MSc- Data science and analytics\\sem 3 (dissertation)\\mydata\\datasets_for_model\\cleaned\\GP_cleaned.csv")
library(readr)

# Read all six datasets
df1_final <- df1_final %>% mutate(StationID = "UCC", NO2 = as.numeric(NO2))
df2_final <- df2_final %>% mutate(StationID = "LG", NO2 = as.numeric(NO2))
df3_final <- df3_final %>% mutate(StationID = "SPS", NO2 = as.numeric(NO2))
df4_final <- df4_final %>% mutate(StationID = "OP", NO2 = as.numeric(NO2))
df5_final <- df5_final %>% mutate(StationID = "BS", NO2 = as.numeric(NO2))
df6_final <- df6_final %>% mutate(StationID = "GP", NO2 = as.numeric(NO2))

# Make sure all column names are consistent before binding
common_cols <- Reduce(intersect, list(
  colnames(df1_final), colnames(df2_final), colnames(df3_final),
  colnames(df4_final), colnames(df5_final), colnames(df6_final)
))

# Select only common columns
df1_final <- df1_final %>% select(all_of(common_cols))
df2_final  <- df2_final  %>% select(all_of(common_cols))
df3_final <- df3_final  %>% select(all_of(common_cols))
df4_final  <- df4_final  %>% select(all_of(common_cols))
df5_final  <- df5_final  %>% select(all_of(common_cols))
df6_final  <- df6_final  %>% select(all_of(common_cols))

# Combine all into a single long-format dataframe
long_df <- bind_rows(df1_final, df2_final, df3_final, df4_final, df5_final, df6_final)

long_df <- rbind(df1_final, df2_final, df3_final, df4_final, df5_final, df6_final)

# Optional: arrange chronologically and by station
long_df <- long_df %>% arrange(Timestamp, StationID)

# Check final structure
glimpse(long_df)

head(long_df)

long_df <- long_df %>%
  mutate(
    date = as.Date(date),  # ensure proper Date type
    weekday = as.integer(format(date, "%u")),  # 1 = Monday, 7 = Sunday
    month = as.integer(format(date, "%m"))
  )

long_df$season <- with(long_df, ifelse(month %in% c(3, 4, 5), 1,  # Spring
                           ifelse(month %in% c(6, 7, 8), 2,       # Summer
                           ifelse(month %in% c(9, 10, 11), 3,     # Autumn
                           4))))                                  # winter
head(long_df)
# Save if needed
write_csv(long_df, "D:\\MSc- Data science and analytics\\sem 3 (dissertation)\\mydata\\datasets_for_model\\cleaned\\sorted_long_format_NO2_dataset.csv")


l_df1 <- read.csv("D:\\MSc- Data science and analytics\\sem 3 (dissertation)\\mydata\\datasets_for_model\\cleaned\\long_format_NO2_dataset.csv")
l_df2<- read.csv("D:\\MSc- Data science and analytics\\sem 3 (dissertation)\\mydata\\datasets_for_model\\cleaned\\sorted_long_format_NO2_dataset.csv")

holidays <- read.csv("D:\\MSc- Data science and analytics\\sem 3 (dissertation)\\mydata\\CORK SCHOOLS\\school_hols_23_24.csv")
head(holidays)
holidays$date <- as.Date(holidays$date, format='%d-%m-%Y')

holidays$schoolTerm=0
public_holidays <- as.Date(c(
  # 2023
  "2023-01-01",
  "2023-02-06",
  "2023-03-17",
  "2023-04-10",
  "2023-05-01",
  "2023-06-05",
  "2023-08-07",  # August Bank Holiday
  "2023-10-30",  # October Bank Holiday
  "2023-12-25",  # Christmas Day
  "2023-12-26",  # St. Stephen's Day
  # 2024
  "2024-01-01",  # New Year's Day
  "2024-02-05",  # February Bank Holiday
  "2024-03-17",  # St. Patrick's Day
  "2024-04-01",  # Easter Monday
  "2024-05-06",  # May Bank Holiday
  "2024-06-03", # June Bank Holiday
  "2024-08-05",
  "2024-10-28",
  "2024-12-25",
  "2024-12-26"
))
library(dplyr)
school_hols <- holidays$date
school_hols
public_holidays
df5_final$date <- as.Date(df5_final$date, format='%d-%m-%Y')
head(df5_final)
df5_final <-df5_final %>%
  mutate(
    school_term = ifelse(((date %in% school_hols) | (date %in% public_holidays)), 0, 1)
  )
head(df5_final)
df5_final$weekday_iso <- as.numeric(format(df5_final$date, "%u"))
df5_final$school_term[df5_final$weekday_num %in% c(6, 7)] <- 0
head(df5_final)
write.csv(df5_final,"D:\\MSc- Data science and analytics\\sem 3 (dissertation)\\mydata\\datasets_for_model\\cleaned\\GP_cleaned.csv")
