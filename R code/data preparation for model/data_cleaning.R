library(dplyr)
### repeat same steps for the rest of the streets.

data<- read.csv("D:\\MSc- Data science and analytics\\sem 3 (dissertation)\\mydata\\datasets_for_model\\combined\\UCC_23_24_combined.csv")
df<- data[,c('Timestamp','date','NO2')]
head(df)


df<- data[,c('Timestamp','date','counter.2','NO2')]
df[df$NO2=='#N/A',]
df[is.na(df$NO2),]
df$NO2[(df$NO2<0)]

which(df$NO2<0)
df$NO2[(df$NO2<0)] <- NA
data$NO2[data$NO2<0] <- NA
which(data$NO2<0)

df[df$NO2=='#N/A',]
df[is.na(df$Timestamp),]
head(df)
library(dplyr)

# Assuming df is your full dataset and NO2 is read as numeric
df$NO2 <- as.numeric(df$NO2)
data$NO2 <- as.numeric(data$NO2)
summary(df)
summary(data)

head(df)
df[is.na(df$NO2),]
data[is.na(data$NO2),]
# Count NA NO2 values per date
missing_summary <- df %>%
  group_by(date) %>%
  summarise(
    total_obs = n(),
    na_count = sum(is.na(NO2)),
    na_percent = 100 * na_count / total_obs
  ) %>%
  arrange(desc(na_percent))

missing_df<-as.data.frame(missing_summary)
missing_df[missing_df$na_percent>0,]


# Flag days with > 20% missing
bad_days <- missing_df$date[missing_df$na_percent > 20]
length(bad_days)
nrow(df)
df[!(df$date %in% bad_days),]


data <- data[!(data$date %in% bad_days),]
df <- df[!(df$date %in% bad_days),]

missing_summary <- df %>%
  group_by(date) %>%
  summarise(
    total_obs = n(),
    na_count = sum(is.na(NO2)),
    na_percent = 100 * na_count / total_obs
  ) %>%
  arrange(desc(na_percent))


missing_df<-as.data.frame(missing_summary)
missing_df[missing_df$na_percent>0,]

df[is.na(df$NO2),]
library(zoo)
df$Timestamp <- ymd_hms(df$Timestamp, tz = "Europe/Dublin")
df<-df %>%
  arrange(Timestamp) %>%            # Ensure chronological order
  group_by(date) %>%                # Apply interpolation day-wise
  mutate(NO2 = na.approx(NO2,       # Interpolate missing values
                         x = Timestamp, 
                         na.rm = FALSE)) %>%
  ungroup()



df[is.na(df$NO2),]
still_missing <- df %>% filter(is.na(NO2))


# Check their timestamps
still_missing %>% select(Timestamp)
still_missing

df<-df %>%
  mutate(hour=hour(Timestamp))



library(dplyr)
library(lubridate)

impute_local_smooth <- function(df) {
  df <- df %>%
    arrange(Timestamp)  # Ensure global time order
  
  # Loop over rows where NO2 is NA and hour is 23 or 0
  for (i in which(is.na(df$NO2))) {
    current_time <- df$Timestamp[i]
    
    # Define 2 hours before and 2 hours after the current time
    neighbor_window <- c(current_time - hours(2), current_time - hours(1),
                         current_time + hours(1), current_time + hours(2))
    
    # Get indices of these timestamps
    neighbor_indices <- which(df$Timestamp %in% neighbor_window)
    
    # Extract non-NA neighbor values
    neighbor_values <- df$NO2[neighbor_indices]
    valid_values <- neighbor_values[!is.na(neighbor_values)]
    
    # If at least one non-NA value found, replace current NA with their mean
    if (length(valid_values) > 0) {
      df$NO2[i] <- mean(valid_values)
    }
  }
  
  return(df)
}


df <- impute_local_smooth(df)

df[df$Timestamp %in% still_missing$Timestamp,]

head(df)


# Recalculate median for each hour (excluding NAs)
hourly_medians <- df %>%
  filter(!is.na(NO2)) %>%
  group_by(hour) %>%
  summarise(hour_median = median(NO2, na.rm = TRUE))

# Replace remaining NAs using hour-specific median
df <- df %>%
  left_join(hourly_medians, by = "hour") %>%
  mutate(NO2 = ifelse(is.na(NO2), hour_median, NO2)) %>%
  select(-hour_median)


nrow(df)

data <- data[!((data$Timestamp=='2023-10-29 01:00:00+00:00') | (data$Timestamp=='2023-10-29 01:00:00+01:00')),]

data<-data %>%
  mutate(hour=hour(Timestamp))

data$Timestamp <- ymd_hms(data$Timestamp, tz = "Europe/Dublin")
data<-data %>%
  arrange(Timestamp) %>%            # Ensure chronological order
  group_by(date) %>%                # Apply interpolation day-wise
  mutate(NO2 = na.approx(NO2,       # Interpolate missing values
                         x = Timestamp, 
                         na.rm = FALSE)) %>%
  ungroup()
# data$NO2 <- ifelse((is.na(data$NO2) & (data$hour == 23)), median_23, data$NO2)


data <- impute_local_smooth(data)

data <- data %>%
  left_join(hourly_medians, by = "hour") %>%
  mutate(NO2 = ifelse(is.na(NO2), hour_median, NO2)) %>%
  select(-hour_median)

still_missing <- data %>% filter(is.na(NO2))
still_missing
head(data)
tail(data)
nrow(data)


write.csv(data,"D:\\MSc- Data science and analytics\\sem 3 (dissertation)\\mydata\\datasets_for_model\\cleaned\\UCC_23_24_cleaned.csv")


data <- read.csv("D:\\MSc- Data science and analytics\\sem 3 (dissertation)\\mydata\\datasets_for_model\\cleaned\\LG_cleaned.csv")
df<- data[,c('Timestamp','date','counter.2','NO2')]
df[df$counter.2=='#N/A',]
df[is.na(df$counter.2),]
df[df$NO2=='#N/A',]
df[is.na(df$Timestamp),]

head(df)


# Assuming df is your full dataset and NO2 is read as numeric
df$counter.2 <- as.numeric(df$counter.2)
data$counter.2 <-as.numeric(data$counter.2)
summary(df)

head(df)
df[is.na(df$counter.2),]

# Count NA NO2 values per date
missing_summary <- df %>%
  group_by(date) %>%
  summarise(
    total_obs = n(),
    na_count = sum(is.na(counter.2)),
    na_percent = 100 * na_count / total_obs
  ) %>%
  arrange(desc(na_percent))

missing_df<-as.data.frame(missing_summary)
missing_df[missing_df$na_percent>0,]


# Flag days with > 20% missing
bad_days <- missing_df$date[missing_df$na_percent > 20]
length(bad_days)
nrow(df)
df[!(df$date %in% bad_days),]


data <- data[!(data$date %in% bad_days),]
df <- df[!(df$date %in% bad_days),]

missing_summary <- df %>%
  group_by(date) %>%
  summarise(
    total_obs = n(),
    na_count = sum(is.na(counter.2)),
    na_percent = 100 * na_count / total_obs
  ) %>%
  arrange(desc(na_percent))


missing_df<-as.data.frame(missing_summary)
missing_df[missing_df$na_percent>0,]

data[is.na(data$counter.2),]
data[is.na(data$counter.1),]
data[data$counter.2<0]
##########################################################################

holidays <- read.csv("D:\\MSc- Data science and analytics\\sem 3 (dissertation)\\mydata\\CORK SCHOOLS\\school_hols_23_24.csv")

head(holidays)
holidays$date <- as.Date(holidays$date, format='%d-%m-%Y')

holidays$schoolTerm=0
public_holidays <- as.Date(c(
  # 2023
  "2023-01-01",  # New Year's Day
  "2023-02-06",  # February Bank Holiday (new from 2023)
  "2023-03-17",  # St. Patrick's Day
  "2023-04-10",  # Easter Monday
  "2023-05-01",  # May Bank Holiday
  "2023-06-05",  # June Bank Holiday
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
  "2024-06-03",  # June Bank Holiday
  "2024-08-05",  # August Bank Holiday
  "2024-10-28",  # October Bank Holiday
  "2024-12-25",  # Christmas Day
  "2024-12-26"   # St. Stephen's Day
))
school_hols <- holidays$date
school_hols
public_holidays
head(data)
data
as.Date(data$date)
data$date <- as.Date(data$date)
head(data)
data <- data %>%
  mutate(
    school_term = ifelse(((date %in% school_hols) | (date %in% public_holidays)), 0, 1)
  )
head(data)
data
data$date<- as.Date(data$Timestamp, format = '%d-%m-%Y %H:%M', tz='Europe/Dublin')
write.csv(data,"D:\\MSc- Data science and analytics\\sem 3 (dissertation)\\mydata\\datasets_for_model\\cleaned\\UCC_23_24_cleaned.csv")
