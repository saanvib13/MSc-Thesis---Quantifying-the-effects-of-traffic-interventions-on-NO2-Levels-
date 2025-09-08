data=read.csv("D:\\MSc- Data science and analytics\\sem 3 (dissertation)\\mydata\\hourly_NO2_streets\\UCC_hourly_NO2.csv")

data <- data[!is.na(data$NO2),]

data$Timestamp<- as.POSIXct(data$Timestamp, format="%d-%m-%Y %H:%M", tz='utc')
# Convert to Irish time zone (Europe/Dublin)
data$Timestamp_Irish <- format(data$Timestamp, tz = "Europe/Dublin", usetz = TRUE)

# Optional: convert it back to POSIXct if needed for plotting or analysis
data$Timestamp_Irish <- as.POSIXct(data$Timestamp_Irish, tz = "Europe/Dublin")

data$date<-as.Date(data$Timestamp_Irish)
data$hour<- hour(data$Timestamp_Irish)
data$year <- format(data$date, "%Y")

data[2000,]

library(tidyverse)

monthly_no2<-data %>%
  group_by(date) %>%
  summarise(
    max_no2 = max(NO2)
  )

monthly_no2


format(data$date, "%j")
##################################################################
monthly_no2$doy <- as.numeric(format(monthly_no2$date, "%j"))
# df$year <- format(df$date, "%Y")

library(ggplot2)
ggplot(monthly_no2, aes(x = doy, y = max_no2, color = max_no2)) +
  geom_point(alpha = 0.7) +
  scale_color_gradientn(colors = c("blue", "green", "red"), name = "NO2") +
  coord_polar(start = -pi/2) +
  scale_x_continuous(
    breaks = c(1, 32, 60, 91, 121, 152, 182, 213, 244, 274, 305, 335),
    labels = c(
      "Jan","Feb","Mar","Apr","May","June","July","Aug","Sept","Oct","Nov","Dec"
    )
  ) +
  labs(title = "NO2 for Monthly Data: 2020–2024", x = "", y = "") +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.major = element_line(color = "grey50"),
    axis.text.x = element_text(face = "bold")
  )



