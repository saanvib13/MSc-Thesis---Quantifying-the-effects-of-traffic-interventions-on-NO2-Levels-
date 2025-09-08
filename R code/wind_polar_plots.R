data=read.csv("D:\\MSc- Data science and analytics\\sem 3 (dissertation)\\mydata\\hourly_cork_airport_since_2022.csv")

# Original conversion (may produce 2062 instead of 1962)
data$Timestamp <- as.POSIXct(data$date, tz='Europe/Dublin', format='%d-%m-%Y %H:%M')

data$date <- as.Date(data$Timestamp, format = "%d-%b-%y")
head(data)
# Manually fix years > current year (e.g., 2025) to 1900s
data$year <- as.numeric(format(data$date, "%Y"))

data$month <- as.numeric(format(data$date, "%m"))
data$year[data$year > 2025] <- data$year[data$year > 2025] - 100

# Rebuild corrected date
data$date <- as.Date(paste(format(data$date, "%d-%b"), data$year), format = "%d-%b %Y")
data

data<-data[data$year>=2014 & data$year<=2024,]
data

data1 <- data[(data$date >= as.Date('2022-04-01') & data$date<=as.Date('2022-09-30')) | 
(data$date >= as.Date('2021-04-01') & data$date<=as.Date('2021-09-30')) |
(data$date >= as.Date('2020-04-01') & data$date<=as.Date('2020-09-30')) |                
(data$date >= as.Date('2019-04-01') & data$date<=as.Date('2019-09-30')) |
(data$date >= as.Date('2018-04-01') & data$date<=as.Date('2018-09-30')) |
(data$date >= as.Date('2017-04-01') & data$date<=as.Date('2017-09-30')) |
(data$date >= as.Date('2016-04-01') & data$date<=as.Date('2016-09-30')) |
(data$date >= as.Date('2015-04-01') & data$date<=as.Date('2015-09-30')) |
(data$date >= as.Date('2014-04-01') & data$date<=as.Date('2014-09-30')) |
(data$date >= as.Date('2023-04-01') & data$date<=as.Date('2023-09-30')) |
(data$date >= as.Date('2024-04-01') & data$date<=as.Date('2024-09-30')), ]

data2 <- data[((data$date >= as.Date('2022-10-01') & data$date<=as.Date('2023-03-31')) | 
                (data$date >= as.Date('2023-10-01') & data$date<=as.Date('2024-03-31')) |
                 (data$date >= as.Date('2020-10-01') & data$date<=as.Date('2021-03-31')) |
                 (data$date >= as.Date('2019-10-01') & data$date<=as.Date('2020-03-31')) |
                 (data$date >= as.Date('2018-10-01') & data$date<=as.Date('2019-03-31')) |
                 (data$date >= as.Date('2017-10-01') & data$date<=as.Date('2018-03-31')) |
                 (data$date >= as.Date('2016-10-01') & data$date<=as.Date('2017-03-31')) |
                 (data$date >= as.Date('2015-10-01') & data$date<=as.Date('2016-03-31')) |
                 (data$date >= as.Date('2014-01-01') & data$date<=as.Date('2014-03-31')) |
                 (data$date >= as.Date('2017-10-01') & data$date<=as.Date('2018-03-31')) |
                (data$date >= as.Date('2021-10-01') & data$date<=as.Date('2023-03-31'))), ]
data1

data2

wdspd1 <-data1$wdsp
wddir1 <- data1$wddir

wdspd2 <- data2$wdsp
wddir2 <- data2$wddir

wdspd<-data$wdsp
wddir<-data$wddir

library(ggplot2)

df1 <- data.frame(speed = wdspd1, direction = wddir1)
library(openair)
windRose(data1, ws = "wdsp", wd = "wddir",
         paddle = FALSE,                   # bar instead of petal
         breaks = c(0, 2, 4, 6, 8, 10),    # wind speed bins
         cols = "Set2",                    # color palette
         angle = 30,                       # wind direction bin width
         main = "Windspeed vs Wind direction for Summer months 2014–2024")

windRose(data2, ws = "wdsp", wd = "ddhm",
         paddle = FALSE,                   # bar instead of petal
         breaks = c(0, 2, 4, 6, 8, 10),    # wind speed bins
         cols = "Set2",                    # color palette
         angle = 30,                       # wind direction bin width
         main = "Windspeed vs Wind direction for Winter months 2014–2024")

windRose(data, ws = "wdsp", wd = "wddir",
         paddle = FALSE,                   # bar instead of petal
         breaks = c(0, 2, 4, 6, 8, 10),    # wind speed bins
         cols = "Set2",                    # color palette
         angle = 30,                       # wind direction bin width
         main = "Windspeed vs Wind direction for 2014–2024")


# Sample data: assume 'df' has columns wind_speed and wind_dir (0–360 degrees)
ggplot(df1, aes(x = direction, y = speed, color = speed)) +
  geom_point(alpha = 0.7, size = 2) +
  scale_color_gradientn(
    colours = c("lightblue", "blue","red"),
    name = "Wind Speed"
  )+
  coord_polar(start = -pi/2) +  # rotate so 0° (North) is up
  scale_x_continuous(
    breaks = c(0, 90, 180, 270),
    labels = c("N", "E", "S", "W"),
    limits = c(0, 360)
  ) +
  labs(
    title = "Wind Speed & Direction Polar Plot for April-September",
    x = "Wind Direction (°)",
    y = "Wind Speed"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.major = element_line(color = "grey50"),
    axis.text.x = element_text(face = "bold")
  )


df2 <- data.frame(speed = wdspd2, direction = wddir2)
library(ggplot2)

# Sample data: assume 'df' has columns wind_speed and wind_dir (0–360 degrees)
ggplot(df2, aes(x = direction, y = speed, color = speed)) +
  geom_point(alpha = 0.7, size = 2) +
  scale_color_gradientn(
    colours = c("lightblue", "blue", "red"),
    name = "Wind Speed"
  )+
  coord_polar(start = -pi/2) +  # rotate so 0° (North) is up
  scale_x_continuous(
    breaks = c(0, 90, 180, 270),
    labels = c("N", "E", "S", "W"),
    limits = c(0, 360)
  ) +
  labs(
    title = "Wind Speed & Direction Polar Plot For October-March",
    x = "Wind Direction (°)",
    y = "Wind Speed"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.major = element_line(color = "grey50"),
    axis.text.x = element_text(face = "bold")
  )






# #########################################################3
# # POLAR PLOTS FOR TEMPERATURE
# df1 <- data1[!is.na(data1$temp),]
# df2 <- data2[!is.na(data2$temp),]

df <- data[!is.na(data$temp),]
# 
# df1$doy <- as.numeric(format(df1$date, "%j"))
# df2$doy <- as.numeric(format(df2$date, "%j"))

df$doy <- as.numeric(format(df$date, "%j"))
# 
# # If you want to normalize across years:
# df1$year <- format(df1$date, "%Y")
# df2$year <- format(df2$date, "%Y")

df$year <- format(df$date, "%Y")
# 
# ggplot(df1, aes(x = doy, y = temp, color = temp)) +
#   geom_point(alpha = 0.7) +
#   scale_color_gradientn(colors = c("blue", "green", "red"), name = "Temp (°C)") +
#   coord_polar(start = -pi/2) +
#   scale_x_continuous(
#     breaks = c(91, 121, 152, 182, 213, 244, 274),  # approximate DOYs for Apr–Sep
#     labels = c("Apr", "May", "Jun", "Jul", "Aug", "Sep", "")
#   ) +
#   labs(title = "Temperature Pattern: April–September (2022–2024)", x = "", y = "") +
#   theme_minimal(base_size = 12) +
#   theme(
#     panel.grid.major = element_line(color = "grey50"),
#     axis.text.x = element_text(face = "bold")
#   )


ggplot(df, aes(x = doy, y = temp, color = temp)) +
  geom_point(alpha = 0.7) +
  scale_color_gradientn(colors = c("blue", "green", "red"), name = "Temp (°C)") +
  coord_polar(start = -pi/2) +
  scale_x_continuous(
    breaks = c(91, 121, 152, 182, 213, 244, 274),  # approximate DOYs for Apr–Sep
    labels = c("Apr", "May", "Jun", "Jul", "Aug", "Sep", "")
  ) +
  labs(title = "Temperature Pattern: April–September (2022–2024)", x = "", y = "") +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.major = element_line(color = "grey50"),
    axis.text.x = element_text(face = "bold")
  )



# 
# 
# ggplot(df2, aes(x = doy, y = temp, color = temp)) +
#   geom_point(alpha = 0.7) +
#   scale_color_gradientn(colors = c("blue", "green", "red"), name = "Temp (°C)") +
#   coord_polar(start = -pi/2) +
#   scale_x_continuous(
#     breaks = c(274, 305, 335, 1, 32, 60, 91),  # DOYs for Oct–Mar
#     labels = c("Oct", "Nov", "Dec", "Jan", "Feb", "Mar", "")
#   ) +
#   labs(title = "Temperature Pattern: October–March (2022–2024)", x = "", y = "") +
#   theme_minimal()


##################################################################
df <- data[!is.na(data$wdsp),]
df$doy <- as.numeric(format(df$date, "%j"))
df$year <- format(df$date, "%Y")

library(ggplot2)
ggplot(df, aes(x = doy, y = wdsp, color = wdsp)) +
  geom_point(alpha = 0.7) +
  scale_color_gradientn(colors = c("blue", "green", "red"), name = "Speed (knots)") +
  coord_polar(start = -pi/2) +
  scale_x_continuous(
    breaks = c(1, 32, 60, 91, 121, 152, 182, 213, 244, 274, 305, 335),
    labels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
  ) +
  labs(title = "Windspeed Pattern for Monthly Data: 2014–2024", x = "", y = "") +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.major = element_line(color = "grey50"),
    axis.text.x = element_text(face = "bold")
  )



