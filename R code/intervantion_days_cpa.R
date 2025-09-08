data <- read.csv("D:\\MSc- Data science and analytics\\sem 3 (dissertation)\\mydata\\St_Patricks_2min_FullDataSet.csv")
data
## this is the analysis for st patrick's day. similar analysis was conducted for car free day and open streets event days
data<-data[!is.na(data$NO2),]

data$Timestamp<- as.POSIXct(data$Timestamp, format="%Y-%m-%d %H:%M:%S", tz='utc')
# Convert to Irish time zone (Europe/Dublin)
data$Timestamp_Irish <- format(data$Timestamp, tz = "Europe/Dublin", usetz = TRUE)

# Optional: convert it back to POSIXct if needed for plotting or analysis
data$Timestamp_Irish <- as.POSIXct(data$Timestamp_Irish, tz = "Europe/Dublin")
data$Weekday<- weekdays(data$Timestamp_Irish, abbreviate = FALSE)


tail(data[data$date==as.Date("2023-09-22"),])

data$date <- as.Date(format(data$Timestamp_Irish, tz = "Europe/Dublin", usetz = TRUE))
no2_patrick_day<-data[data$date==as.Date("2024-03-17"),]

no2_patrick_day


acf(no2_patrick_day$NO2)
shapiro.test(no2_patrick_day$NO2)

library(forecast)
fit<-auto.arima(no2_patrick_day$NO2, seasonal = FALSE)
res <- residuals(fit)
acf(res)
Box.test(residuals(fit), lag = 20, type = "Ljung-Box")


library(dplyr)

# Assuming your data has a timestamp column
no2_patrick_day_agg <- no2_patrick_day %>%
  mutate(minute_group = as.numeric(format(Timestamp_Irish, "%M")) %/% 10) %>%
  group_by(hour = format(Timestamp_Irish, "%H"), minute_group) %>%
  summarise(NO2 = mean(NO2), .groups = "drop")

# Create ts object and fit ARIMA
ts_no2 <- ts(no2_patrick_day_agg$NO2)
fit <- auto.arima(ts_no2, seasonal = FALSE)
checkresiduals(fit)

fit2 <- Arima(ts_no2, order = c(5,1,4))
checkresiduals(fit2)



library(dplyr)
library(stringr)


no2_patrick_day_agg<- no2_patrick_day_agg %>%
  mutate(
    # Convert minute group to actual minute value
    minutes = minute_group * 10,
    
    # Pad hour and minute with leading zeros
    hour_str = str_pad(hour, width = 2, pad = "0"),
    minute_str = str_pad(minutes, width = 2, pad = "0"),
    
    # Combine to HH:MM format
    time = paste0(hour_str, ":", minute_str)
  )

no2_patrick_day_agg

no2_patrick_day_agg$datetime <- as.POSIXct(
  paste("2024-05-19", no2_patrick_day_agg$time),
  format = "%Y-%m-%d %H:%M"
)

plot(no2_patrick_day$Timestamp_Irish,no2_patrick_day$NO2,ylim=c(0,80), type='l')
library(changepoint.np)
library(changepoint)
cps<-cpt.np(residuals(fit2), method='PELT',penalty='MBIC', minseglen = 2)
plot(cps)
no2_patrick_day_agg$datetime[cpts(cps)]

no2_patrick_day_agg$minute_group[cpts(cps)]

cts<-cpts(cps)
if(cts[1]!=1){
  bounds2=c(1, cts, length(no2_patrick_day$Timestamp_Irish))
} else{
  bounds2=c(cts, length(no2_patrick_day$Timestamp_Irish))
}

if(cts[1]!=1){
  bounds2=c(1, cts, length(no2_patrick_day_agg$datetime))
} else{
  bounds2=c(cts, length(no2_patrick_day_agg$datetime))
}

#plot(data$wdsp, type='l')

library(boot)
# 95 % percentile CI from a boot object
boot_ci95 <- function(bt) boot.ci(bt, type = "perc")$percent[4:5]  # lower, upper

# ------------------------------------------------------------
# 2.  Bootstrap each segment’s mean (1 000 resamples)
# ------------------------------------------------------------
boot_mean <- function(data, i) mean(data[i], na.rm = TRUE)
#boot_sd <- function(data, i) sd(data[i], na.rm = TRUE)
seg_tbl <- data.frame(
  Segment = integer(),
  Start   = character(),
  End     = character(),
  Mean    = numeric(),
  CI_low  = numeric(),
  CI_high = numeric(),
  stringsAsFactors = FALSE
)

for (s in seq_len(length(bounds2)-1)) {
  vals <- na.omit(no2_patrick_day_agg$NO2)[ bounds2[s] : (bounds2[s + 1]) ]
  hrs_ <- no2_patrick_day_agg$datetime[ bounds2[s] : (bounds2[s + 1]) ]
  
  if (length(unique(vals)) <= 1) {
    warning(paste("Skipping segment", s, "due to insufficient variability"))
    next
  }
  
  # bootstrap
  bt   <- boot(vals, boot_mean, R = 2000)
  ci   <- boot_ci95(bt)
  seg_tbl <- rbind(seg_tbl, data.frame(
    Segment = s,
    Start   = hrs_[1],
    End     = hrs_[length(hrs_)],
    Mean    = mean(vals, na.rm = TRUE),
    CI_low  = ci[1],
    CI_high = ci[2]
  ))
}

# ------------------------------------------------------------
# 3.  Test CI overlap with the previous segment
# ------------------------------------------------------------
seg_tbl$mean_CI_nonoverlap <- c(NA,  # segment 1 has no predecessor
                                vapply(2:nrow(seg_tbl), function(i) {
                                  ci1 <- seg_tbl[i,   c("CI_low", "CI_high")]
                                  ci0 <- seg_tbl[i-1, c("CI_low", "CI_high")]
                                  !(ci1$CI_low <= ci0$CI_high && ci0$CI_low <= ci1$CI_high)  # TRUE if no overlap
                                }, logical(1))
)

print(seg_tbl, digits = 3)

seg_tbl$Mean
mean_diff_threshold <- 1.5 # in NO2 units (e.g., µg/m³)
library(data.table)

# Convert to data.table for convenience
segments_dt <- as.data.table(seg_tbl)

# Initialize merge group vector
merge_groups <- integer(nrow(segments_dt))
group_id <- 1
merge_groups[1] <- group_id

for(i in 2:nrow(segments_dt)) {
  mean_diff <- abs(segments_dt$Mean[i] - segments_dt$Mean[i-1])
  
  # If difference is below threshold → same group, else new group
  if(mean_diff <= mean_diff_threshold) {
    merge_groups[i] <- group_id
  } else {
    group_id <- group_id + 1
    merge_groups[i] <- group_id
  }
}

# Add grouping column
segments_dt[, MergeGroup := merge_groups]

merged_segments <- segments_dt[, .(
  Start = min(Start),
  End = max(End),
  Mean = mean(Mean),  # or weighted mean if you have duration
  CI_low = min(CI_low),
  CI_high = max(CI_high),
  SegmentsIncluded = paste(Segment, collapse = ",")
), by = MergeGroup]



x<-merged_segments$Start
y <-merged_segments$End

# x<-rbind(x,seg_tbl$Start[(seg_tbl$mean_CI_nonoverlap)])

x
y
indices <- c(which(no2_patrick_day_agg$datetime == x[1]))
indices_end <- c(which(no2_patrick_day_agg$datetime == y[1]))
x<-x[2:length(x)]
y<-y[2:length(y)]
df[df$Timestamp=="2022-06-21 06:00:00+01:00",]

for (j in x){
  indices <-c(indices, which(no2_patrick_day_agg$datetime == j))
}

for (j in y){
  indices_end <-c(indices_end, which(no2_patrick_day_agg$datetime == j))
}

indices
indices_end
ticks <- seq(1, nrow(no2_patrick_day_agg), by = 15)

merged_segments

mseg_df <- as.data.frame(list(
  Start=indices,
  End=indices_end,
  Mean=merged_segments$Mean)
)
mseg_df
par(mar = c(6, 4, 4, 2))  # Bottom, Left, Top, Right margins
no2_patrick_day$Time <- format(no2_patrick_day$Timestamp_Irish, "%H:%M:%S")

no2_patrick_day_agg$Time <- format(no2_patrick_day_agg$datetime, "%H:%M:%S")

plot(no2_patrick_day$NO2,xaxt="n", type='l',xlab="",ylab='NO2 conc', main='Open Streets Event 16 June 2024, Oliver Plunkett Street',ylim=c(0,60),lwd=2, col='grey50')

plot(no2_patrick_day_agg$NO2,xaxt="n", type='l',xlab="",ylab='NO2 conc', main='Open Streets Event 19 May 2024, St Patrick\'s Street',ylim=c(0,60),lwd=2, col='grey50')
abline(v=indices, col='magenta',lwd=2,lty=3)

axis(1, at = ticks, labels = no2_patrick_day_agg$Time[ticks], las = 2, cex.axis = 0.7)
mtext('Time', side=1, line=4)

with(mseg_df, {
  segments(x0 = Start,
           y0 = Mean,
           x1 = End,
           y1 = Mean,
           col = "royalblue",
           lwd = 2,
           lty = 1)
})

with(mseg_df, {
  mid_x <- (Start + End) / 2  # midpoint of the segment
  text(x = mid_x,
       y = Mean + 3,          # a little above the line
       labels = round(Mean, 2),  # label with rounded mean value
       col = "blue",
       cex = 0.8)
})


# y<-seg_tbl[(seg_tbl$mean_CI_nonoverlap),]
# 
# rbind(seg_tbl[1,], y[2:nrow(y),])

merged_segments

library(lubridate)
library(dplyr)

no2_patrick_day <- no2_patrick_day %>%
  mutate(
    hour=hour(no2_patrick_day$Timestamp_Irish) 
  )

no2_patrick_day_agg <- no2_patrick_day_agg %>%
  mutate(
    hour2=hour(no2_patrick_day_agg$datetime) 
  )

hourwise_patrick_day <- no2_patrick_day %>% group_by(hour) %>%
  summarise(
    mean_no2=mean(NO2)
  )

hourwise_patrick_day <- no2_patrick_day_agg %>% group_by(hour2) %>%
  summarise(
    mean_no2=mean(NO2)
  )

?mutate
no2_patrick_day

hourwise_patrick_day

# ticks <- seq(1,length(hourwise_patrick_day$hour), by=3)
# plot(hourwise_patrick_day$hour, hourwise_patrick_day$mean_no2, type='l', xaxt='n',xlab="",ylab='NO2 concentration', main='NO2 concentration on St Patrick\'s Day by hour')
# axis(1,at = ticks,labels =hourwise_patrick_day$hour[ticks])
# mtext('Hour', side=1, line=3)

ticks <- seq(1,length(no2_patrick_day$hour), by=3)
plot(no2_patrick_day$hour, no2_patrick_day$NO2, type='l', xaxt='n',xlab="",ylab='NO2 concentration', main='NO2 concentration on St Patrick\'s Day by hour')
axis(1,at = ticks,labels =hourwise_patrick_day$hour[ticks])
mtext('Hour', side=1, line=3)

data<-data %>%
  mutate(
    month=month(Timestamp_Irish)
  )
march_24 <- data[data$month==3,]
# march_24_sundays= march_24[((march_24$date==as.Date('2023-09-16')) | (march_24$date==as.Date('2023-09-30')) |
#                               (march_24$date==as.Date('2023-09-02'))),]
march_24
march_24_sundays= march_24[((march_24$date==as.Date('2024-03-10')) | (march_24$date==as.Date('2024-03-03')) |
                              (march_24$date==as.Date('2024-03-24')) | (march_24$date==as.Date('2024-03-31'))),]

march_24_sundays= march_24[((march_24$date==as.Date('2024-06-02')) | (march_24$date==as.Date('2024-06-09')) |
                              (march_24$date==as.Date('2024-06-23')) | (march_24$date==as.Date('2024-06-30'))),]

unique(march_24_sundays$date)

march_24_sundays[2,]

march_24_sundays$date=as.Date(march_24_sundays$Timestamp_Irish)
march_24_sundays$Timestamp_Irish

march_24_sundays

as.Date(march_24_sundays$Timestamp_Irish, format='%Y-%m-%d')
march_24_sundays<- march_24_sundays %>%
  mutate(hour=hour(march_24_sundays$Timestamp_Irish))

hourwise_march_24_sundays <- march_24_sundays %>% group_by(hour) %>%
  summarise(
    mean_no2=mean(NO2)
  )

hourwise_march_24_sundays

no2_patrick_day
march_24_sundays


no2_patrick_day <- no2_patrick_day %>%
  mutate(period = "St Patrick's Day")

march_24_sundays <- march_24_sundays %>%
  mutate(period = "March 2024 Sundays")

spd=no2_patrick_day[1:nrow(no2_patrick_day),c('NO2','Timestamp_Irish', 'hour','date','period')]
other_march=march_24_sundays[1:nrow(march_24_sundays),c('NO2','Timestamp_Irish', 'hour','date','period')]

no2_patrick_day
spd$NO2
other_march$NO2
# Combine both datasets
combined_df <- rbind(spd, other_march)
combined_df

library(purrr)

hourly_tests <- map_df(0:23, function(h) {
  df_hour <- combined_df %>% filter(hour == h)
  
  
  if (n_distinct(df_hour$period) < 2) return(NULL)  # skip if one group missing
  print('wow run till here')
  test <- wilcox.test(NO2 ~ period, data = df_hour, exact = FALSE)
  
  tibble(
    hour = h,
    spd_mean = mean(df_hour$NO2[df_hour$period == "St Patrick's Day"]),
    march_mean = mean(df_hour$NO2[df_hour$period == "March 2024 Sundays"]),
    diff = spd_mean - march_mean,
    p_value = test$p.value
  )
}) %>%
  mutate(
    p_adj = p.adjust(p_value, method = "bonferroni"),
    significant = p_adj < 0.05,
    no2_ratio = spd_mean / march_mean
  )
hourly_tests

print(as.data.frame(hourly_tests)[12:24,])

spd


intervention_test <- combined_df %>%
  filter(hour >= 7, hour < 17)

intervention_result <- wilcox.test(
  NO2 ~ period,
  data = intervention_test,
  exact = FALSE
)

print(intervention_result)

ratio_df <- as.data.frame(list(
  hour=hourwise_patrick_day$hour2,
  no2_ratio=hourwise_patrick_day$mean_no2/hourwise_march_24_sundays$mean_no2
))

ratio_df

plot(ratio_df$no2_ratio,type='l')
plot(hourwise_march_24_sundays$mean_no2,type='l')
plot(hourwise_patrick_day$mean_no2,type='l')
abline(h=1)
abline(h=1.2, lty=2)
abline(h=0.8, lty=2)


library(ggplot2)
hourwise_march_24_sundays$period='Mean NO2 - June 2024'
hourwise_patrick_day$period='Mean NO2 - Open Streets Event, 16 June 2024'

hourwise_march_24_sundays
hourwise_patrick_day
hourwise_patrick_day$hour<- hourwise_patrick_day$hour2
complete_df <- rbind(hourwise_patrick_day[,c("hour","mean_no2","period")], hourwise_march_24_sundays)
complete_df

complete_df<-complete_df %>% arrange(hour)
?sort_by

library(ggplot2)
par(mar=c(6,2,1,2))
ggplot(complete_df, aes(x = hour, y = mean_no2, color = period)) +
  geom_line(size = 1.2) +
  geom_point() +
  labs(title = "Hourly Mean NO2 Levels Comparison",
       x = "Hour", y = "Mean NO2 (µg/m³)",
       color = "Period") +
  scale_x_continuous(breaks = seq(0, 23, by = 2)) +  # customize interval
  coord_cartesian(ylim = c(0, 35)) + 
  theme_minimal()  +
  theme(
    legend.position = c(0.40, 0.90),  # (x, y) in [0, 1] relative to plot area
    legend.background = element_rect(fill = "white", color = "gray80"),
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 9)
  )


#### cpa on ratio data ###########

acf(ratio_df$no2_ratio)
fit <- auto.arima(ratio_df$no2_ratio)
res <- residuals(fit)
acf(res)

cps<-cpt.np(res,method='PELT',penalty='MBIC')
cpts(cps)
ticks <- seq(1,length(ratio_df$hour), by=2)
par(mar=c(6,4,4,2))

plot(cps)






plot(ratio_df$no2_ratio, xaxt="n",xlab="", main='Hourly NO2 Ratio Trends: St. Patrick’s Day and Other March Sundays (2024)', ylab='Ratio', type='l')
axis(1,at = ticks,labels =ratio_df$hour[ticks])
abline(v=cpts(cps)+1, col='red')
mtext('Hour', side=1, line=3)

ratio_df$hour[8]
######### statistical test on the two mean no2s ###########
library(tidyr)
wide_data <- pivot_wider(complete_df, names_from = period, values_from = mean_no2)
wide_data



wide_data$difference <- wide_data[["Mean NO2 - Open Streets Event, 16 June 2024"]] - wide_data[["Mean NO2 - June 2024"]]
summary(wide_data$difference)

library(ggplot2)

ggplot(wide_data, aes(x = hour, y = difference)) +
  geom_bar(stat = "identity", fill = "darkorange") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(
    title = "Hourly Difference in Mean NO2: Open Streets Event - Regular Sundays",
    x = "Hour",
    y = "NO2 Difference (µg/m³)"
  ) +
  scale_x_continuous(breaks = seq(0, 23, by = 2)) +
  theme_minimal()

