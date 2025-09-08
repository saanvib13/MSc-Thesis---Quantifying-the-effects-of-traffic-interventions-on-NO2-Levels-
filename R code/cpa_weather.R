df<-read.csv('D:\\MSc- Data science and analytics\\sem 3 (dissertation)\\mydata\\datasets_for_model\\cleaned\\SPS_cleaned.csv')
df$temp=(df$mintp +df$maxtp)/2

head(df)
df <- df[!(is.na(df$temp)),]
plot(df$temp, type='l')

acf(df$temp)
df$temp
shapiro.test(df$temp)

library(changepoint.np)
library(changepoint)
library(forecast)

fit <- auto.arima(df$temp)
resid <- residuals(fit)

acf(resid)
shapiro.test(resid)

cps<-cpt.np(resid, method='PELT', penalty='MBIC',minseglen =24*30)
cpts(cps)
plot(cps)

cts<-cpts(cps)
if(cts[1]!=1){
  bounds2=c(1, cts, length(df$Timestamp))
} else{
  bounds2=c(cts, length(df$Timestamp))
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
  vals <- na.omit(df$temp)[ bounds2[s] : (bounds2[s + 1]-1) ]
  hrs_ <- df$Timestamp[ bounds2[s] : (bounds2[s + 1]-1) ]
  
  if (length(unique(vals)) <= 1) {
    warning(paste("Skipping segment", s, "due to insufficient variability"))
    next
  }
  
  # bootstrap
  bt   <- boot(vals, boot_mean, R = 5000)
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
mean_diff_threshold <- 0.5  # in NO2 units (e.g., µg/m³)
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


indices <- c(which(df$Timestamp == x[1]))
x<-x[2:length(x)]
df[df$Timestamp=="2022-06-21 06:00:00+01:00",]

for (j in x){
  indices <-c(indices, which(df$Timestamp == j))
}

indices

plot(df$temp, type='l', xlab='Time', ylab='NO2 conc', main='NO2 concentration - 2023/24 Academic Session',ylim=c(-5,30))
abline(v=indices, col='red',lwd=1,lty=2)

merged_segments





###############################
#DIVIDING DATASET ACCORDING TO DIFFERENT SEASONS #

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

