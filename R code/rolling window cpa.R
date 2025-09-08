# cps for OLIVER PLUNKETT STREET (SIMILARLY DONE FOR REST OF THE STREETS)
df <- read.csv("D:\\MSc- Data science and analytics\\sem 3 (dissertation)\\mydata\\datasets_for_model\\cleaned\\OP_cleaned.csv")

# df <- read.csv("D:\\MSc- Data science and analytics\\sem 3 (dissertation)\\mydata\\datasets_for_model\\cleaned\\SPS_cleaned.csv")
# df <- read.csv("D:\\MSc- Data science and analytics\\sem 3 (dissertation)\\mydata\\datasets_for_model\\cleaned\\BS_cleaned.csv")
# df <- read.csv("D:\\MSc- Data science and analytics\\sem 3 (dissertation)\\mydata\\datasets_for_model\\cleaned\\GP_cleaned.csv")
# df <- read.csv("D:\\MSc- Data science and analytics\\sem 3 (dissertation)\\mydata\\datasets_for_model\\cleaned\\UCC_23_24_cleaned.csv")
# df <- read.csv("D:\\MSc- Data science and analytics\\sem 3 (dissertation)\\mydata\\datasets_for_model\\cleaned\\LG_23_24_cleaned.csv")

df[is.na(df$Timestamp),]
head(df)
tail(df)
library(changepoint.np)
library(dplyr)
library(lubridate)
df$Timestamp <- as.POSIXct(df$Timestamp, tz='Europe/Dublin', format='%d-%m-%Y %H:%M')
library(ggplot2)
library(tseries)
library(tidyverse)
library(zoo)
library(rio)
library(boot)
library(forecast)

df <- df %>% arrange(Timestamp)

ts_no2 <- ts(df$NO2)
head(df)
acf(ts_no2)
df$no2_ma_day=ma(df$NO2, order = 24)

conc_ma_day=ts(na.omit(df$NO2), frequency=24)
decomposed_conc <-stl(conc_ma_day,'periodic')
deseasoned_no2 <-seasadj(decomposed_conc)
plot(decomposed_conc)

adf.test(conc_ma_day, alternative='stationary')
Acf(conc_ma_day,main="")
Pacf(conc_ma_day,main="")

conc_d2=diff(deseasoned_no2,difference=2)
plot(conc_d2)
adf.test(conc_d2, alternative='stationary')

Acf(conc_d2, main='ACF for differenced series')
Pacf(conc_d2, main='PACF for differenced series')

auto.arima(deseasoned_no2, seasonal=FALSE)
fit<- auto.arima(deseasoned_no2, seasonal=FALSE)
tsdisplay(residuals(fit), lag.max = 40)

fit2<- arima(deseasoned_no2, order=c(3,1,2))
tsdisplay(residuals(fit2), lag.max = 40)

# ---------- (1) PCA for traffic (optional) ----------
traffic_mat <- scale(df[, c("counter.1", "counter.2")])
pca_result <- prcomp(traffic_mat, center = TRUE, scale. = TRUE)
df$traffic_PC1 <- pca_result$x[, 1]

# ---------- (2) Fit SARIMA once ----------
ts_no2 <- ts(df$NO2, frequency = 24)
fit_sarima <- auto.arima(
  ts_no2,
  seasonal = TRUE,
  xreg = as.matrix(df[, c("traffic_PC1",'temp','wdsp','rhum','msl')]),  # add temp, wind if needed
  stepwise = TRUE
)
checkresiduals(fit_sarima)
df$resids <- residuals(fit_sarima)

# ---------- (3) Bootstrapping helper functions ----------
boot_ci95 <- function(bt) boot.ci(bt, type = "perc")$percent[4:5]
boot_mean <- function(data, i) mean(data[i], na.rm = TRUE)

# ---------- (4) Rolling-window changepoint detection ----------
residuals(fit2)
df$resids =residuals(fit2)
window_days <- 14
step_days <- 7
results <- list()
# q1 <-df[df$Timestamp<='2023-11-14 23:00:00',]

q1 <-df[df$Timestamp<='2024-01-05 23:00:00',]
q2 <- df[((df$Timestamp>'2024-01-05 23:00:00')),]

# q2 <- df[((df$Timestamp>="2023-12-16 00:00:00") & (df$Timestamp<='2024-01-07 23:00:00')),]

q3 <- df[((df$Timestamp>'2024-01-07 23:00:00') & (df$Timestamp<='2024-03-19 23:00:00')),]
q4 <- df[((df$Timestamp>'2024-03-19 23:00:00')),]
head(q1)

start_time <- min(q1$Timestamp)
end_time <- max(q1$Timestamp)

start_time <- min(q2$Timestamp)
end_time <- max(q2$Timestamp)

start_time <- min(q3$Timestamp)
end_time <- max(q3$Timestamp)

start_time <- min(q4$Timestamp)
end_time <- max(q4$Timestamp)

start_time <- min(df$Timestamp)
end_time <- max(df$Timestamp)

start_time
end_time
# +days(window_days)
while (start_time  <= end_time) {
  print(start_time)
  current_end <- start_time + days(window_days)
  if(current_end>end_time){
    current_end=end_time
  }

  window_data <- q2 %>%
    filter(Timestamp >= start_time & Timestamp <= current_end)
  
  seg_tbl <- data.frame()

  if (nrow(window_data) > 4) {
    cps <- cpt.np(window_data$resids, method = "PELT", penalty = "MBIC")
    cts <- cpts(cps)

    if (length(cts) == 0) {
      bounds <- c(1, nrow(window_data))
    } else {
      bounds <- c(1, cts, nrow(window_data))
    }
    

    for (s in seq_len(length(bounds) - 1)) {
      vals <- window_data$NO2[bounds[s]:(bounds[s+1] - 1)]
      hrs_ <- window_data$Timestamp[bounds[s]:(bounds[s+1] - 1)]

      if (length(unique(vals)) > 1) {
        bt <- boot(vals, boot_mean, R = 500) # reduce R for speed
        ci <- boot_ci95(bt)

        seg_tbl <- rbind(seg_tbl, data.frame(
        Window_Start = start_time,
        Window_End   = current_end,
        Segment      = s,
        Start        = hrs_[1],
        End          = hrs_[length(hrs_)],
        Mean         = mean(vals, na.rm = TRUE),
        CI_low       = ci[1],
        CI_high      = ci[2]
        ))
        }
      }
      results[[length(results) + 1]] <- seg_tbl
  } else if (nrow(window_data) > 0) {
    # Too few points → treat as one segment
    vals <- window_data$NO2[(window_data$Timestamp>=start_time & window_data$Timestamp<=current_end)]
    hrs_ <- window_data$Timestamp[(window_data$Timestamp>=start_time & window_data$Timestamp<=current_end)]
    
    if (length(unique(vals)) > 1) {
      bt <- boot(vals, boot_mean, R = 500) # reduce R for speed
      ci <- boot_ci95(bt)
    seg_tbl <- data.frame(
      Window_Start = start_time,
      Window_End   = current_end,
      Segment      = 1,
      Start        = min(window_data$Timestamp),
      End          = max(window_data$Timestamp),
      Mean         = mean(window_data$NO2, na.rm = TRUE),
      CI_low       = ci[1],
      CI_high      = ci[2]
    )
    results[[length(results) + 1]] <- seg_tbl
  }}
  
  if((start_time + days(step_days))<end_time){
    start_time <- start_time + days(step_days)
  } else {
    break
  }

  # if(end_time-start_time<days(step_days)){
  #   step_days=end_time-start_time
  # }
}


# ---------- (5) Combine all windows ----------
cp_df <- bind_rows(results) %>% arrange(Start)

# ---------- (6) (Optional) Merge redundant segments within each window ----------
merge_segments <- function(seg_tbl, mean_diff_threshold = 1) {
  seg_tbl <- seg_tbl %>% arrange(Start)
  
  if (nrow(seg_tbl) == 1) {
    seg_tbl$mean_CI_nonoverlap <- NA
    seg_tbl$MergeGroup <- 1
    return(seg_tbl)
  }
  
  seg_tbl$mean_CI_nonoverlap <- c(NA, vapply(2:nrow(seg_tbl), function(i) {
    ci1 <- seg_tbl[i, c("CI_low", "CI_high")]
    ci0 <- seg_tbl[i-1, c("CI_low", "CI_high")]
    !(ci1$CI_low <= ci0$CI_high && ci0$CI_low <= ci1$CI_high)
  }, logical(1)))
  
  merge_groups <- integer(nrow(seg_tbl))
  group_id <- 1
  merge_groups[1] <- group_id
  
  for (i in 2:nrow(seg_tbl)) {
    mean_diff <- abs(seg_tbl$Mean[i] - seg_tbl$Mean[i-1])
    if ((mean_diff <= mean_diff_threshold) & (!seg_tbl$mean_CI_nonoverlap[i])) {
      merge_groups[i] <- group_id
    } else {
      group_id <- group_id + 1
      merge_groups[i] <- group_id
    }
  }
  
  seg_tbl <- seg_tbl %>% mutate(MergeGroup = merge_groups)
  
  return(seg_tbl)
}


View(cp_df)
result1=cp_df
result1=rbind(result1,cp_df)

View(result1)

# Now call safely:
merged_results <- result1 %>%
  group_split(Window_Start, Window_End) %>%
  lapply(merge_segments) %>%
  bind_rows()
View(merged_results)
write.csv(merged_results, "D:\\MSc- Data science and analytics\\sem 3 (dissertation)\\mydata\\datasets_for_model\\OP_14_window_size_7_slide.csv")
