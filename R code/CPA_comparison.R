library(dplyr)
library(forecast)
library('ggplot2')
library(rio)
library(tidyverse)
library(forecast)
library(tseries)
r1=read.csv("D:\\MSc- Data science and analytics\\sem 3 (dissertation)\\mydata\\datasets_for_model\\cleaned\\SPS_cleaned.csv")
# r1<-r1[((r1$weekday_iso!=6) & (r1$weekday_iso!=7)),]
r1$Timestamp <- as.POSIXct(r1$Timestamp, tz='Europe/Dublin', format='%d-%m-%Y %H:%M')
nrow(r1)
head(r1)
r1[is.na(r1$Timestamp),]
r1[is.na(r1$wdsp),]
unique(r1$NO2)

acf(r1$wdsp)
# Create ts object and fit ARIMA
ts_no2 <- ts(r1$wdsp)

traffic<-r1$counter.2
# fit <- auto.arima(ts_no2, seasonal = TRUE)
# checkresiduals(fit)
# 
# 
# fit_sarima <- Arima(ts_no2, order = c(5,1,2))  # adjust as needed

fit_auto <- auto.arima(
  ts_no2,
  seasonal  = TRUE,
  stepwise  = FALSE,    # more thorough search
  approximation = FALSE # exact AICc
)
checkresiduals(fit_auto)



# checkresiduals(fit_sarima)
head(r1)
# ddd <- r1[,c('date','NO2','hour','Timestamp')]
# head(ddd)
# ddd <- ddd %>%
#   mutate(
#     hour_seg = ifelse(hour <= 6, '0-6',
#                       ifelse(hour <= 12, '7-12', ifelse(hour<=18, '13-18', '19-23')))
#   )
# 
# daily_no2 <- ddd %>%
#   group_by(date,hour_seg) %>%
#   summarize(
#     no2=mean(NO2)
#   ) 
# 
# daily_no2 <- daily_no2 %>%
#   mutate(
#     interval=date + " " + hour_seg
#   )
# head(daily_no2)
# nrow(daily_no2)
# acf(daily_no2$no2)
fit2 <- auto.arima(ts_no2)
checkresiduals(fit2)

fit_auto2 <- auto.arima(
  ts_no2,
  seasonal  = TRUE,
  stepwise  = FALSE,    # more thorough search
  approximation = FALSE # exact AICc
)
checkresiduals(fit_auto2)




library(changepoint.np)
cps <-cpt.np(residuals(fit_auto), method='PELT', penalty='MBIC', minseglen = 24*30)
cpts(cps)

plot(ts_no2,type='l', ylab='NO2 concentration - PELT MBIC')
abline(v=cpts(cps), col='red', lwd=2, lty=2)

index <- 1:length(ts_no2)

plot(index, ts_no2, type = "l", ylab = "NO2 Conc", xlab = "Time", xaxt = "n", main='Detected changepoints with PELT MBIC')

# Add x-axis labels at specific points
axis_ticks <- seq(1, length(index), length.out = 20)  # choose ~6 labels
axis_labels <- format(r1$Timestamp[axis_ticks], "%b-%Y")

axis(1, at = axis_ticks, labels = axis_labels)

abline(v = index[cpts(cps)], col = "red", lwd = 2, lty = 1)

r1$date[cpts(cps)]
r1$Timestamp[cpts(cps)]

cts<- cpts(cps)
cts
if(cts[1]!=1){
  bounds2=c(1, cts, length(r1$Timestamp))
}else{
  bounds2=c(cts, length(r1$Timestamp)) 
}

bounds2

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
seq_len(length(bounds2)-1)
for (s in seq_len(length(bounds2)-1)) {
  vals <- ts_no2[ bounds2[s] : (bounds2[s + 1]-1) ]
  hrs_ <- r1$Timestamp[ bounds2[s] : (bounds2[s + 1]-1) ]
  
  if (length(unique(vals)) <= 1) {
    warning(paste("Skipping segment", s, "due to insufficient variability"))
    next
  }
  
  # bootstrap
  bt   <- boot(vals, boot_mean, R = 1000)
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

mean_diff_threshold <- 1 # in NO2 units (e.g., µg/m³)
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
  if((mean_diff<=mean_diff_threshold) & (!segments_dt$mean_CI_nonoverlap[i])) {
    merge_groups[i] <- group_id
  } else {
    group_id <- group_id + 1
    merge_groups[i] <- group_id
  }
}
merge_groups
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

merged_segments

x<-merged_segments$Start
y <-merged_segments$End

# x<-rbind(x,seg_tbl$Start[(seg_tbl$mean_CI_nonoverlap)])

x
y
indices <- c(which(r1$Timestamp == x[1]))
indices_end <- c(which(r1$Timestamp == y[1]))
x<-x[2:length(x)]
y<-y[2:length(y)]
df[df$Timestamp=="2022-06-21 06:00:00+01:00",]


for (j in x){
  indices <-c(indices, which(r1$Timestamp == j))
}

for (j in y){
  indices_end <-c(indices_end, which(r1$Timestamp == j))
}

indices
indices_end
ticks <- seq(1, nrow(r1), by = 15)

merged_segments

mseg_df <- as.data.frame(list(
  Start=r1$Timestamp[indices],
  End=r1$Timestamp[indices_end],
  Mean=merged_segments$Mean)
)
mseg_df

# plot(ts_no2, ylab=expression("NO"[2]*" conc ("*mu*"g/"*m^3*")"),xaxt = "n", main='Detected Changepoints - PELT MBIC', col='grey45')
# 
# axis_ticks <- seq(1, length(index), length.out = 20)  # choose ~6 labels
# axis_labels <- format(r1$Timestamp[axis_ticks], "%b-%Y")
# 
# axis(1, at = axis_ticks, labels = axis_labels)
# abline(v=indices_end, col='red', lwd=2,lty=1)
# for (i in 1:nrow(mseg_df)) {
#   # Convert POSIXct to row indices
#   start_idx <- which(r1$Timestamp == mseg_df$Start[i])
#   end_idx   <- which(r1$Timestamp == mseg_df$End[i])
#   mean_val  <- mseg_df$Mean[i]
#   
#   # Horizontal line for segment mean
#   segments(x0 = start_idx, y0 = mean_val,
#            x1 = end_idx, y1 = mean_val,
#            col = "green", lwd = 2, lty = 2)
#   
#   # Midpoint index for label
#   mid_idx <- floor((start_idx + end_idx) / 2)
#   
#   text(x = mid_idx, y = mean_val + 4,
#        labels = round(mean_val, 1), col = "", cex = 1)
# }

plot(ts_no2, 
     ylab = "Windspeed (knots)",
     xaxt = "n", 
     main = 'Detected Changepoints In Windspeed', 
     col = 'grey25')

# custom x-axis
axis_ticks <- seq(1, length(index), length.out = 20)
axis_labels <- format(r1$Timestamp[axis_ticks], "%b-%Y")
axis(1, at = axis_ticks, labels = axis_labels)

# changepoint vertical lines
abline(v = indices_end, col = 'red', lwd = 2, lty = 1)

# --- helper function to add text with background ---
text_with_bg <- function(x, y, labels, col="black", bg="yellow", cex=1, padding=0.2, ...) {
  w <- strwidth(labels, cex=cex)
  h <- strheight(labels, cex=cex)
  
  # draw rectangle behind text
  rect(x - w/2 - padding, y - h/2 - padding,
       x + w/2 + padding, y + h/2 + padding,
       col = bg, border = NA)
  
  # draw text on top
  text(x, y, labels=labels, col=col, cex=cex, ...)
}

# add horizontal mean lines + labels
for (i in 1:nrow(mseg_df)) {
  # Convert POSIXct to row indices
  start_idx <- which(r1$Timestamp == mseg_df$Start[i])
  end_idx   <- which(r1$Timestamp == mseg_df$End[i])
  mean_val  <- mseg_df$Mean[i]
  
  # horizontal line at mean
  segments(x0 = start_idx, y0 = mean_val,
           x1 = end_idx,   y1 = mean_val,
           col = "green", lwd = 2, lty = 2)
  
  # midpoint for label
  mid_idx <- floor((start_idx + end_idx) / 2)
  
  # text label with background
  text_with_bg(mid_idx, mean_val + 2,
               labels = round(mean_val, 1),
               col = "blue", bg = "yellow", cex = 0.7)
}

tail(r1)


##################################################################################

head(r1)
r1$date <- as.Date(r1$date, tz='Europe/Dublin', format="%Y-%m-%d")

spd <- r1[((r1$date>=as.Date('2024-03-11')) & (r1$date<=as.Date('2024-03-24'))),]
ts_no2 <- ts(spd$NO2)
fit_auto3 <- auto.arima(
  spd$NO2,
  seasonal  = TRUE,
  stepwise  = FALSE,    # more thorough search
  approximation = FALSE # exact AICc
)
checkresiduals(fit_auto3)

library(changepoint.np)
cps <-cpt.np(residuals(fit_auto3), method='PELT', penalty='BIC', minseglen = 12)
cpts(cps)

plot(spd$NO2,type='l')
abline(v=cpts(cps), col='red', lwd=2, lty=2)

spd$date[cpts(cps)]
spd$Timestamp[cpts(cps)]

cts<- cpts(cps)
cts
if(cts[1]!=1){
  bounds2=c(1, cts, length(spd$Timestamp))
}else{
  bounds2=c(cts, length(spd$Timestamp)) 
}

bounds2

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
seq_len(length(bounds2)-1)
for (s in seq_len(length(bounds2)-1)) {
  vals <- ts_no2[ bounds2[s] : (bounds2[s + 1]-1) ]
  hrs_ <- spd$Timestamp[ bounds2[s] : (bounds2[s + 1]-1) ]
  
  if (length(unique(vals)) <= 1) {
    warning(paste("Skipping segment", s, "due to insufficient variability"))
    next
  }
  
  # bootstrap
  bt   <- boot(vals, boot_mean, R = 1000)
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


# library(strucchange)
# ts_no2=ts(daily_no2$no2)
# # ts_no2 is your time series object, hourly NO2 levels
# break_model <- breakpoints(ts_no2 ~ 1, h=10)  # Intercept-only model (mean shifts)
# 
# # View results
# summary(break_model)
# plot(break_model)
# 
# # Add confidence intervals
# confint(break_model)
# 
# # Overlay on time series
# plot(ts_no2)
# lines(break_model, col='red')
# break_model$breakpoints
# daily_no2[break_model$breakpoints,c('date','no2', 'hour_seg')]



library(changepointGA)
ts_no2<-ts(r1$wdsp)
# Your series
N <- length(ts_no2)
N
# 1) Define the AR/MA order search space
p.range <- list(ar = 0:2, ma = 0:2)

# 2) Set up GA control parameters
GA_param <- list(
  popsize      = 50,        # population size
  Pcrossover   = 0.9,       # crossover probability
  Pmutation    = 0.2,       # mutation probability
  Pchangepoint = 30 / N,    # initial chance of a changepoint gene
  minDist      = 6,         # minimum distance between CPs
  mmax         = floor(N/2) - 1,
  lmax         = 2 + floor(N/2) - 1,
  maxgen       = 70,       # max generations
  maxconv      = 40,        # stop if no improvement
  option       = "both",    # allow adding/removing CPs
  monitoring   = TRUE,
  parallel     = FALSE,
  nCore        = NULL,
  tol          = 1e-5
)

# 3) Use the built‑in default GA operators
#    (you can customize these, but the defaults work fine)
operators <- list(
  population = random_population(popsize = GA_param$popsize,
                                 prange    = p.range,
                                 N         = N,
                                 minDist   = GA_param$minDist,
                                 Pb        = GA_param$Pchangepoint,
                                 mmax      = GA_param$mmax,
                                 lmax      = GA_param$lmax),
  selection  = "selection_linearrank",
  crossover  = "uniformcrossover",
  mutation   = "mutation"
)

# 4) Run the GA with an ARIMA‐BIC fitness
res <- GA(
  ObjFunc     = ARIMA.BIC.Order,  # ARIMA+BIC cost per segment
  N           = N,
  GA_param    = GA_param,
  GA_operators= operators,
  p.range     = p.range,
  XMat        = matrix(1, nrow = N, ncol = 1),  # no extra regressors
  Xt          = ts_no2
)

# Inspect
result<-res$overbestchrom   # best “chromosome” (k, p, q, τ₁,…,τ_k, N+1)
res$overbestfit     # its penalized BIC
result
cps<- result[4:length(result)]
cps
plot(ts_no2, ylab='Traffic count', type='l')
abline(v=cps, col='red', lty=2, lwd=2)

index <- 1:length(traffic)

plot(index, traffic, type = "l", ylab = "Traffic Counts", xlab = "Time", xaxt = "n")

# Add x-axis labels at specific points
axis_ticks <- seq(1, length(index), length.out = 20)  # choose ~6 labels
axis_labels <- format(r1$Timestamp[axis_ticks], "%b-%Y")

axis(1, at = axis_ticks, labels = axis_labels)

abline(v = index[cps], col = "red", lwd = 2, lty = 2)

r1$Timestamp[cps]

if(cps[1]!=1){
  bounds2=c(1, cps, length(r1$Timestamp))
}else{
  bounds2=c(cps, length(r1$Timestamp)) 
}

bounds2

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
seq_len(length(bounds2)-1)
for (s in seq_len(length(bounds2)-1)) {
  vals <- ts_no2[ bounds2[s] : (bounds2[s + 1]-1) ]
  hrs_ <- r1$Timestamp[ bounds2[s] : (bounds2[s + 1]-1) ]
  
  if (length(unique(vals)) <= 1) {
    warning(paste("Skipping segment", s, "due to insufficient variability"))
    next
  }
  
  # bootstrap
  bt   <- boot(vals, boot_mean, R = 1000)
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




#####################################################################################################

spd$Timestamp[cps]

if(cps[1]!=1){
  bounds2=c(1, cps[-length(cps)], length(spd$Timestamp))
}else{
  bounds2=c(cps[-length(cps)], length(spd$Timestamp)) 
}

bounds2

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
seq_len(length(bounds2)-1)
for (s in seq_len(length(bounds2)-1)) {
  vals <- ts_no2[ bounds2[s] : (bounds2[s + 1]-1) ]
  hrs_ <- spd$Timestamp[ bounds2[s] : (bounds2[s + 1]-1) ]
  
  if (length(unique(vals)) <= 1) {
    warning(paste("Skipping segment", s, "due to insufficient variability"))
    next
  }
  
  # bootstrap
  bt   <- boot(vals, boot_mean, R = 1000)
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
  if((mean_diff<=mean_diff_threshold) & (!segments_dt$mean_CI_nonoverlap[i])) {
    merge_groups[i] <- group_id
  } else {
    group_id <- group_id + 1
    merge_groups[i] <- group_id
  }
}
merge_groups
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

merged_segments

x<-merged_segments$Start
y <-merged_segments$End

# x<-rbind(x,seg_tbl$Start[(seg_tbl$mean_CI_nonoverlap)])

x
y
indices <- c(which(r1$Timestamp == x[1]))
indices_end <- c(which(r1$Timestamp == y[1]))
x<-x[2:length(x)]
y<-y[2:length(y)]
df[df$Timestamp=="2022-06-21 06:00:00+01:00",]


for (j in x){
  indices <-c(indices, which(r1$Timestamp == j))
}

for (j in y){
  indices_end <-c(indices_end, which(r1$Timestamp == j))
}

indices
indices_end
ticks <- seq(1, nrow(r1), by = 15)

merged_segments

mseg_df <- as.data.frame(list(
  Start=r1$Timestamp[indices],
  End=r1$Timestamp[indices_end][1:14],
  Mean=merged_segments$Mean[1:14])
)
mseg_df


plot(ts_no2, 
     ylab = expression("NO"[2]*" conc ("*mu*"g/"*m^3*")"), 
     xaxt = "n", 
     main = 'Detected Changepoints - PELT GA', 
     col = 'grey25')

# custom x-axis
axis_ticks <- seq(1, length(index), length.out = 20)
axis_labels <- format(r1$Timestamp[axis_ticks], "%b-%Y")
axis(1, at = axis_ticks, labels = axis_labels)

# changepoint vertical lines
abline(v = indices_end, col = 'red', lwd = 1.5, lty = 1)

# --- helper function to add text with background ---
text_with_bg <- function(x, y, labels, col="black", bg="yellow", cex=1, padding=0.2, ...) {
  w <- strwidth(labels, cex=cex)
  h <- strheight(labels, cex=cex)
  
  # draw rectangle behind text
  rect(x - w/2 - padding, y - h/2 - padding,
       x + w/2 + padding, y + h/2 + padding,
       col = bg, border = NA)
  
  # draw text on top
  text(x, y, labels=labels, col=col, cex=cex, ...)
}

# add horizontal mean lines + labels
for (i in 1:nrow(mseg_df)) {
  # Convert POSIXct to row indices
  start_idx <- which(r1$Timestamp == mseg_df$Start[i])
  end_idx   <- which(r1$Timestamp == mseg_df$End[i])
  mean_val  <- mseg_df$Mean[i]
  
  # horizontal line at mean
  segments(x0 = start_idx, y0 = mean_val,
           x1 = end_idx,   y1 = mean_val,
           col = "green", lwd = 2, lty = 2)
  
  # midpoint for label
  mid_idx <- floor((start_idx + end_idx) / 2)
  
  # text label with background
  text_with_bg(mid_idx, mean_val + 4,
               labels = round(mean_val, 1),
               col = "blue", bg = "yellow", cex = 0.7)
}


plot(ts_no2, ylab='Windspeed (knots)')
abline(v=indices_end, col='red', lwd=2,lty=2)
tail(r1)
