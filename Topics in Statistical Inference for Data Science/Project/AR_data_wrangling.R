# SI Solar Flare Project Data Wrangling
library(tidyverse)
library(VGAM)
library(tm)
library(hash)
library(lubridate)
library(fpp2)
library(zoo)
library(reshape)

RHESSI <- read_csv("D:/main/Datasets/Solar_REU/RHESSI_data/RHESSI_clean.csv")
RHESSI$cycle <- as.factor(RHESSI$cycle)

## Some Basic Data Exploration on Active Regions ##

head(RHESSI)

# Flare Counts by Active Region
RHESSI_AR <- RHESSI %>% count(RARreg)

summary(RHESSI_AR)

ggplot(data = RHESSI_AR[which(RHESSI_AR > 0),], aes(x = n)) + geom_histogram(binwidth = 1, color = "gray", fill = "blue") + labs(title = "Distribution of Flare Counts per Active Region", x = "Flare Count per Active Region")

ggplot(data = RHESSI_AR[which(RHESSI_AR > 0),], aes(x = n)) + geom_histogram(binwidth = 1, color = "gray", fill = "blue") + labs(title = "Distribution of Flare Counts per Active Region", x = "Flare Count per Active Region") + scale_y_log10()

ggplot(data = RHESSI_AR[which(RHESSI_AR > 0),], aes(x = n)) + geom_histogram(bins = 30, color = "gray", fill = "blue") + labs(title = "Distribution of Flare Counts per Active Region", x = "Flare Count per Active Region") + scale_x_log10()

ggplot(data = RHESSI_AR[which(RHESSI_AR > 0),], aes(x = n)) + geom_histogram(bins = 30, color = "gray", fill = "blue") + labs(title = "Distribution of Flare Counts per Active Region", x = "Flare Count per Active Region") + scale_x_log10() + scale_y_log10()

# Average Energy release by active regions each month

RHESSI_AR_ave <- RHESSI[which(RHESSI$RARreg > 0),] %>% group_by(RARreg) %>% summarise(mean = mean(RFlrTotalEnergy),
                                                                                      time = median(Rpeak))

RHESSI_AR_ave <- RHESSI_AR_ave[order(RHESSI_AR_ave$time),]

ave_TE_timeseries <- ts(log(RHESSI_AR_ave$mean), frequency = 12)

autoplot(ave_TE_timeseries)
ggplot(data = RHESSI_AR_ave, aes(x = mean)) + geom_histogram()

# Within regions total energy release per flare (top 5)

regions <- RHESSI_AR[which(RHESSI_AR$RARreg > 0 & RHESSI_AR$n > 530),]$RARreg

ggplot(RHESSI[which(RHESSI$RARreg == regions[1]),], aes(x = Rpeak, y = log(RFlrTotalEnergy))) +
  geom_point() + geom_smooth(se = FALSE)

ggplot(RHESSI[which(RHESSI$RARreg == regions[2]),], aes(x = Rpeak, y = log(RFlrTotalEnergy))) +
  geom_point()  + geom_smooth(se = FALSE)

ggplot(RHESSI[which(RHESSI$RARreg == regions[3]),], aes(x = Rpeak, y = log(RFlrTotalEnergy))) +
  geom_point()  + geom_smooth(se = FALSE)

ggplot(RHESSI[which(RHESSI$RARreg == regions[4]),], aes(x = Rpeak, y = log(RFlrTotalEnergy))) +
  geom_point()  + geom_smooth(se = FALSE)

ggplot(RHESSI[which(RHESSI$RARreg == regions[5]),], aes(x = Rpeak, y = log(RFlrTotalEnergy))) +
  geom_point()  + geom_smooth(se = FALSE)


# Creating time series objects for top 5 most active ARs in RHESSI dataset#

regions <- RHESSI_AR[which(RHESSI_AR$RARreg > 0 & RHESSI_AR$n > 530),]$RARreg


# region 1 - Occurred in Jan. 2004
# start - "2004-01-01 00:40:06 UTC"
# end - "2004-01-15 08:18:38 UTC"
# length - 14.3184259259259 days
region_1 <- RHESSI[which(RHESSI$RARreg == regions[1]),]

region_1 <- region_1 %>% mutate(date = cut(Rpeak, breaks = 250))

region_1_TE <- region_1 %>% group_by(date) %>% summarise(avg_TE = mean(log(RFlrTotalEnergy)))

start_time <- strptime(region_1[1,]$Rpeak, format = "%Y-%m-%d %H:%M:%S")
end_time <- strptime(region_1[nrow(region_1),]$Rpeak, format = "%Y-%m-%d %H:%M:%S")
length <- difftime(end_time, start_time)

region_1_ts <- ts(region_1_TE$avg_TE)
autoplot(region_1_ts)

# 150 minute intervals

date <- as.POSIXlt("2004-01-01 00:40:06")
mean_TE <- c()
mean_PK <- c()
mean_DR <- c()
time <- C()

while (date <= as.POSIXlt("2004-01-15 08:18:38")){
  date_1 <- date + minutes(150)
  
  mean_current_te <- mean(region_1[which(region_1$Rpeak < date_1 & region_1$Rpeak >= date),]$RFlrTotalEnergy)
  mean_current_pk <- mean(region_1[which(region_1$Rpeak < date_1 & region_1$Rpeak >= date),]$Rflxpeak)
  mean_current_dur <- mean(region_1[which(region_1$Rpeak < date_1 & region_1$Rpeak >= date),]$Rduration)
  
  # naive interpolation - total energy
  if (is.nan(mean_current_te) == TRUE){
    mean_current_te <- mean_TE[length(mean_TE)]
  } else if (mean_current_te == 0){
    mean_current_te <- mean_TE[length(mean_TE)]
  }
  
  # naive interpolation - peak flux
  if (is.nan(mean_current_pk) == TRUE){
    mean_current_pk <- mean_PK[length(mean_PK)]
  } else if (mean_current_pk == 0){
    mean_current_pk <- mean_PK[length(mean_PK)]
  }
  
  # naive interpolation - duration
  if (is.nan(mean_current_dur) == TRUE){
    mean_current_dur <- mean_DR[length(mean_DR)]
  } else if (mean_current_dur == 0){
    mean_current_dur <- mean_DR[length(mean_DR)]
  }
  
  mean_TE <- c(mean_TE, mean_current_te)
  mean_PK <- c(mean_PK, mean_current_pk)
  mean_DR <- c(mean_DR, mean_current_dur)
  time <- c(time, date_1)
  
  date <- date_1
}

ts_region1_150_te <- ts(log(mean_TE))
autoplot(ts_region1_150_te)
ts_region1_150_te <- as.data.frame(ts_region1_150_te)$x
ts_region1_150[is.nan(ts_region1_150_te)] <- 0

ts_region1_150_pk <- ts(log(mean_PK))
autoplot(ts_region1_150_pk)
ts_region1_150_pk <- as.data.frame(ts_region1_150_pk)$x
ts_region1_150_pk[is.nan(ts_region1_150_pk)] <- 0

ts_region1_150_dur <- ts(log(mean_DR))
autoplot(ts_region1_150_dur)
ts_region1_150_dur <- as.data.frame(ts_region1_150_dur)$x
ts_region1_150_dur[is.nan(ts_region1_150_dur)] <- 0

region1_150 <- data.frame(ts_region1_150_te, ts_region1_150_pk, ts_region1_150_dur)
colnames(region1_150) <- c("totalEnergy", "peakFlux", "duration")

write.csv(region1_150, file = "D:/main/Projects/undergraduate-coursework/Topics in Statistical Inference for Data Science/Project/Data/region1_150.csv")

# 60 minute intervals

date <- as.POSIXlt("2004-01-01 00:40:06")
mean_TE <- c()
mean_PK <- c()
mean_DR <- c()
time <- C()

while (date <= as.POSIXlt("2004-01-15 08:18:38")){
  date_1 <- date + minutes(60)
  
  mean_current_te <- mean(region_1[which(region_1$Rpeak < date_1 & region_1$Rpeak >= date),]$RFlrTotalEnergy)
  mean_current_pk <- mean(region_1[which(region_1$Rpeak < date_1 & region_1$Rpeak >= date),]$Rflxpeak)
  mean_current_dur <- mean(region_1[which(region_1$Rpeak < date_1 & region_1$Rpeak >= date),]$Rduration)
  
  # naive interpolation - total energy
  if (is.nan(mean_current_te) == TRUE){
    mean_current_te <- mean_TE[length(mean_TE)]
  } else if (mean_current_te == 0){
    mean_current_te <- mean_TE[length(mean_TE)]
  }
  
  # naive interpolation - peak flux
  if (is.nan(mean_current_pk) == TRUE){
    mean_current_pk <- mean_PK[length(mean_PK)]
  } else if (mean_current_pk == 0){
    mean_current_pk <- mean_PK[length(mean_PK)]
  }
  
  # naive interpolation - duration
  if (is.nan(mean_current_dur) == TRUE){
    mean_current_dur <- mean_DR[length(mean_DR)]
  } else if (mean_current_dur == 0){
    mean_current_dur <- mean_DR[length(mean_DR)]
  }
  
  mean_TE <- c(mean_TE, mean_current_te)
  mean_PK <- c(mean_PK, mean_current_pk)
  mean_DR <- c(mean_DR, mean_current_dur)
  time <- c(time, date_1)
  
  date <- date_1
}

ts_region1_60_te <- ts(log(mean_TE))
autoplot(ts_region1_60_te)
ts_region1_60_te <- as.data.frame(ts_region1_60_te)$x
ts_region1_60[is.nan(ts_region1_60_te)] <- 0

ts_region1_60_pk <- ts(log(mean_PK))
autoplot(ts_region1_60_pk)
ts_region1_60_pk <- as.data.frame(ts_region1_60_pk)$x
ts_region1_60_pk[is.nan(ts_region1_60_pk)] <- 0

ts_region1_60_dur <- ts(log(mean_DR))
autoplot(ts_region1_60_dur)
ts_region1_60_dur <- as.data.frame(ts_region1_60_dur)$x
ts_region1_60_dur[is.nan(ts_region1_60_dur)] <- 0

region1_60 <- data.frame(ts_region1_60_te, ts_region1_60_pk, ts_region1_60_dur)
colnames(region1_60) <- c("totalEnergy", "peakFlux", "duration")

write.csv(region1_60, file = "D:/main/Projects/undergraduate-coursework/Topics in Statistical Inference for Data Science/Project/Data/region1_60.csv")

# region 2 - Occurred in Aug. 2004
# start - "2004-08-05 05:26:50 UTC"
# end - "2004-08-18 04:30:46 UTC"
# length - 14.8777314814815
region_2 <- RHESSI[which(RHESSI$RARreg == regions[2]),]

region_2 <- region_2 %>% mutate(date = cut(Rpeak, breaks = 250))

region_2_TE <- region_2 %>% group_by(date) %>% summarise(avg_TE = mean(log(RFlrTotalEnergy)))

start_time <- strptime(region_2[1,]$Rpeak, format = "%Y-%m-%d %H:%M:%S")
end_time <- strptime(region_2[nrow(region_2),]$Rpeak, format = "%Y-%m-%d %H:%M:%S")
length <- difftime(end_time, start_time)

region_2_ts <- ts(region_2_TE$avg_TE)
autoplot(region_2_ts)

# 150 minute intervals

date <- as.POSIXlt("2004-08-05 05:26:50 UTC")
mean_TE <- c()
mean_PK <- c()
mean_DR <- c()
time <- c()

while (date <= as.POSIXlt("2004-08-18 04:30:46 UTC")){
  date_1 <- date + minutes(150)
  
  mean_current_te <- mean(region_2[which(region_2$Rpeak < date_1 & region_2$Rpeak >= date),]$RFlrTotalEnergy)
  mean_current_pk <- mean(region_2[which(region_2$Rpeak < date_1 & region_2$Rpeak >= date),]$Rflxpeak)
  mean_current_dur <- mean(region_2[which(region_2$Rpeak < date_1 & region_2$Rpeak >= date),]$Rduration)
  
  # naive interpolation - total energy
  if (is.nan(mean_current_te) == TRUE){
    mean_current_te <- mean_TE[length(mean_TE)]
  } else if (mean_current_te == 0){
    mean_current_te <- mean_TE[length(mean_TE)]
  }
  
  # naive interpolation - peak flux
  if (is.nan(mean_current_pk) == TRUE){
    mean_current_pk <- mean_PK[length(mean_PK)]
  } else if (mean_current_pk == 0){
    mean_current_pk <- mean_PK[length(mean_PK)]
  }
  
  # naive interpolation - duration
  if (is.nan(mean_current_dur) == TRUE){
    mean_current_dur <- mean_DR[length(mean_DR)]
  } else if (mean_current_dur == 0){
    mean_current_dur <- mean_DR[length(mean_DR)]
  }
  
  mean_TE <- c(mean_TE, mean_current_te)
  mean_PK <- c(mean_PK, mean_current_pk)
  mean_DR <- c(mean_DR, mean_current_dur)
  time <- c(time, date_1)
  
  date <- date_1
}

ts_region2_150_te <- ts(log(mean_TE))
autoplot(ts_region2_150_te)
ts_region2_150_te <- as.data.frame(ts_region2_150_te)$x
ts_region2_150[is.nan(ts_region2_150_te)] <- 0

ts_region2_150_pk <- ts(log(mean_PK))
autoplot(ts_region2_150_pk)
ts_region2_150_pk <- as.data.frame(ts_region2_150_pk)$x
ts_region2_150_pk[is.nan(ts_region2_150_pk)] <- 0

ts_region2_150_dur <- ts(log(mean_DR))
autoplot(ts_region2_150_dur)
ts_region2_150_dur <- as.data.frame(ts_region2_150_dur)$x
ts_region2_150_dur[is.nan(ts_region2_150_dur)] <- 0

region2_150 <- data.frame(ts_region2_150_te, ts_region2_150_pk, ts_region2_150_dur)
colnames(region2_150) <- c("totalEnergy", "peakFlux", "duration")

write.csv(region2_150, file = "D:/main/Projects/undergraduate-coursework/Topics in Statistical Inference for Data Science/Project/Data/region2_150.csv")

# 60 minute intervals

date <- as.POSIXlt("2004-08-05 05:26:50 UTC")
mean_TE <- c()
mean_PK <- c()
mean_DR <- c()
time <- c()

while (date <= as.POSIXlt("2004-08-18 04:30:46 UTC")){
  date_1 <- date + minutes(60)
  
  mean_current_te <- mean(region_2[which(region_2$Rpeak < date_1 & region_2$Rpeak >= date),]$RFlrTotalEnergy)
  mean_current_pk <- mean(region_2[which(region_2$Rpeak < date_1 & region_2$Rpeak >= date),]$Rflxpeak)
  mean_current_dur <- mean(region_2[which(region_2$Rpeak < date_1 & region_2$Rpeak >= date),]$Rduration)
  
  # naive interpolation - total energy
  if (is.nan(mean_current_te) == TRUE){
    mean_current_te <- mean_TE[length(mean_TE)]
  } else if (mean_current_te == 0){
    mean_current_te <- mean_TE[length(mean_TE)]
  }
  
  # naive interpolation - peak flux
  if (is.nan(mean_current_pk) == TRUE){
    mean_current_pk <- mean_PK[length(mean_PK)]
  } else if (mean_current_pk == 0){
    mean_current_pk <- mean_PK[length(mean_PK)]
  }
  
  # naive interpolation - duration
  if (is.nan(mean_current_dur) == TRUE){
    mean_current_dur <- mean_DR[length(mean_DR)]
  } else if (mean_current_dur == 0){
    mean_current_dur <- mean_DR[length(mean_DR)]
  }
  
  mean_TE <- c(mean_TE, mean_current_te)
  mean_PK <- c(mean_PK, mean_current_pk)
  mean_DR <- c(mean_DR, mean_current_dur)
  time <- c(time, date_1)
  
  date <- date_1
}

ts_region2_60_te <- ts(log(mean_TE))
autoplot(ts_region2_60_te)
ts_region2_60_te <- as.data.frame(ts_region2_60_te)$x
ts_region2_60[is.nan(ts_region2_60_te)] <- 0

ts_region2_60_pk <- ts(log(mean_PK))
autoplot(ts_region2_60_pk)
ts_region2_60_pk <- as.data.frame(ts_region2_60_pk)$x
ts_region2_60_pk[is.nan(ts_region2_60_pk)] <- 0

ts_region2_60_dur <- ts(log(mean_DR))
autoplot(ts_region2_60_dur)
ts_region2_60_dur <- as.data.frame(ts_region2_60_dur)$x
ts_region2_60_dur[is.nan(ts_region2_60_dur)] <- 0

region2_60 <- data.frame(ts_region2_60_te, ts_region2_60_pk, ts_region2_60_dur)
colnames(region2_60) <- c("totalEnergy", "peakFlux", "duration")

write.csv(region2_60, file = "D:/main/Projects/undergraduate-coursework/Topics in Statistical Inference for Data Science/Project/Data/region2_60.csv")

# region 3 - Occurred in Jan. 2005
# start - "2005-01-10 15:01:17 UTC"
# end - "2005-01-23 20:54:53 UTC"
# length - 13.2455555555556
region_3 <- RHESSI[which(RHESSI$RARreg == regions[3]),]

region_3 <- region_3 %>% mutate(date = cut(RTpeak, breaks = 250))

region_3_TE <- region_3 %>% group_by(date) %>% summarise(avg_TE = mean(log(RFlrTotalEnergy)))

start_time <- strptime(region_3[1,]$Rpeak, format = "%Y-%m-%d %H:%M:%S")
end_time <- strptime(region_3[nrow(region_3),]$Rpeak, format = "%Y-%m-%d %H:%M:%S")
length <- difftime(end_time, start_time)

region_3_ts <- ts(region_3_TE$avg_TE)
autoplot(region_3_ts)

# 150 minute intervals

date <- as.POSIXlt("2005-01-10 15:01:17 UTC")
mean_TE <- c()
mean_PK <- c()
mean_DR <- c()
time <- c()

while (date <= as.POSIXlt("2005-01-23 20:54:53 UTC")){
  date_1 <- date + minutes(150)
  
  mean_current_te <- mean(region_3[which(region_3$Rpeak < date_1 & region_3$Rpeak >= date),]$RFlrTotalEnergy)
  mean_current_pk <- mean(region_3[which(region_3$Rpeak < date_1 & region_3$Rpeak >= date),]$Rflxpeak)
  mean_current_dur <- mean(region_3[which(region_3$Rpeak < date_1 & region_3$Rpeak >= date),]$Rduration)
  
  # naive interpolation - total energy
  if (is.nan(mean_current_te) == TRUE){
    mean_current_te <- mean_TE[length(mean_TE)]
  } else if (mean_current_te == 0){
    mean_current_te <- mean_TE[length(mean_TE)]
  }
  
  # naive interpolation - peak flux
  if (is.nan(mean_current_pk) == TRUE){
    mean_current_pk <- mean_PK[length(mean_PK)]
  } else if (mean_current_pk == 0){
    mean_current_pk <- mean_PK[length(mean_PK)]
  }
  
  # naive interpolation - duration
  if (is.nan(mean_current_dur) == TRUE){
    mean_current_dur <- mean_DR[length(mean_DR)]
  } else if (mean_current_dur == 0){
    mean_current_dur <- mean_DR[length(mean_DR)]
  }
  
  mean_TE <- c(mean_TE, mean_current_te)
  mean_PK <- c(mean_PK, mean_current_pk)
  mean_DR <- c(mean_DR, mean_current_dur)
  time <- c(time, date_1)
  
  date <- date_1
}

ts_region3_150_te <- ts(log(mean_TE))
autoplot(ts_region3_150_te)
ts_region3_150_te <- as.data.frame(ts_region3_150_te)$x
ts_region3_150[is.nan(ts_region3_150_te)] <- 0

ts_region3_150_pk <- ts(log(mean_PK))
autoplot(ts_region3_150_pk)
ts_region3_150_pk <- as.data.frame(ts_region3_150_pk)$x
ts_region3_150_pk[is.nan(ts_region3_150_pk)] <- 0

ts_region3_150_dur <- ts(log(mean_DR))
autoplot(ts_region3_150_dur)
ts_region3_150_dur <- as.data.frame(ts_region3_150_dur)$x
ts_region3_150_dur[is.nan(ts_region3_150_dur)] <- 0

region3_150 <- data.frame(ts_region3_150_te, ts_region3_150_pk, ts_region3_150_dur)
colnames(region3_150) <- c("totalEnergy", "peakFlux", "duration")

write.csv(region3_150, file = "D:/main/Projects/undergraduate-coursework/Topics in Statistical Inference for Data Science/Project/Data/region3_150.csv")

# 60 minute intervals

date <- as.POSIXlt("2005-01-10 15:01:17 UTC")
mean_TE <- c()
mean_PK <- c()
mean_DR <- c()
time <- c()

while (date <= as.POSIXlt("2005-01-23 20:54:53 UTC")){
  date_1 <- date + minutes(60)
  
  mean_current_te <- mean(region_3[which(region_3$Rpeak < date_1 & region_3$Rpeak >= date),]$RFlrTotalEnergy)
  mean_current_pk <- mean(region_3[which(region_3$Rpeak < date_1 & region_3$Rpeak >= date),]$Rflxpeak)
  mean_current_dur <- mean(region_3[which(region_3$Rpeak < date_1 & region_3$Rpeak >= date),]$Rduration)
  
  # naive interpolation - total energy
  if (is.nan(mean_current_te) == TRUE){
    mean_current_te <- mean_TE[length(mean_TE)]
  } else if (mean_current_te == 0){
    mean_current_te <- mean_TE[length(mean_TE)]
  }
  
  # naive interpolation - peak flux
  if (is.nan(mean_current_pk) == TRUE){
    mean_current_pk <- mean_PK[length(mean_PK)]
  } else if (mean_current_pk == 0){
    mean_current_pk <- mean_PK[length(mean_PK)]
  }
  
  # naive interpolation - duration
  if (is.nan(mean_current_dur) == TRUE){
    mean_current_dur <- mean_DR[length(mean_DR)]
  } else if (mean_current_dur == 0){
    mean_current_dur <- mean_DR[length(mean_DR)]
  }
  
  mean_TE <- c(mean_TE, mean_current_te)
  mean_PK <- c(mean_PK, mean_current_pk)
  mean_DR <- c(mean_DR, mean_current_dur)
  time <- c(time, date_1)
  
  date <- date_1
}

ts_region3_60_te <- ts(log(mean_TE))
autoplot(ts_region3_60_te)
ts_region3_60_te <- as.data.frame(ts_region3_60_te)$x
ts_region3_60[is.nan(ts_region3_60_te)] <- 0

ts_region3_60_pk <- ts(log(mean_PK))
autoplot(ts_region3_60_pk)
ts_region3_60_pk <- as.data.frame(ts_region3_60_pk)$x
ts_region3_60_pk[is.nan(ts_region3_60_pk)] <- 0

ts_region3_60_dur <- ts(log(mean_DR))
autoplot(ts_region3_60_dur)
ts_region3_60_dur <- as.data.frame(ts_region3_60_dur)$x
ts_region3_60_dur[is.nan(ts_region3_60_dur)] <- 0

region3_60 <- data.frame(ts_region3_60_te, ts_region3_60_pk, ts_region3_60_dur)
colnames(region3_60) <- c("totalEnergy", "peakFlux", "duration")

write.csv(region3_60, file = "D:/main/Projects/undergraduate-coursework/Topics in Statistical Inference for Data Science/Project/Data/region3_60.csv")

# region 4 - occurred in Sept. 2005
# start - "2005-09-06 13:48:41 UTC"
# end - "2005-09-21 04:09:25 UTC"
# length - 14.5977314814815
region_4 <- RHESSI[which(RHESSI$RARreg == regions[4]),]

region_4 <- region_4 %>% mutate(date = cut(RTpeak, breaks = 250))

region_4_TE <- region_4 %>% group_by(date) %>% summarise(avg_TE = mean(log(RFlrTotalEnergy)))

start_time <- strptime(region_4[1,]$Rpeak, format = "%Y-%m-%d %H:%M:%S")
end_time <- strptime(region_4[nrow(region_4),]$Rpeak, format = "%Y-%m-%d %H:%M:%S")
length <- difftime(end_time, start_time)

region_4_ts <- ts(region_4_TE$avg_TE)
autoplot(region_4_ts)

# 150 minute intervals

date <- as.POSIXlt("2005-09-06 13:48:41 UTC")
mean_TE <- c()
mean_PK <- c()
mean_DR <- c()
time <- c()

while (date <= as.POSIXlt("2005-09-21 04:09:25 UTC")){
  date_1 <- date + minutes(150)
  
  mean_current_te <- mean(region_4[which(region_4$Rpeak < date_1 & region_4$Rpeak >= date),]$RFlrTotalEnergy)
  mean_current_pk <- mean(region_4[which(region_4$Rpeak < date_1 & region_4$Rpeak >= date),]$Rflxpeak)
  mean_current_dur <- mean(region_4[which(region_4$Rpeak < date_1 & region_4$Rpeak >= date),]$Rduration)
  
  # naive interpolation - total energy
  if (is.nan(mean_current_te) == TRUE){
    mean_current_te <- mean_TE[length(mean_TE)]
  } else if (mean_current_te == 0){
    mean_current_te <- mean_TE[length(mean_TE)]
  }
  
  # naive interpolation - peak flux
  if (is.nan(mean_current_pk) == TRUE){
    mean_current_pk <- mean_PK[length(mean_PK)]
  } else if (mean_current_pk == 0){
    mean_current_pk <- mean_PK[length(mean_PK)]
  }
  
  # naive interpolation - duration
  if (is.nan(mean_current_dur) == TRUE){
    mean_current_dur <- mean_DR[length(mean_DR)]
  } else if (mean_current_dur == 0){
    mean_current_dur <- mean_DR[length(mean_DR)]
  }
  
  mean_TE <- c(mean_TE, mean_current_te)
  mean_PK <- c(mean_PK, mean_current_pk)
  mean_DR <- c(mean_DR, mean_current_dur)
  time <- c(time, date_1)
  
  date <- date_1
}

ts_region4_150_te <- ts(log(mean_TE))
autoplot(ts_region4_150_te)
ts_region4_150_te <- as.data.frame(ts_region4_150_te)$x
ts_region4_150[is.nan(ts_region4_150_te)] <- 0

ts_region4_150_pk <- ts(log(mean_PK))
autoplot(ts_region4_150_pk)
ts_region4_150_pk <- as.data.frame(ts_region4_150_pk)$x
ts_region4_150_pk[is.nan(ts_region4_150_pk)] <- 0

ts_region4_150_dur <- ts(log(mean_DR))
autoplot(ts_region4_150_dur)
ts_region4_150_dur <- as.data.frame(ts_region4_150_dur)$x
ts_region4_150_dur[is.nan(ts_region4_150_dur)] <- 0

region4_150 <- data.frame(ts_region4_150_te, ts_region4_150_pk, ts_region4_150_dur)
colnames(region4_150) <- c("totalEnergy", "peakFlux", "duration")

write.csv(region4_150, file = "D:/main/Projects/undergraduate-coursework/Topics in Statistical Inference for Data Science/Project/Data/region4_150.csv")

# 60 minute intervals

date <- as.POSIXlt("2005-09-06 13:48:41 UTC")
mean_TE <- c()
mean_PK <- c()
mean_DR <- c()
time <- c()

while (date <= as.POSIXlt("2005-09-21 04:09:25 UTC")){
  date_1 <- date + minutes(60)
  
  mean_current_te <- mean(region_4[which(region_4$Rpeak < date_1 & region_4$Rpeak >= date),]$RFlrTotalEnergy)
  mean_current_pk <- mean(region_4[which(region_4$Rpeak < date_1 & region_4$Rpeak >= date),]$Rflxpeak)
  mean_current_dur <- mean(region_4[which(region_4$Rpeak < date_1 & region_4$Rpeak >= date),]$Rduration)
  
  # naive interpolation - total energy
  if (is.nan(mean_current_te) == TRUE){
    mean_current_te <- mean_TE[length(mean_TE)]
  } else if (mean_current_te == 0){
    mean_current_te <- mean_TE[length(mean_TE)]
  }
  
  # naive interpolation - peak flux
  if (is.nan(mean_current_pk) == TRUE){
    mean_current_pk <- mean_PK[length(mean_PK)]
  } else if (mean_current_pk == 0){
    mean_current_pk <- mean_PK[length(mean_PK)]
  }
  
  # naive interpolation - duration
  if (is.nan(mean_current_dur) == TRUE){
    mean_current_dur <- mean_DR[length(mean_DR)]
  } else if (mean_current_dur == 0){
    mean_current_dur <- mean_DR[length(mean_DR)]
  }
  
  mean_TE <- c(mean_TE, mean_current_te)
  mean_PK <- c(mean_PK, mean_current_pk)
  mean_DR <- c(mean_DR, mean_current_dur)
  time <- c(time, date_1)
  
  date <- date_1
}

ts_region4_60_te <- ts(log(mean_TE))
autoplot(ts_region4_60_te)
ts_region4_60_te <- as.data.frame(ts_region4_60_te)$x
ts_region4_60[is.nan(ts_region4_60_te)] <- 0

ts_region4_60_pk <- ts(log(mean_PK))
autoplot(ts_region4_60_pk)
ts_region4_60_pk <- as.data.frame(ts_region4_60_pk)$x
ts_region4_60_pk[is.nan(ts_region4_60_pk)] <- 0

ts_region4_60_dur <- ts(log(mean_DR))
autoplot(ts_region4_60_dur)
ts_region4_60_dur <- as.data.frame(ts_region4_60_dur)$x
ts_region4_60_dur[is.nan(ts_region4_60_dur)] <- 0

region4_60 <- data.frame(ts_region4_60_te, ts_region4_60_pk, ts_region4_60_dur)
colnames(region4_60) <- c("totalEnergy", "peakFlux", "duration")

write.csv(region4_60, file = "D:/main/Projects/undergraduate-coursework/Topics in Statistical Inference for Data Science/Project/Data/region4_60.csv")

# region 5 - Occurred in Oct. 2014
# start - "2014-10-16 07:44:33 UTC"
# end - "2014-11-01 10:04:13 UTC"
# length - 16.0969907407407
region_5 <- RHESSI[which(RHESSI$RARreg == regions[5]),]

region_5 <- region_5 %>% mutate(date = cut(RTpeak, breaks = 250))

region_5_TE <- region_5 %>% group_by(date) %>% summarise(avg_TE = mean(log(RFlrTotalEnergy)))

start_time <- strptime(region_5[1,]$Rpeak, format = "%Y-%m-%d %H:%M:%S")
end_time <- strptime(region_5[nrow(region_5),]$Rpeak, format = "%Y-%m-%d %H:%M:%S")
length <- difftime(end_time, start_time)

region_5_ts <- ts(region_5_TE$avg_TE)
autoplot(region_5_ts)

# 150 minute intervals

date <- as.POSIXlt("2014-10-16 07:44:33 UTC")
mean_TE <- c()
mean_PK <- c()
mean_DR <- c()
time <- c()

while (date <= as.POSIXlt("2014-11-01 10:04:13 UTC")){
  date_1 <- date + minutes(150)
  
  mean_current_te <- mean(region_5[which(region_5$Rpeak < date_1 & region_5$Rpeak >= date),]$RFlrTotalEnergy)
  mean_current_pk <- mean(region_5[which(region_5$Rpeak < date_1 & region_5$Rpeak >= date),]$Rflxpeak)
  mean_current_dur <- mean(region_5[which(region_5$Rpeak < date_1 & region_5$Rpeak >= date),]$Rduration)
  
  # naive interpolation - total energy
  if (is.nan(mean_current_te) == TRUE){
    mean_current_te <- mean_TE[length(mean_TE)]
  } else if (mean_current_te == 0){
    mean_current_te <- mean_TE[length(mean_TE)]
  }
  
  # naive interpolation - peak flux
  if (is.nan(mean_current_pk) == TRUE){
    mean_current_pk <- mean_PK[length(mean_PK)]
  } else if (mean_current_pk == 0){
    mean_current_pk <- mean_PK[length(mean_PK)]
  }
  
  # naive interpolation - duration
  if (is.nan(mean_current_dur) == TRUE){
    mean_current_dur <- mean_DR[length(mean_DR)]
  } else if (mean_current_dur == 0){
    mean_current_dur <- mean_DR[length(mean_DR)]
  }
  
  mean_TE <- c(mean_TE, mean_current_te)
  mean_PK <- c(mean_PK, mean_current_pk)
  mean_DR <- c(mean_DR, mean_current_dur)
  time <- c(time, date_1)
  
  date <- date_1
}

ts_region5_150_te <- ts(log(mean_TE))
autoplot(ts_region5_150_te)
ts_region5_150_te <- as.data.frame(ts_region5_150_te)$x
ts_region5_150[is.nan(ts_region5_150_te)] <- 0

ts_region5_150_pk <- ts(log(mean_PK))
autoplot(ts_region5_150_pk)
ts_region5_150_pk <- as.data.frame(ts_region5_150_pk)$x
ts_region5_150_pk[is.nan(ts_region5_150_pk)] <- 0

ts_region5_150_dur <- ts(log(mean_DR))
autoplot(ts_region5_150_dur)
ts_region5_150_dur <- as.data.frame(ts_region5_150_dur)$x
ts_region5_150_dur[is.nan(ts_region5_150_dur)] <- 0

region5_150 <- data.frame(ts_region5_150_te, ts_region5_150_pk, ts_region5_150_dur)
colnames(region5_150) <- c("totalEnergy", "peakFlux", "duration")

write.csv(region5_150, file = "D:/main/Projects/undergraduate-coursework/Topics in Statistical Inference for Data Science/Project/Data/region5_150.csv")

# 60 minute intervals

date <- as.POSIXlt("2014-10-16 07:44:33 UTC")
mean_TE <- c()
mean_PK <- c()
mean_DR <- c()
time <- c()

while (date <= as.POSIXlt("2014-11-01 10:04:13 UTC")){
  date_1 <- date + minutes(60)
  
  mean_current_te <- mean(region_5[which(region_5$Rpeak < date_1 & region_5$Rpeak >= date),]$RFlrTotalEnergy)
  mean_current_pk <- mean(region_5[which(region_5$Rpeak < date_1 & region_5$Rpeak >= date),]$Rflxpeak)
  mean_current_dur <- mean(region_5[which(region_5$Rpeak < date_1 & region_5$Rpeak >= date),]$Rduration)
  
  # naive interpolation - total energy
  if (is.nan(mean_current_te) == TRUE){
    mean_current_te <- mean_TE[length(mean_TE)]
  } else if (mean_current_te == 0){
    mean_current_te <- mean_TE[length(mean_TE)]
  }
  
  # naive interpolation - peak flux
  if (is.nan(mean_current_pk) == TRUE){
    mean_current_pk <- mean_PK[length(mean_PK)]
  } else if (mean_current_pk == 0){
    mean_current_pk <- mean_PK[length(mean_PK)]
  }
  
  # naive interpolation - duration
  if (is.nan(mean_current_dur) == TRUE){
    mean_current_dur <- mean_DR[length(mean_DR)]
  } else if (mean_current_dur == 0){
    mean_current_dur <- mean_DR[length(mean_DR)]
  }
  
  mean_TE <- c(mean_TE, mean_current_te)
  mean_PK <- c(mean_PK, mean_current_pk)
  mean_DR <- c(mean_DR, mean_current_dur)
  time <- c(time, date_1)
  
  date <- date_1
}

ts_region5_60_te <- ts(log(mean_TE))
autoplot(ts_region5_60_te)
ts_region5_60_te <- as.data.frame(ts_region5_60_te)$x
ts_region5_60[is.nan(ts_region5_60_te)] <- 0

ts_region5_60_pk <- ts(log(mean_PK))
autoplot(ts_region5_60_pk)
ts_region5_60_pk <- as.data.frame(ts_region5_60_pk)$x
ts_region5_60_pk[is.nan(ts_region5_60_pk)] <- 0

ts_region5_60_dur <- ts(log(mean_DR))
autoplot(ts_region5_60_dur)
ts_region5_60_dur <- as.data.frame(ts_region5_60_dur)$x
ts_region5_60_dur[is.nan(ts_region5_60_dur)] <- 0

region5_60 <- data.frame(ts_region5_60_te, ts_region5_60_pk, ts_region5_60_dur)
colnames(region5_60) <- c("totalEnergy", "peakFlux", "duration")

write.csv(region5_60, file = "D:/main/Projects/undergraduate-coursework/Topics in Statistical Inference for Data Science/Project/Data/region5_60.csv")
