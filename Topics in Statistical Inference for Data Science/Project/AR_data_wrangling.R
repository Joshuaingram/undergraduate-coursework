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

region_1_ts <- ts(region_1_TE$avg_TE, )
autoplot(region_1_ts)

date <- as.POSIXlt("2004-01-01 00:40:06")
mean_TE <- c()
time <- C()

while (date <= as.POSIXlt("2004-01-15 08:18:38")){
  date_1 <- date + minutes(150)
  
  mean_current <- mean(region_1[which(region_1$Rpeak < date_1 & region_1$Rpeak >= date),]$RFlrTotalEnergy)
  
  mean_TE <- c(mean_TE, mean_current)
  time <- c(time, date_1)
  
  date <- date_1
}

mean_TE["=-Inf"] <- 0
ts_region1 <- ts(log(mean_TE))
autoplot(ts_region1)
ts_region1 <- as.data.frame(ts_region1)$x
ts_region1[is.nan(ts_region1)] <- 0

write.csv(ts_region1, file = "D:/main/Projects/undergraduate-coursework/Topics in Statistical Inference for Data Science/Project/data/region1.csv")

plot()

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

date <- as.POSIXlt("2004-08-05 05:26:50 UTC")
mean_TE <- c()
time <- c()

while (date <= as.POSIXlt("2004-08-18 04:30:46 UTC")){
  date_1 <- date + minutes(150)
  
  mean_current <- mean(region_2[which(region_2$Rpeak < date_1 & region_2$Rpeak >= date),]$RFlrTotalEnergy)
  
  mean_TE <- c(mean_TE, mean_current)
  time <- c(time, date_1)
  
  date <- date_1
}

mean_TE["=-Inf"] <- 0
ts_region2 <- ts(log(mean_TE))
autoplot(ts_region2)
ts_region2 <- as.data.frame(ts_region2)$x
ts_region2[is.nan(ts_region2)] <- 0

write.csv(ts_region2, file = "D:/main/Projects/undergraduate-coursework/Topics in Statistical Inference for Data Science/Project/data/region2.csv")

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

date <- as.POSIXlt("2005-01-10 15:01:17 UTC")
mean_TE <- c()
time <- c()

while (date <= as.POSIXlt("2005-01-23 20:54:53 UTC")){
  date_1 <- date + minutes(150)
  
  mean_current <- mean(region_3[which(region_3$Rpeak < date_1 & region_3$Rpeak >= date),]$RFlrTotalEnergy)
  
  mean_TE <- c(mean_TE, mean_current)
  time <- c(time, date_1)
  
  date <- date_1
}

mean_TE["=-Inf"] <- 0
ts_region3 <- ts(log(mean_TE))
autoplot(ts_region3)
ts_region3 <- as.data.frame(ts_region3)$x
ts_region3[is.nan(ts_region3)] <- 0

write.csv(ts_region3, file = "D:/main/Projects/undergraduate-coursework/Topics in Statistical Inference for Data Science/Project/data/region3.csv")

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

date <- as.POSIXlt("2005-09-06 13:48:41 UTC")
mean_TE <- c()
time <- c()

while (date <= as.POSIXlt("2005-09-21 04:09:25 UTC")){
  date_1 <- date + minutes(150)
  
  mean_current <- mean(region_4[which(region_4$Rpeak < date_1 & region_4$Rpeak >= date),]$RFlrTotalEnergy)
  
  mean_TE <- c(mean_TE, mean_current)
  time <- c(time, date_1)
  
  date <- date_1
}

mean_TE["=-Inf"] <- 0
ts_region4 <- ts(log(mean_TE))
autoplot(ts_region4)
ts_region4 <- as.data.frame(ts_region4)$x
ts_region4[is.nan(ts_region4)] <- 0

write.csv(ts_region4, file = "D:/main/Projects/undergraduate-coursework/Topics in Statistical Inference for Data Science/Project/data/region4.csv")

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

date <- as.POSIXlt("2014-10-16 07:44:33 UTC")
mean_TE <- c()
time <- c()

while (date <= as.POSIXlt("2014-11-01 10:04:13 UTC")){
  date_1 <- date + minutes(150)
  
  mean_current <- mean(region_5[which(region_5$Rpeak < date_1 & region_5$Rpeak >= date),]$RFlrTotalEnergy)
  
  mean_TE <- c(mean_TE, mean_current)
  time <- c(time, date_1)
  
  date <- date_1
}

mean_TE["=-Inf"] <- 0
ts_region5 <- ts(log(mean_TE))
autoplot(ts_region5)
ts_region5 <- as.data.frame(ts_region5)$x
ts_region5[is.nan(ts_region5)] <- 0

write.csv(ts_region5, file = "D:/main/Projects/undergraduate-coursework/Topics in Statistical Inference for Data Science/Project/data/region5.csv")
