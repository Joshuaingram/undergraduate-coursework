# SI Solar Flare Project Data Wrangling

library(tidyverse)
library(VGAM)
library(tm)
library(hash)
library(lubridate)
library(fpp2)
library(zoo)

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

# for loop that goes through equidistant time intervals until time runs out... make this the same for each region, take mean in each



# region 2 - Occurred in Aug. 2004
# start - "2004-08-05 05:26:50 UTC"
# end - "2004-08-20 02:30:46 UTC"
# length - 14.8777314814815
region_2 <- RHESSI[which(RHESSI$RARreg == regions[2]),]

region_2 <- region_2 %>% mutate(date = cut(Rpeak, breaks = 250))

region_2_TE <- region_2 %>% group_by(date) %>% summarise(avg_TE = mean(log(RFlrTotalEnergy)))

start_time <- strptime(region_2[1,]$Rpeak, format = "%Y-%m-%d %H:%M:%S")
end_time <- strptime(region_2[nrow(region_2),]$Rpeak, format = "%Y-%m-%d %H:%M:%S")
length <- difftime(end_time, start_time)

region_2_ts <- ts(region_2_TE$avg_TE)
autoplot(region_2_ts)



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
