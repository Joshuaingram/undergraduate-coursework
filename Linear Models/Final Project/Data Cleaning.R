####################################################################################################
# LendingClub Loan Data Cleaning for Economics Thesis and Linear Models Final Project              #
#                                                                                                  #
# Joshua D. Ingram                                                                                 #
#                                                                                                  #
# 05/07/2020                                                                                       #
####################################################################################################

# Libraries being used
library(tidyverse)
library(readr)
library(zoo)

# setting seed for reproducibility
set.seed(1)

###########################
# Importing Original Data #
###########################

# Loading in all the .csv files given by LendingClub (by year and/or year, quarter)
# 2007-2011
LCD_07_11 <- read.csv("D:/main/Datasets/LendingClub Loans/Separated/2007_2011.csv")
# 2012-2013
LCD_12_13 <- read.csv("D:/main/Datasets/LendingClub Loans//Separated/2012_2013.csv")
# 2014
LCD_14 <- read.csv("D:/main/Datasets/LendingClub Loans/Separated/2014.csv")
# 2015
LCD_15 <- read.csv("D:/main/Datasets/LendingClub Loans/Separated/2015.csv")
# Q1 2016
LCD_Q1_16 <- read.csv("D:/main/Datasets/LendingClub Loans/Separated/Q1_2016.csv")
# Q2 2016
LCD_Q2_16 <- read.csv("D:/main/Datasets/LendingClub Loans/Separated/Q2_2016.csv")
# Q3 2016
LCD_Q3_16 <- read.csv("D:/main/Datasets/LendingClub Loans/Separated/Q3_2016.csv")
# Q4 2016
LCD_Q4_16 <- read.csv("D:/main/Datasets/LendingClub Loans/Separated/Q4_2016.csv")
# Q1 2017
LCD_Q1_17 <- read.csv("D:/main/Datasets/LendingClub Loans/Separated/Q1_2017.csv")
# Q2 2017
LCD_Q2_17 <- read.csv("D:/main/Datasets/LendingClub Loans/Separated/Q2_2017.csv")
# Q3 2017
LCD_Q3_17 <- read.csv("D:/main/Datasets/LendingClub Loans/Separated/Q3_2017.csv")
# Q4 2017
LCD_Q4_17 <- read.csv("D:/main/Datasets/LendingClub Loans/Separated/Q4_2017.csv")
# Q1 2018
LCD_Q1_18 <- read.csv("D:/main/Datasets/LendingClub Loans/Separated/Q1_2018.csv")
# Q2 2018
LCD_Q2_18 <- read.csv("D:/main/Datasets/LendingClub Loans/Separated/Q2_2018.csv")
# Q3 2018
LCD_Q3_18 <- read.csv("D:/main/Datasets/LendingClub Loans/Separated/Q3_2018.csv")
# Q4 2018
LCD_Q4_18 <- read.csv("D:/main/Datasets/LendingClub Loans/Separated/Q4_2018.csv")
# Q1 2019
LCD_Q1_19 <- read.csv("D:/main/Datasets/LendingClub Loans/Separated/Q1_2019.csv")
# Q2 2019
LCD_Q2_19 <- read.csv("D:/main/Datasets/LendingClub Loans/Separated/Q2_2019.csv")
# Q3 2019
LCD_Q3_19 <- read.csv("D:/main/Datasets/LendingClub Loans/Separated/Q3_2019.csv")
# Q4 2019
LCD_Q4_19 <- read.csv("D:/main/Datasets/LendingClub Loans/Separated/Q4_2019.csv")
# Q1 2020
LCD_Q1_20 <- read.csv("D:/main/Datasets/LendingClub Loans/Separated/Q1_2020.csv")

# Combining .csv files into one dataset to work with
LCD <- rbind(LCD_07_11, LCD_12_13, LCD_14, LCD_15, LCD_Q1_16, LCD_Q2_16, LCD_Q3_16, LCD_Q4_16, 
             LCD_Q1_17, LCD_Q2_17, LCD_Q3_17, LCD_Q4_17, LCD_Q1_18, LCD_Q2_18, LCD_Q3_18, LCD_Q4_18,
             LCD_Q1_19, LCD_Q2_19, LCD_Q3_19, LCD_Q4_19, LCD_Q1_20)

# Saving the full dataset as a .csv file
write.csv(LCD, "LendingClubData.csv")

# Reading in the new full dataset
LCD <- as.data.frame(read_csv("D:/main/Datasets/LendingClub Loans/LendingClubData.csv"))

# number of rows: 2,881,089
nrow(LCD)

# number of variables (-1 cause of the X1 variable): 150
ncol(LCD) - 1

# For now, here's a new dataframe that I can work with in case mistakes are made
LCD_mutate <- LCD

####################################################################
# Let's make sure our variables of interest are of the right class #
####################################################################

# issue_d from character to zoo yearmon class
LCD_mutate$issue_d <- as.yearmon(LCD_mutate$issue_d, "%b-%y")

# int_rate from character to numeric
LCD_mutate$int_rate <- as.numeric(gsub("%","", LCD_mutate$int_rate))

# grade from character to factor
LCD_mutate$grade <- factor(LCD_mutate$grade)

# sub_grade from character to factor
LCD_mutate$sub_grade <- factor(LCD_mutate$sub_grade)

# addr_state from character to factor
LCD_mutate$addr_state <- factor(LCD_mutate$addr_state)

# application_type from character to factor
LCD_mutate$application_type <- factor(LCD_mutate$application_type)

# earliest_cr_line from character to zoo yearmon class
LCD_mutate$earliest_cr_line <- as.yearmon(LCD_mutate$earliest_cr_line, "%b-%y")

# emp_length from character to factor
LCD_mutate$emp_length <- factor(LCD_mutate$emp_length)

# home_ownership from character to factor
LCD_mutate$home_ownership <- factor(LCD_mutate$home_ownership)

# last_pymnt_d from charactor to zoo yearmon class
LCD_mutate$last_pymnt_d <- as.yearmon(LCD_mutate$last_pymnt_d, "%b-%y")

# last_credit_pull_d from character to zoo yearmon class
LCD_mutate$last_credit_pull_d <- as.yearmon(LCD_mutate$last_pymnt_d, "%b-%y")

# loan_status from character to factor
LCD_mutate$loan_status <- factor(LCD_mutate$loan_status)

# pymnt_plan from character to factor
LCD_mutate$pymnt_plan <- factor(LCD_mutate$pymnt_plan)

# verification_status from character to factor
LCD_mutate$verification_status <- factor(LCD_mutate$verification_status)

# term from character to factor
LCD_mutate$term <- factor(LCD_mutate$term)

# verification_status_joint from character to factor
LCD_mutate$verification_status_joint <- factor(LCD_mutate$verification_status_joint)

#########################################
# Removing Variables that won't be used #
#########################################

LCD_mutate <- LCD_mutate[,names(LCD_mutate) %in% c("loan_amnt", "funded_amnt", "int_rate", "grade", "sub_grade", "issue_d", "acc_now_delinq", 
                                             "addr_state", "all_util","annual_inc", "annual_inc_joint", "application_type", 
                                             "avg_cur_bal", "dti", "dti_joint", "earliest_cr_line", "emp_length", "fico_range_high",
                                             "fico_range_low", "funded_amnt_inv", "home_ownership", "id",
                                             "installment", "last_credit_pull_d", "last_pymnt_amnt", "loan_status", "max_bal_bc", "mort_acc",
                                             "pub_rec_bankruptcies", "revol_bal", "revol_util", "term", "verification_status")]

##########################################################
# Checking and Changing factor leveles to be more usable #
##########################################################

# application_type
levels(LCD_mutate$application_type)
# from Individual and Joint App to individual and joint
levels(LCD_mutate$application_type)[levels(LCD_mutate$application_type)=="Individual"] <- "Individual"
levels(LCD_mutate$application_type)[levels(LCD_mutate$application_type)=="Joint App"] <- "Joint"

# grade
levels(LCD_mutate$grade)
# fine

# sub_grad
levels(LCD_mutate$sub_grade)
# fine

# emp_length
levels(LCD_mutate$emp_length)
# changing from < 1, 1...9, 10+, n/a to 0 (<1), 1, 2...., 10 (10+), and NA
levels(LCD_mutate$emp_length)[levels(LCD_mutate$emp_length)=="< 1 year"] <- "0"
levels(LCD_mutate$emp_length)[levels(LCD_mutate$emp_length)=="1 year"] <- "1"
levels(LCD_mutate$emp_length)[levels(LCD_mutate$emp_length)=="2 years"] <- "2"
levels(LCD_mutate$emp_length)[levels(LCD_mutate$emp_length)=="3 years"] <- "3"
levels(LCD_mutate$emp_length)[levels(LCD_mutate$emp_length)=="4 years"] <- "4"
levels(LCD_mutate$emp_length)[levels(LCD_mutate$emp_length)=="5 years"] <- "5"
levels(LCD_mutate$emp_length)[levels(LCD_mutate$emp_length)=="6 years"] <- "6"
levels(LCD_mutate$emp_length)[levels(LCD_mutate$emp_length)=="7 years"] <- "7"
levels(LCD_mutate$emp_length)[levels(LCD_mutate$emp_length)=="8 years"] <- "8"
levels(LCD_mutate$emp_length)[levels(LCD_mutate$emp_length)=="9 years"] <- "9"
levels(LCD_mutate$emp_length)[levels(LCD_mutate$emp_length)=="10+ years"] <- "10"
levels(LCD_mutate$emp_length)[levels(LCD_mutate$emp_length)=="n/a"] <- NA

# home_ownership
levels(LCD_mutate$home_ownership)
# ANY and OTHER is an NA category
levels(LCD_mutate$home_ownership)[levels(LCD_mutate$home_ownership)=="ANY"] <- NA
levels(LCD_mutate$home_ownership)[levels(LCD_mutate$home_ownership)=="OTHER"] <- NA
# removing rows with NONE category since there are only 55 observations
LCD_mutate <- LCD_mutate[-which(LCD_mutate$home_ownership == "NONE"),]
LCD_mutate$home_ownership <- factor(LCD_mutate$home_ownership)

# term
levels(LCD_mutate$term)
# fine for now, but may change to just be 36/60

# loan_status
levels(LCD_mutate$loan_status)
# too complicated, but not going to use this variable so I'll leave it alone for now

########################
# Adding New Variables #
########################

# region Variable (west, midwest, south, northeast)

west <- c("WA", "OR", "MT", "ID", "WY", "CA", "NV", "UT", "CO", "AZ", "NM")
  
midwest <- c("ND", "MN", "WI", "MI", "OH", "IN", "IL", "IA", "SD", "NE", "KS", "MO")
  
south <- c("TX", "OK", "AR", "LA", "MS", "TN", "KY", "AL", "GA", "SC", "NC", "WV", "MD", "DC", "DE", "FL", "VA")
  
northeast <- c("PA", "NY", "NJ", "CT", "RI", "MA", "NH", "VT", "ME")

pacific <- c("HI", "AK")

LCD_mutate <- mutate(LCD_mutate, region = factor(case_when(addr_state %in% west ~ "west",
                                                           addr_state %in% midwest ~ "midwest",
                                                           addr_state %in% south ~ "south",
                                                           addr_state %in% northeast ~ "northeast",
                                                           addr_state %in% pacific ~ "pacific")))

# real_estate_col (own/mortgage, other/none)

collateral <- c("OWN", "MORTGAGE")
  
none <- c("OTHER", "NONE", "ANY", "RENT")

LCD_mutate <- mutate(LCD_mutate, real_estate_col = factor(case_when(home_ownership %in% collateral ~ "own/mortgage",
                                                                    home_ownership %in% none ~ "none")))

# emp_length_cat (1 or less, 2-5, 6-9, 10+)

little <- c(0,1)
  
some <- c(2,3,4,5)
  
more <- c(6,7,8,9)
  
lots <- c(10)

LCD_mutate <- mutate(LCD_mutate, emp_length_cat = factor(case_when(emp_length %in% little ~ "0-1",
                                                                   emp_length %in% some ~ "2-5",
                                                                   emp_length %in% more ~ "6-9",
                                                                   emp_length %in% lots ~ "10+",
                                                                   is.na(emp_length) == T ~ "unknown")))

#######################
# Saving Mutated Data #
#######################

write.csv(LCD_mutate, "LCD_mutate.csv")

##################################################################
# Checking for NA values and removing variables with mostly NA's #
##################################################################

# overall, 18.24% of the cells in our data are NA... we need to get rid of variables with mostly NA's
mean(is.na(LCD_mutate))

vars <- c()

for (i in 1:ncol(LCD_mutate)){
  na <- is.na(LCD_mutate[,i])
  if (na > 0.5){
    vars <- c(vars, i)
  }
}
vars 

summary(LCD_mutate[,vars])


#We have 9 variables with over 50% NAs as their entries... we will remove all
LCD_clean <- LCD_mutate[,-vars]

# we are now down to 3.93% of our cells being NA values... great improvement!
mean(is.na(LCD_clean))

# to check if there are variables with more than 5% NAs for entries... NOPE!
vars <- c()

for (i in 1:ncol(LCD_clean)){
  na <- is.na(LCD_clean[,i])
  if (na > 0.05){
    vars <- c(vars, i)
  }
}
vars

# 4.3% is manageable, so let's remove the columns we aren't using at all and then rows with NAs
LCD_clean<- LCD_clean[,names(LCD_clean) %in% c("loan_amnt", "int_rate", "grade", "sub_grade", "issue_d", "addr_state", "annual_inc", 
                                                "application_type",  "dti", "fico_range_low", "fico_range_high", 
                                                "real_estate_col", "home_ownership", "verification_status", "emp_length_cat", 
                                                "region")]

# We are now to .3% cells having NAs... AWESOME
mean(is.na(LCD_clean))


LCD_clean <- na.omit(LCD_clean)

###########################
# Saving new data as .csv #
###########################

write.csv(LCD_clean, "LCD_clean.csv")

##################
# Year Selection #
##################

# Since the data is so big and considering the focus of the project, I will be seleccting 5 years out of the 13.25 years
# I will select 2008 to look at the 2008 market during the great recession and 2019 for the most recent FULL year of data
# The other 3 years will be randomly seleccted from 2009 to 2018 (removing 2007 because it is pre-great recession data)
years <- sample(2009:2018, 3, replace=FALSE)
years

# The result of the random sample selected 2012, 2015, and 2017 as our other 3 years
# Let's make a subset of our data that only contains loans issued from 2008, 2012, 2015, 2017, and 2019\

###################
# Subsetting Data #
###################

# Subsetting data by selected years of interest
LCD_years <- LCD_clean[which(format(LCD_clean$issue_d,"%y") =="08" | format(LCD_clean$issue_d,"%y") == '12' | 
                                 format(LCD_clean$issue_d,"%y") == '15' | format(LCD_clean$issue_d,"%y") == '17' |
                                 format(LCD_clean$issue_d,"%y") == '19'),]

###############
# Saving Data #
###############

write.csv(LCD_years, "LCD_years.csv")

##############################
# Saving by individual years #
##############################
# 2008
LCD_2008 <- LCD_years[which(format(LCD_years$issue_d,"%y") =="08"),]
write.csv(LCD_2008, "LCD_2008.csv")

# 2012
LCD_2012 <- LCD_years[which(format(LCD_years$issue_d,"%y") =="12"),]
write.csv(LCD_2012, "LCD_2012.csv")

# 2015
LCD_2015 <- LCD_years[which(format(LCD_years$issue_d,"%y") =="15"),]
write.csv(LCD_2015, "LCD_2015.csv")

# 2017
LCD_2017 <- LCD_years[which(format(LCD_years$issue_d,"%y") =="17"),]
write.csv(LCD_2017, "LCD_2017.csv")

# 2019
LCD_2019 <- LCD_years[which(format(LCD_years$issue_d,"%y") =="19"),]
write.csv(LCD_2019, "LCD_2019.csv")
