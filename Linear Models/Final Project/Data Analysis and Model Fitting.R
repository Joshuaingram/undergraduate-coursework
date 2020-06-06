###################################
# Data Analysis and Model Fitting #
#                                 #
# Joshua D. Ingram                #
#                                 #
# 05/12/2020                      #
###################################

#######################################
# Loading Packages and Importing Data #
#######################################
library(tidyverse)
library(RColorBrewer)
library(vcd)
library(plyr)
library(VGAM)
library(car)
library(knitr)

# full dataset (only including variables of interest)
LCD <- read.csv("D:/main/Datasets/LendingClub Loans/LendingClubData.csv")
# mutated dataset
LCD_mutate <- read.csv("D:/main/Datasets/LendingClub Loans/LCD_mutate.csv")
# clean dataset
LCD_clean <- read.csv("D:/main/Datasets/LendingClub Loans/LCD_clean.csv")
# 2008 subset
LCD_2008 <- read.csv("D:/main/Datasets/LendingClub Loans/Final/LCD_2008.csv")
# 2012 subset
LCD_2012 <- read.csv("D:/main/Datasets/LendingClub Loans/Final/LCD_2012.csv")
# 2015 subset
LCD_2015 <- read.csv("D:/main/Datasets/LendingClub Loans/Final/LCD_2015.csv")
# 2017 subset
LCD_2017 <- read.csv("D:/main/Datasets/LendingClub Loans/Final/LCD_2017.csv")
# 2019 subset
LCD_2019 <- read.csv("D:/main/Datasets/LendingClub Loans/Final/LCD_2019.csv")

#######################
# Exploring NA Values #
#######################

################
# Full Dataset

# Number of cells with NA values in our full dataset: 23.59%
mean(is.na(LCD))

##################
# Mutated Dataset

# From data cleaning.R, we got that we have 0.3% cells that are NA after creating new categories and selecting variables of interest

# removed all rows that have NAs
# subsets of each year have 0 missing values

########################
# Descriptive Analysis #
########################

######################
# Exploration of 2008

# number of loans
nrow(LCD_2008)

# average loan amount
mean(LCD_2008$loan_amnt)

# density of loan amounts by year
# 2008
ggplot(LCD_2008, aes(x=loan_amnt)) + 
  geom_density(stat = "density", adjust = 3, alpha = .4, fill = "lightblue") + 
  labs(y = "Density", x = "Loan Amount", title = "Distribution of Loan Amounts in 2008")
mean(LCD_2008$loan_amnt)

# 2019
ggplot(LCD_2019, aes(x=loan_amnt)) + 
  geom_density(stat = "density", adjust = 3, alpha = .4, fill = "lightblue") + 
  labs(y = "Density", x = "Loan Amount", title = "Distribution of Loan Amounts in 2019")
mean(LCD_2019$loan_amnt)

# proportion and Distribution of loan grades by year
# 2008
grade.counts.table <- table(LCD_2008$grade)
grade.prop.long <- as.data.frame(as.table(prop.table(grade.counts.table)))
colnames(grade.prop.long) <- c("Grade", "Proportion")
grade.prop.table <- prop.table(grade.counts.table)

ggplot(data = grade.prop.long, aes(x = Grade, y = Proportion, fill = Grade)) + 
  geom_bar(stat = "identity", width = .8) + 
  geom_bar(stat = "identity", width = .8, color = "black", show.legend = F, alpha = 1) + 
  labs(y = "Proportion", title = "Distribution of Loan Grades in 2008") + 
  scale_fill_manual(name = "Grade", values = brewer.pal(7, "Reds")[1:7])

# 2019
grade.counts.table <- table(LCD_2019$grade)
grade.prop.long <- as.data.frame(as.table(prop.table(grade.counts.table)))
colnames(grade.prop.long) <- c("Grade", "Proportion")
grade.prop.table <- prop.table(grade.counts.table)

ggplot(data = grade.prop.long, aes(x = Grade, y = Proportion, fill = Grade)) + 
  geom_bar(stat = "identity", width = .8) + 
  geom_bar(stat = "identity", width = .8, color = "black", show.legend = F, alpha = 1) + 
  labs(y = "Proportion", title = "Distribution of Loan Grades in 2019") + 
  scale_fill_manual(name = "Grade", values = brewer.pal(7, "Reds")[1:7])


# relationship between int_rate and loan_amnt by year
# 2008
ggplot(LCD_2008, aes(x=int_rate, y=loan_amnt))+
  geom_point() +
  labs(y="Loan Amount", x="Interest Rate", title = "Plot of Interest Rate and Loan Amount in 2008")

# 2019
ggplot(LCD_2019, aes(x=int_rate, y=loan_amnt))+
  geom_point() +
  labs(y="Loan Amount", x="Interest Rate", title = "Plot of Interest Rate and Loan Amount in 2019")

######################
# Exploration of 2019

# number of loans
nrow(LCD_2019)

# average loan amount
mean(LCD_2019$loan_amnt)

# density of loan amounts
ggplot(LCD_2019, aes(x=loan_amnt)) + 
  geom_density(stat = "density", adjust = 3, alpha = .4, fill = "lightblue") + 
  labs(y = "Density", x = "Loan Amount", title = "Distribution of Loan Amounts in 2019")

# proportion of loan grades
grade.counts.table <- table(LCD_2019$grade)
grade.prop.long <- as.data.frame(as.table(prop.table(grade.counts.table)))
colnames(grade.prop.long) <- c("Grade", "Proportion")
grade.prop.table <- prop.table(grade.counts.table)

# Distribution of Loan Grades
ggplot(data = grade.prop.long, aes(x = Grade, y = Proportion, fill = Grade)) + 
  geom_bar(stat = "identity", width = .8) + 
  geom_bar(stat = "identity", width = .8, color = "black", show.legend = F, alpha = 1) + 
  labs(y = "Proportion", title = "Distribution of Loan Grades") + 
  scale_fill_manual(name = "Grade", values = brewer.pal(7, "Reds")[1:7])

# relationship between int_rate and loan_amnt
ggplot(LCD_2019, aes(x=int_rate, y=loan_amnt))+
  geom_point() +
  labs(y="Loan Amount", x="Interest Rate", title = "Plot of Interest Rate and Loan Amount in 2019")


#############################################################
# Outlining Economic Theory of Demand and Supply for Credit #
#############################################################

# Assuming our market has no credit rationing and we are in equilibrium, quantity demanded = quantity supplied... we have a simultaneity issue since price and quantity are determined simultaneously
# There is evidence that the market is in equilibrium since the funded amount is almost always equal to the loan amount (0.02% of loans aren't fully funded).

# Demand Equation: Q_D (laon_amnt) = beta_0 + beta_1(int_rate) + beta_2(annual_inc) + beta_3(empl_length_cat) + beta_4(region) + beta_5(home_ownership) + epsilon_1

# Supply Equation: Q_S (loan_amnt) = alpha_0 + alpha_1(int_rate) + alpha_2(dti) + alpha_3(fico_range_low) + alpha_4(real_estate_col) + epsilon_2

# Q_D = Q_S

# int_rate_hat = delta_0 + delta_1(dti) + delta_2(fic0_range_low) + delta_3(fico_range_high) + delta_4(real_estate_col) + delta_5(annual_inc) 
# + delta_6(empl_length_cat) + delta_7(region) + epsilon_3 (dropped home_ownership because it is perfectly collinear with real_estate collateral)

# New Demand Equation: Q_D (loan_amnt) = beta_0 + beta_1(int_rate_hat + epsilon_3) + beta_2(annual_inc) + beta_3(empl_length_cat) + beta_4(region) + beta_5(home_ownership) + epsilon_4

# significance level: 0.10

###########################
# Two-Stage Least Squares #
###########################


########
# 2008 #
########

#########################################
# First Stage to estimate price

int_rate_hat_2008 <- lm(int_rate ~ dti + fico_range_low + real_estate_col + annual_inc + emp_length_cat + region, data = LCD_2008)

############################
# Checking for Collinearity

cor(LCD_2008$dti, LCD_2008$fico_range_low)
cor(LCD_2008$dti, LCD_2008$annual_inc)
cor(LCD_2008$fico_range_low, LCD_2008$annual_inc)
vif(int_rate_hat_2008)

#############################
# Checking Model Assumptions

# non-linearity and non-constant variance... clearly have a non-linear relationship
plot(int_rate_hat_2008,1)

# normality assumption... not good
plot(int_rate_hat_2008,2)
plot(density(rstandard(int_rate_hat_2008)))

###############################
# Remedies to model assumptions

int_rate_hat_2008 <- lm(log(int_rate) ~ dti + fico_range_low + real_estate_col + annual_inc + emp_length_cat + region, data = LCD_2008)

# non-linearity and non-constant variance... clearly have a non-linear relationship
plot(int_rate_hat_2008,1)

# normality assumption... still not great but our sample is large
plot(int_rate_hat_2008,2)
plot(density(rstandard(int_rate_hat_2008)))

# perhaps some predictors have a non-linear relationship with price

plot(int_rate ~ dti, data = LCD_2008)
plot(int_rate ~ fico_range_low, data = LCD_2008)
plot(int_rate ~ annual_inc, data = LCD_2008[which(LCD_2008$annual_inc < 80000),])

######################
# Outliers

plot(int_rate_hat_2008,4)
cutoff <- 4/(nrow(LCD_2008) - (6+1))

# removing highly infuential outlier
LCD_2008_new <- LCD_2008[-1157,]

int_rate_hat_2008_wo <- lm(log(int_rate) ~ dti + fico_range_low + real_estate_col + annual_inc + emp_length_cat + region, data = LCD_2008_new)

# variance is just fine... not perfectly linear but it's noticeably better
plot(int_rate_hat_2008_wo,1)

# normality isn't great still, but we have enough observations
plot(int_rate_hat_2008_wo,2)
plot(density(rstandard(int_rate_hat_2008_wo)))

plot(int_rate_hat_2008_wo,4)
cutoff <- 4/(nrow(LCD_2008_new) - (6+1))

# removing the three influential outliers... They don't heavily affecct our estimates unlike the last outlier, so we will keep them in our data
LCD_2008_new2 <- LCD_2008_new[-c(1505,1739,2059),]

int_rate_hat_2008_wo2 <- lm(log(int_rate) ~ dti + fico_range_low + real_estate_col + annual_inc + emp_length_cat + region, data = LCD_2008_new2)
summary(int_rate_hat_2008_wo2)

plot(int_rate_hat_2008_wo2,1)

#################################
# Model Accuracy and Significance

int_rate_hat_2008_final <- lm(log(int_rate) ~ dti + fico_range_low + real_estate_col + annual_inc + emp_length_cat + region, data = LCD_2008_new)
summary(int_rate_hat_2008_final)

####################################
# Creating Column of Price Estimates

int_rate_hat <- predict(int_rate_hat_2008_final)
int_rate_hat <- exp(int_rate_hat)

LCD_2008_new$int_rate_hat <- int_rate_hat

###################
# Demand Model

lm_08_q_1 <- lm(loan_amnt ~ int_rate_hat + annual_inc + emp_length_cat + region + home_ownership, data = LCD_2008_new)
summary(lm_08_q_1)

###########################
# Checking for Collinearity

# everything is fine
vif(lm_08_q_1)

##########################################
# Checking Model Assumptions and Outliers

# linearity and homoscedasticity... There are likely outliers
plot(lm_08_q_1,1)

# checking for outliers
plot(lm_08_q_1, 4)
cutoff <- cutoff <- 4/(nrow(LCD_2008_new) - (4+1))
LCD_2008_new3 <- LCD_2008_new[-c(1504,2058,2260),]

# new model without outliers
lm_08_q_2 <- lm(loan_amnt ~ int_rate_hat + annual_inc + emp_length_cat + region + home_ownership, data = LCD_2008_new3)
summary(lm_08_q_2)

# checking residual plots
plot(lm_08_q_2,1)
plot(lm_08_q_2,4)
cutoff <- cutoff <- 4/(nrow(LCD_2008_new3) - (4+1))
LCD_2008_new4 <- LCD_2008_new[-c(1165,1504,2058,2260),]

# We will move forward by removing these 4 outliers from our final model
lm_08_q_3 <- lm(loan_amnt ~ int_rate_hat + annual_inc + emp_length_cat + region + home_ownership, data = LCD_2008_new4)
summary(lm_08_q_3)

# model assumptions

# linearity and homoscedasticity... Variance is not good
plot(lm_08_q_3,1)

# normality... not great but the size is large so it's okay
plot(lm_08_q_3,2)
plot(density(rstandard(lm_08_q_3)))

################################
# Remedies to Model Assumptions

# ####### GO BACK AND REDO ALL OF THIS

##################################
# Model Accuracy and Significance

lm_08_q_final <- lm(loan_amnt ~ int_rate_hat + annual_inc + emp_length_cat + region + home_ownership, data = LCD_2008_new4)
summary(lm_08_q_final)
plot(lm_08_q_final,1)
plot(lm_08_q_final,2)
plot(density(rstandard(lm_08_q_final)))
conf <- confint(lm_08_q_final, level = 0.9)
coef2 <- summary(lm_08_q_final)$coef

############## GO BACK AND USE THE SAME SUSBET OF DATA FOR FIRST STAGE AND SECOND STAGE #######################


########
# 2012 #
########

#########################################
# First Stage to estimate price

############################
# Checking for Collinearity

#############################
# Checking Model Assumptions

###############################
# Remedies to model assumptions

######################
# Outliers

#################################
# Model Accuracy and Significance

####################################
# Creating Column of Price Estimates



###################
# Demand Model


###########################
# Checking for Collinearity

############################
# Checking Model Assumptions

################################
# Remedies to Model Assumptions

######################
# Outliers

##################################
# Model Accuracy and Significance




########
# 2015 #
########

#########################################
# First Stage to estimate price

############################
# Checking for Collinearity

#############################
# Checking Model Assumptions

###############################
# Remedies to model assumptions

######################
# Outliers

#################################
# Model Accuracy and Significance

####################################
# Creating Column of Price Estimates



###################
# Demand Model


###########################
# Checking for Collinearity

############################
# Checking Model Assumptions

################################
# Remedies to Model Assumptions

######################
# Outliers

##################################
# Model Accuracy and Significance







########
# 2017 #
########

#########################################
# First Stage to estimate price

############################
# Checking for Collinearity

#############################
# Checking Model Assumptions

###############################
# Remedies to model assumptions

######################
# Outliers

#################################
# Model Accuracy and Significance

####################################
# Creating Column of Price Estimates



###################
# Demand Model


###########################
# Checking for Collinearity

############################
# Checking Model Assumptions

################################
# Remedies to Model Assumptions

######################
# Outliers

##################################
# Model Accuracy and Significance






########
# 2017 #
########

#########################################
# First Stage to estimate price

############################
# Checking for Collinearity

#############################
# Checking Model Assumptions

###############################
# Remedies to model assumptions

######################
# Outliers

#################################
# Model Accuracy and Significance

####################################
# Creating Column of Price Estimates



###################
# Demand Model


###########################
# Checking for Collinearity

############################
# Checking Model Assumptions

################################
# Remedies to Model Assumptions

######################
# Outliers

##################################
# Model Accuracy and Significance





########
# 2019 #
########

#########################################
# First Stage to estimate price

int_rate_hat_2019 <- lm(int_rate ~ dti + fico_range_low + real_estate_col + annual_inc + emp_length_cat + region, data = LCD_2019)
summary(int_rate_hat_2019)

plot(int_rate_hat_2019, 1)
plot(int_rate_hat_2019, 4)
cutoff <- 4/(nrow(LCD_2019) - (6+1))

LCD_2019_new <- LCD_2019[-c(25588,289650,1139),]

int_rate_hat_2019_2 <- lm(int_rate ~ dti + fico_range_low + real_estate_col + annual_inc + emp_length_cat + region, data = LCD_2019_new)
summary(int_rate_hat_2019_2)

plot(int_rate_hat_2019_2, 1)
plot(int_rate_hat_2019_2, 4)
cutoff <- 4/(nrow(LCD_2019) - (6+1))

############################
# Checking for Collinearity

cor(LCD_2008$dti, LCD_2008$fico_range_low)
cor(LCD_2008$dti, LCD_2008$annual_inc)
cor(LCD_2008$fico_range_low, LCD_2008$annual_inc)
vif(int_rate_hat_2008)

plot(int_rate ~ dti, data = LCD_2019)
plot(int_rate ~ fico_range_low, data = LCD_2019)
plot(int_rate ~ annual_inc, data = LCD_2019[which(LCD_2019$annual_inc < 250000),])

#############################
# Checking Model Assumptions

# normality and homescedasticity
plot(int_rate_hat_2019_2,1)
plot(int_rate_hat_2019_2,2)

###############################
# Remedies to model assumptions

######################
# Outliers

#################################
# Model Accuracy and Significance

####################################
# Creating Column of Price Estimates

int_rate_hat <- predict(int_rate_hat_2019_2)

LCD_2019_new$int_rate_hat <- int_rate_hat

###################
# Demand Model

lm_19_q_1 <- lm(loan_amnt ~ int_rate_hat + annual_inc + emp_length_cat + region + home_ownership, data = LCD_2019_new)
coef1 <- summary(lm_19_q_1)$coef
conf <- confint(lm_19_q_1, level = .9)
cbind(coef1, conf)

###########################
# Checking for Collinearity

# everything is fine
vif(lm_19_q_1)

##########################################
# Checking Model Assumptions and Outliers

# linearity and homoscedasticity... There are likely outliers
plot(lm_19_q_1,1)
plot(lm_19_q_1,2)

# checking for outliers
plot(lm_19_q_1, 4)
cutoff <- cutoff <- 4/(nrow(LCD_2019_new) - (4+1))

###########################
# Checking for Collinearity

############################
# Checking Model Assumptions

################################
# Remedies to Model Assumptions

######################
# Outliers

##################################
# Model Accuracy and Significance



#################################
# Comparison of Effects by Year #
#################################





######################################################################
# Logistic Regression Model to predict real estate ownership in 2019 #
######################################################################

#################
# Model Fitting 1

log_1 <- glm(real_estate_col ~ annual_inc + dti + emp_length_cat, family = "binomial", data = LCD_2019)

###########################
# Checking for Collinearity

vif(log_1)
cor(LCD_2019$dti, LCD_2019$annual_inc)

##################################
# Model Accuracy and Significance

# significance
summary(log_1)

log_null <- glm(real_estate_col ~ 1, data = LCD_2019, family="binomial")
anova(log_null, log_1, test = "LRT")

# accuracy

log_prob <- predict(log_1, type='response')
log_predict <- ifelse(log_prob > 0.50, "own/mortgage","none")
mean(log_predict == LCD_2019$real_estate_col)

####################
# Varaible Selection

log_final <- step(log_1)

##############
# Final Model

sum_log_final <- summary(log_final)

###############################
# Visualzations and Predictions

income_seq <- seq(0, 250000, length.out = 250)
dti_seq <- mean(LCD_2019$dti)
emp_length1 <- "10+"
emp_length2 <- "2-5"


expand_grid_length1 <- expand.grid(income_seq, dti_seq, emp_length1)
colnames(expand_grid_length1) <- c("annual_inc", "dti", "emp_length_cat")

expand_grid_length2 <- expand.grid(income_seq, dti_seq, emp_length2)
colnames(expand_grid_length2) <- c("annual_inc", "dti", "emp_length_cat")

# graphs for probabilities
fit_logit_length1 <- predict(log_final, newdata = expand_grid_length1, untransform = TRUE, type = "response")
fit_logit_length2 <- predict(log_final, newdata = expand_grid_length2, untransform = TRUE, type = "response")

expanded_predicted_length1 <- cbind(expand_grid_length1, fit_logit_length1)
colnames(expanded_predicted_length1) <- c("annual_inc", "dti", "emp_length_cat", "probability")
expanded_predicted_length2 <- cbind(expand_grid_length2, fit_logit_length2)
colnames(expanded_predicted_length2) <- c("annual_inc", "dti", "emp_length_cat", "probability")

predicted_probs <- rbind(expanded_predicted_length1, expanded_predicted_length2)

ggplot(predicted_probs, aes(x = annual_inc, y = probability, color = emp_length_cat)) + 
  geom_line() + 
  labs(x = "Annual Income", y = "Probability of Owning Real Estate", title = "Probability of Loan Applicant Owning Real Estate by Experience", col="Employment")

############
# Interpretations, Confidence Intervals

confint.default(log_final)
