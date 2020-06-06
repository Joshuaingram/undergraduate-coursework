#####################################
### Final Analysis and Clean Code ### 
#####################################
## Joshua D. Ingram #################
#####################################

# Packages to be used
library(readr)
library(knitr)
library(tidyverse)
library(RColorBrewer)
library(car)
library(pscl)
library(boot)
library(bootstrap)


############ Working with and cleaning dataset #################
suicide.data <- read.csv("C:/Users/joshi/Downloads/suicide-rates-overview-1985-to-2016 (3)/master.csv")

# Overview of the dataset
head(suicide.data)
ls(suicide.data)
nrow(suicide.data)
ncol(suicide.data)

# changing the variables to the appropriate classes
suicide.data$sex <- as.factor(suicide.data$sex)
suicide.data$country <- as.factor(suicide.data$ï..country)
suicide.data$generation <- as.factor(suicide.data$generation)
suicide.data$age <- as.factor(suicide.data$age)
suicide.data$suicides_no <- as.numeric(suicide.data$suicides_no)
suicide.data$population <- as.numeric(suicide.data$population)
suicide.data$suicides.100k.pop <- as.numeric(suicide.data$suicides.100k.pop)
suicide.data$HDI.for.year <- as.numeric(suicide.data$HDI.for.year)
suicide.data$gdp_per_capita <- as.numeric(suicide.data$gdp_per_capita....)
suicide.data$gdp_for_year <- suicide.data$gdp_for_year....

# removing variables that won't be used in the dataset
suicide.data$country.year<- NULL
suicide.data$ï..country <- NULL
suicide.data$gdp_for_year.... <- NULL
suicide.data$gdp_per_capita.... <- NULL



# creating a variable to break countries up into 8 regions (as defined by US Department of Homeland Security)

# lists of countries in each region
Africa <- c("Algeria", "Angola", "Benin", "Botswana", "Burkina Faso", "Burundi", "Cameroon",
            "Cape Verde", "Cental African Republic", "Chad", "Comoros", "Cote d'lvoire", "Democratic Republic of the Congo",
            "Djibouti", "Egypt", "Equatorial Guinea", "Eritrea", "Ethiopia", "Gabon",
            "Gambia", "Ghana", "Guinea", "Guinea-Bissau", "Kenya", "Lesotho", "Liberia",
            "Libya", "Madagascar", "Malawi", "Mali", "Mali", "Mauritania", "Mauritius", "Morocco",
            "Mozambique", "Namibia", "Niger", "Nigeria", "Republic of the Congo", "Reunion", "Rwanda",
            "Saint Helena", "Sao Tome and Principe", "Senegal", "Seychelles", "Sierra Leone", "Somalia", "South Africa", "South Sudan",
            "Sudan", "Swaziland", "Tanzania", "Togo", "Tunisia", "Uganda", "Western Sahara", "Zambia", "Zimbabwe")
Asia <- c("Afghanistan", "Armenia", "Azerbaijan", "Bahrain", "Bangladesh", "Bhutan", "Brunei", "Burma",
          "Cambodia", "China", "Cyprus", "East Timor", "Georgia", "Hong Kong", "India", "Indonesia",
          "Iran", "Iraq", "Israel", "Japan", "Jordan", "Kazakhstan", "Kuwwait", "Kyrgyzstan", "Laos",
          "Lebanon", "Macau", "Malaysia", "Maldives", "Mongolia", "Nepal", "North Korea", "Oman",
          "Pakistan", "Philippines", "Qatar", "Saudi Arabia", "Singapore", "South Korea", "Sri Lanka", "Syria",
          "Taiwan", "Tajikistan", "Thailand", "Turkey", "Turkmenistan", "United Arab Emirates",
          "Uzbekistan", "Yemen")
Caribbean <- c("Anguilla", "Antigua and Barbuda", "Aruba", "Bahamas", "Barbados",
               "Bermuda", "British Virgin Islands", "Cayman Islands", "Cuba", "Dominica",
               "Dominican Republic", "Grenada", "Guadeloupe", "Haiti", "Jamaica", "Martinique",
               "Montserrat", "Netherlands Antilles", "Puerto Rico", "Saint Kitts and Nevis", "Saint Lucia",
               "Saint Vincent and the Grenadines", "Trinidad and Tobago", "Turks and Caicos Islands",
               "U.S. Virgin Islands")
Central.America <- c("Belize", "Costa Rica", "El Salvador", "Guatemala", "Honduras", "Nicaragua",
                     "Panama")
Europe <- c("Albania", "Andorra", "Austria", "Belarus", "Belgium", "Bosnia and Herzegovina", "Bulgaria", "Croatia",
            "Czech Republic", "Denmark", "Estonia", "Finland", "France", "Germany", "Gibraltar", "Greece",
            "Holy See", "Hungary", "Iceland", "Ireland", "Italy", "Kosovo", "Latvia", "Liechtenstein", 
            "Lithuania", "Luxembourg", "Macedonia", "Malta", "Moldova", "Monaco", "Montenegro", "Netherlands",
            "Norway", "Poland", "Portugal", "Romania", "Russia", "San Marino", "Slovak Republic", "Slovenia",
            "Spain", "Serbia", "Serbia and Montenegro", "Sweden", "Switzerland", "Ukraine", "United Kingdom")
North.America <- c("Canada", "Greenland", "Mexico", "Saint Pierre and Miquelon", "United States")
Oceania <- c("American Samoa", "Australia", "Christmas Island", "Cocos Islands", "Cook Islands",
             "Federated States of Micronesia", "Fiji", "French Polynesia", "Guam", "Kiribati",
             "Marshall Islands", "Nauru", "New Caledonia", "New Zealand", "Niue", "Northern Mariana Islands",
             "Palau", "Papua New Guinea", "Pitcairn Islands", "Samoa", "Solomon Islands", "Tokelau", "Tonga",
             "Tuvalu", "Vanuatu", "Wallis and Futuna Islands")
South.America <- c("Argentina", "Bolivia", "Brazil", "Chile", "Colombia", "Ecuador", "Falkland Islands",
                   "French Guiana", "Guyana", "Paraguay", "Peru", "Suriname", "Uruguay", "Venezuela")

# new region variable created with 8 levels (as shown above)
suicide.data.mutated1 <- mutate(suicide.data, Region = factor(case_when(country %in% Africa ~ "Africa",
                                                                                country %in% Asia ~ "Asia",
                                                                                country %in% Caribbean ~ "Caribbean",
                                                                                country %in% Central.America ~ "Central America",
                                                                                country %in% Europe ~ "Europe",
                                                                                country %in% North.America ~ "North America",
                                                                                country %in% Oceania ~ "Oceania",
                                                                                country %in% South.America ~ "South America",
                                                                                TRUE ~ NA_character_)))


# 5.81% of the values in our new data is missing
mean(is.na(suicide.data.mutated1))

# 69,93% of HDI's are missing values
mean(is.na(suicide.data.mutated1$HDI.for.year))

# since we have a significant amount of our developement values misisng, an "unknown" category will be created because this could provide some
# predictive value

# Creating four categories for HDI index: high development (.8-1), medium 
# development (.6-.799), low or very low development (0 - .599), unknown (missing)

# new development variable created with 4 factors (shown above)
suicide.data.mutated2 <- mutate(suicide.data.mutated1, Development = factor(case_when(HDI.for.year >= .8 ~ "high",
                                                                             HDI.for.year >= .6 & HDI.for.year <= .799 ~ "medium",
                                                                             HDI.for.year <= .599 ~ "low or very low",
                                                                             is.na(HDI.for.year) == T ~ "unknown",
                                                                             TRUE ~ NA_character_)))

# removed all years before 1990 since the hdi was not created until 1990
suicide.data.mutated3 <- suicide.data.mutated2[which(suicide.data.mutated2$year >= 1990),]

### Final demographic dataset ###
demographic.suicides <- suicide.data.mutated3


# removing demographic information from the data to look at aggregate suicides in the second part of analysis (separate dataset)
years.list <- seq(1990, 2016, 1)
country.list <- levels(demographic.suicides$country)

colnames(demographic.suicides)

# lists for new data to be placed
aggregate.suicides <- c()
aggregate.population <- c()
aggregate.per100 <- c()
new.years <- c()
new.countries <- c()
new.development <- c()
new.region <- c()
new.gdp.capita <- c()

# for loop that combines demographic info into an aggregate variable for each country and year
for (j in country.list){
  for (i in years.list){
    selected.rows <- suicide.data.mutated3[suicide.data.mutated3$year == i & suicide.data.mutated3$country == j,]
    
    if (nrow(selected.rows) != 0){
      total.suicides <- sum(selected.rows$suicides_no)
      
      aggregate.suicides <- append(aggregate.suicides, total.suicides)
      
      total.population <- sum(selected.rows$population)
      
      aggregate.population <- append(aggregate.population, total.population)
      
      per100 <- total.suicides / (total.population/100000)
      aggregate.per100 <- append(aggregate.per100, per100)
      
      new.years <- append(new.years, i)
      new.countries <- append(new.countries, j)
      new.development <- append(new.development, selected.rows$Development[1])
      new.region <- append(new.region, selected.rows$Region[1])
      new.gdp.capita <- append(new.gdp.capita, selected.rows$gdp_per_capita[1])
    }
    selected.rows <- NULL
  }
}

# combining the lists
new.suicide.data <- data.frame(new.years, new.countries, new.region, new.development, 
                               new.gdp.capita, aggregate.population, aggregate.suicides,  aggregate.per100)

colnames(new.suicide.data) <- c("Year", "Country", "Region", "Development", "GDP.per.capita", "Population", "Suicides", "Suicides.per100")

# Development level
new.suicide.data <- mutate(new.suicide.data, Development = factor(case_when(Development == 1 ~ "high",
                                                                            Development == 3 ~ "medium",
                                                                            Development == 2 ~ "low or very low",
                                                                            Development == 4 ~ "unknown",
                                                                            TRUE ~ NA_character_)))

# Regions
new.suicide.data <- mutate(new.suicide.data, Region = factor(case_when(Region == 1 ~ "Africa",
                                                                       Region == 3 ~ "Asia",
                                                                       Region == 2 ~ "Caribbean",
                                                                       Region == 4 ~ "Central America",
                                                                       Region == 5 ~ "Europe",
                                                                       Region == 6 ~ "North America",
                                                                       Region == 7 ~ "Oceania",
                                                                       Region == 8 ~ "South America",
                                                                       TRUE ~ NA_character_)))

### final dataset for aggregate suicides ###
aggregate.suicides <- new.suicide.data

# overview of the two datasets 
head(demographic.suicides)
head(aggregate.suicides)


######## Dataset Overview and Visualizations ########

# visualizaions of suicides in the US from 1990 to 2015 (first total then per 100k)
ggplot(aggregate.suicides[aggregate.suicides$Country == "United States",], aes(x = Year, y = Suicides)) + 
  geom_line(size = 2, color = "Red") + 
  ylim(0, 50000) + 
  labs(title="Number of Suicides in the U.S. from 1990 to 2015") +
  theme_classic()

ggplot(aggregate.suicides[aggregate.suicides$Country == "United States",], aes(x = Year, y = Suicides.per100)) + 
  geom_line(size = 2, color = "Red") + 
  ylim(0, 16) + 
  labs(title="Number of Suicides per 100,000 in the U.S. from 1990 to 2015", y = "Suicides per 100,000") +
  theme_classic()

# percent change of total suicides from 1990 to 2015 in the US... 43.03%
(aggregate.suicides[aggregate.suicides$Year == 2015 & aggregate.suicides$Country == "United States",]$Suicides - 
    aggregate.suicides[aggregate.suicides$Year == 1990 & aggregate.suicides$Country == "United States",]$Suicides) /
  aggregate.suicides[aggregate.suicides$Year == 1990 & aggregate.suicides$Country == "United States",]$Suicides * 100

# number of suicides in the UK (total then per 100k), for comparison
ggplot(aggregate.suicides[aggregate.suicides$Country == "United Kingdom",], aes(x = Year, y = Suicides)) + 
  geom_line(size = 2, color = "Red") + 
  ylim(0, 8000) + 
  labs(title="Number of Suicides in the U.K. from 1990 to 2015") +
  theme_classic()

ggplot(aggregate.suicides[aggregate.suicides$Country == "United Kingdom",], aes(x = Year, y = Suicides.per100)) + 
  geom_line(size = 2, color = "Red") + 
  ylim(0, 12) + 
  labs(title="Number of Suicides in the U.K. from 1990 to 2015") +
  theme_classic()

# mean number of suicides per 100k from our dataset
mean(aggregate.suicides$Suicides.per100)

# distribution of suicides per 100k divided by age group
ggplot(aes(x = suicides.100k.pop), data = demographic.suicides[demographic.suicides$age == "5-14 years",]) + geom_histogram()
ggplot(aes(x = suicides.100k.pop), data = demographic.suicides[demographic.suicides$age == "15-24 years",]) + geom_histogram()
ggplot(aes(x = suicides.100k.pop), data = demographic.suicides[demographic.suicides$age == "25-34 years",]) + geom_histogram()
ggplot(aes(x = suicides.100k.pop), data = demographic.suicides[demographic.suicides$age == "35-54 years",]) + geom_histogram()
ggplot(aes(x = suicides.100k.pop), data = demographic.suicides[demographic.suicides$age == "55-74 years",]) + geom_histogram()
ggplot(aes(x = suicides.100k.pop), data = demographic.suicides[demographic.suicides$age == "75+ years",]) + geom_histogram()

# All of these distributions by age groups shows that they are right skewed... a majority of the suicides per 100k are zero



####### Demographic Model fitting ########

ls(demographic.suicides)

# first model fit with all the useable variables
demo.fit.all1 <- lm(suicides.100k.pop  ~ Region + Development + age + gdp_per_capita + population + sex + year, data = demographic.suicides)

# the model has a significant p-value and R^2 of .3605.... most variable are significant.... but first we should check the normality and constant variance
# assumptions
summary(demo.fit.all1)

# there is heteroskedasticity and the residuals are not normally distributed.... we should not use the p-values from the summary above
plot(demo.fit.all1, 1)
plot(demo.fit.all1, 2)

# log transformation might get rid of heteroskedasticity (log(X + 1) because some observations have 0 suicides)
demo.fit.all2 <- lm(log(suicides.100k.pop + 1) ~ Region + Development + age + gdp_per_capita + population + sex + year, data = demographic.suicides)

# R^2 of 0.5271 and a very small p-value... let's check our assumptions
summary(demo.fit.all2)

# both of our assumptions are roughly met (normality more-so)... considering the shape of the residual variance we should be cautious with inference
# perhaps a zero-infalted model would be better but we can still gain some insightful information from our model
plot(demo.fit.all2, 1)
plot(demo.fit.all2, 2)

### Final Demographic Model ###
summary(demo.fit.all2)


# plotting our model holding development(high), age (25-34 years), region(North America), population(Mean Population in North America in 2015),
# and year (2015) constant.... we will compare the sexes

# mean population... missing data so we don't include NAs
mean.pop <- mean(aggregate.suicides[aggregate.suicides$Region == "North America" & aggregate.suicides$Year == 2015,]$Population, na.rm = TRUE)
year.pred <- 2015
age.pred <- "25-34 years"
region.pred <- "North America"
develop.pred <- "high"
gdp.seq <- seq(min(aggregate.suicides$GDP.per.capita), max(aggregate.suicides$GDP.per.capita), length.out = 1000)
sex.pred <- c("male", "female")

expand.grid.demo <- expand.grid(region.pred, develop.pred, age.pred, gdp.seq, mean.pop, sex.pred, year.pred)
colnames(expand.grid.demo) <- c("Region", "Development", "age", "gdp_per_capita", "population", "sex", "year")

demo.expand.pred <- predict(demo.fit.all2, newdata = expand.grid.demo)

demo.pred.data <- cbind(expand.grid.demo, demo.expand.pred)
colnames(demo.pred.data) <- c("Region", "Development", "age", "gdp_per_capita", "population", "sex", "year", "suicides.100k.pop")

ggplot(demo.pred.data, aes(x = gdp_per_capita, y = exp(suicides.100k.pop-1), color = sex)) + 
  geom_line() + 
  labs(x = "GDP per capita", y = "Predicted Suicides per 100k", title = "Predicted Suicides for Males and Females") +
  lims(x = c(0, 120000), y = c(0, 100))

mean(aggregate.suicides[aggregate.suicides$Year == 2015 & aggregate.suicides$Region == "North America",]$Suicides.per100, na.rm = TRUE)

####

# Interpretations of the beta estimates (sex male vs female (female baseline))
#
# Holding all other variables constant, we predict, on average, to see the suicides per 100k to multiply by e^1.006 for males, given females as 
# the baseline cateogory (take into account the log+1)
#

####### Aggregate Model fitting ########

# first let's look at the correlations between our numerical predictors
cor(subset(aggregate.suicides, select = c(Year, GDP.per.capita, Population, Suicides.per100)))

# first model
agg.fit1 <- lm(Suicides.per100 ~ Development + Region + GDP.per.capita + Population + Year, data = aggregate.suicides)
summary(agg.fit1)

# there is heteroskedasticity and residuals are not normally distributed
plot(agg.fit1, 1)
plot(agg.fit1, 2)

# let's do a log transformation for our next model
agg.fit2 <- lm(log(Suicides.per100 + 1) ~ Development + Region + GDP.per.capita + Population + Year, data = aggregate.suicides)
agg.fit2.sum <- summary(agg.fit2)

   plot(agg.fit2, 1)
# residuals are not normally distributed.... shouldn't trust p-values in from our lm() function
plot(agg.fit2, 2)

# let's do bootstrapping to get better p-values since we have the residual normality assumption broken
n <- nrow(aggregate.suicides)
i <- sample(1:n, n, replace=T)
set.seed(12)
p.values <- c()
confidence.lower <- c()
confidence.upper <- c()
beta.estimates <- c()

for (index in 2:13){
  beta1 <- function(x,i) { coef(lm(log(Suicides.per100 + 1) ~ Development + Region + GDP.per.capita + Population + Year,
                                   data = x,
                                   subset = i))[index]}
  
  
  res <- boot(data = aggregate.suicides,
              statistic = beta1,
              R = 1000)
  
  beta1.full <- coef(lm(log(Suicides.per100 + 1) ~ Development + Region + GDP.per.capita + Population + Year,
                        data=aggregate.suicides))[index]
  
  bias <- mean(res$t) - beta1.full
  
  unbiased.est <- res$t - bias
  
  conf.low <- quantile(unbiased.est, 0.025)
  conf.up <- quantile(unbiased.est, 0.975)
  
  beta.H0 <-res$t - mean(res$t) + bias
  
  p.val <- mean(abs(beta.H0) >= abs(beta1.full))
  
  beta.estimates <- append(beta.estimates, beta1.full)
  p.values <- append(p.values, p.val)
  confidence.lower <- append(confidence.lower, conf.low)
  confidence.upper <- append(confidence.upper, conf.up)
  
  
}

boot.info <- data.frame( beta.estimates, p.values)
colnames(boot.info) <- c( "Estimates", "P.value", "Conf.lower", "Conf.upper")
# more accurate p-values
boot.info

# dataframe with old and new p-values
p.val.compare1 <- data.frame(beta.estimates, agg.fit2.sum$coefficients[2:13,4], p.values)
colnames(p.val.compare1) <- c("estimates", "lm() p-values", "bootstrap p-values")

p.val.compare1

AIC(agg.fit2)


# population seems to be an insignificant predictor and development is on the line
# to keep our model simple, I'll remove development and poopulation
agg.fit3 <- lm(log(Suicides.per100 + 1) ~ Region + GDP.per.capita + Year, data = aggregate.suicides)
agg.fit3.sum <- summary(agg.fit3)

# constant variance still
plot(agg.fit3, 1)
# residuals are not normally distributed.... shouldn't trust p-values in from our lm() function
plot(agg.fit3, 2)

# let's do bootstrapping to get better p-values since we have the residual normality assumption broken
n <- nrow(aggregate.suicides)
i <- sample(1:n, n, replace=T)
set.seed(12)
p.values <- c()
confidence.lower <- c()
confidence.upper <- c()
beta.estimates <- c()

for (index in 2:10){
  beta1 <- function(x,i) { coef(lm(log(Suicides.per100 + 1) ~ Region + GDP.per.capita + Year,
                                   data = x,
                                   subset = i))[index]}
  
  
  res <- boot(data = aggregate.suicides,
              statistic = beta1,
              R = 1000)
  
  beta1.full <- coef(lm(log(Suicides.per100 + 1) ~ Region + GDP.per.capita + Year,
                        data=aggregate.suicides))[index]
  
  bias <- mean(res$t) - beta1.full
  
  unbiased.est <- res$t - bias
  
  conf.low <- quantile(unbiased.est, 0.025)
  conf.up <- quantile(unbiased.est, 0.975)
  
  beta.H0 <-res$t - mean(res$t) + bias
  
  p.val <- mean(abs(beta.H0) >= abs(beta1.full))
  
  beta.estimates <- append(beta.estimates, beta1.full)
  p.values <- append(p.values, p.val)
  confidence.lower <- append(confidence.lower, conf.low)
  confidence.upper <- append(confidence.upper, conf.up)
  
  
}

boot.info <- data.frame( beta.estimates, p.values)
colnames(boot.info) <- c( "Estimates", "P.value", "Conf.lower", "Conf.upper")
# more accurate p-values
boot.info

# dataframe with old and new p-values
p.val.compare2 <- data.frame(beta.estimates, agg.fit3.sum$coefficients[2:10,4], p.values)
colnames(p.val.compare2) <- c("estimates", "lm() p-values", "bootstrap p-values")

p.val.compare2

# this model is marginally better according to the AIC
AIC(agg.fit3)




# let's try adding back development
agg.fit4 <- lm(log(Suicides.per100 + 1) ~ Development + Region + GDP.per.capita + Year, data = aggregate.suicides)
agg.fit4.sum <- summary(agg.fit4)

# constant variance still
plot(agg.fit4, 1)
# residuals are not normally distributed.... shouldn't trust p-values in from our lm() function
plot(agg.fit4, 2)

# let's do bootstrapping to get better p-values since we have the residual normality assumption broken
n <- nrow(aggregate.suicides)
i <- sample(1:n, n, replace=T)
set.seed(12)
p.values <- c()
confidence.lower <- c()
confidence.upper <- c()
beta.estimates <- c()

for (index in 2:13){
  beta1 <- function(x,i) { coef(lm(log(Suicides.per100 + 1) ~Development + Region + GDP.per.capita + Year,
                                   data = x,
                                   subset = i))[index]}
  
  
  res <- boot(data = aggregate.suicides,
              statistic = beta1,
              R = 1000)
  
  beta1.full <- coef(lm(log(Suicides.per100 + 1) ~ Development + Region + GDP.per.capita + Year,
                        data=aggregate.suicides))[index]
  
  bias <- mean(res$t) - beta1.full
  
  unbiased.est <- res$t - bias
  
  conf.low <- quantile(unbiased.est, 0.025)
  conf.up <- quantile(unbiased.est, 0.975)
  
  beta.H0 <-res$t - mean(res$t) + bias
  
  p.val <- mean(abs(beta.H0) >= abs(beta1.full))
  
  beta.estimates <- append(beta.estimates, beta1.full)
  p.values <- append(p.values, p.val)
  confidence.lower <- append(confidence.lower, conf.low)
  confidence.upper <- append(confidence.upper, conf.up)
  
  
}

boot.info <- data.frame( beta.estimates, p.values)
colnames(boot.info) <- c( "Estimates", "P.value", "Conf.lower", "Conf.upper")
# more accurate p-values
boot.info

# dataframe with old and new p-values
p.val.compare3 <- data.frame(beta.estimates, agg.fit4.sum$coefficients[2:13,4], p.values)
colnames(p.val.comapare3) <- c("estimates", "lm() p-values", "bootstrap p-values")

p.val.compare3

# this model is marginally worse by the AIC
AIC(agg.fit4)


### Final Model without interaction ###

# for the sake of looking for a simpler model, I will be using the model with Region, Year, and GDP.per.capita

agg.fit3
agg.fit3.sum
p.val.comapare2
AIC(agg.fit3)


### Interaction Effects ###

# let's see if there is interaction between GDP and Region
interaction.fit1 <- lm(log(Suicides.per100+1) ~ Region + GDP.per.capita + Year + GDP.per.capita:Region, data = aggregate.suicides)
interaction.fit1.sum <- summary(interaction.fit1)

# variance is fine
plot(interaction.fit1, 1)
# normality is broken so we will get p-values through bootstrapping
plot(interaction.fit1, 2)


# let's do bootstrapping to get better p-values since we have the residual normality assumption broken
n <- nrow(aggregate.suicides)
i <- sample(1:n, n, replace=T)
set.seed(12)
p.values <- c()
confidence.lower <- c()
confidence.upper <- c()
beta.estimates <- c()

for (index in 2:17){
  beta1 <- function(x,i) { coef(lm(log(Suicides.per100 + 1) ~ Region + GDP.per.capita + Year + GDP.per.capita:Region,
                                   data = x,
                                   subset = i))[index]}
  
  
  res <- boot(data = aggregate.suicides,
              statistic = beta1,
              R = 1000)
  
  beta1.full <- coef(lm(log(Suicides.per100 + 1) ~ Region + GDP.per.capita + Year + GDP.per.capita:Region,
                        data=aggregate.suicides))[index]
  
  bias <- mean(res$t) - beta1.full
  
  unbiased.est <- res$t - bias
  
  conf.low <- quantile(unbiased.est, 0.025)
  conf.up <- quantile(unbiased.est, 0.975)
  
  beta.H0 <-res$t - mean(res$t) + bias
  
  p.val <- mean(abs(beta.H0) >= abs(beta1.full))
  
  beta.estimates <- append(beta.estimates, beta1.full)
  p.values <- append(p.values, p.val)
  confidence.lower <- append(confidence.lower, conf.low)
  confidence.upper <- append(confidence.upper, conf.up)
  
  
}

boot.info <- data.frame( beta.estimates, p.values)
colnames(boot.info) <- c( "Estimates", "P.value", "Conf.lower", "Conf.upper")
# more accurate p-values
boot.info

# dataframe with old and new p-values
p.val.compare4 <- data.frame(beta.estimates, interaction.fit1.sum$coefficients[2:17,4], p.values)
colnames(p.val.comapare4) <- c("estimates", "lm() p-values", "bootstrap p-values")

p.val.compare4

# this model is the best so far
AIC(interaction.fit1)



## Let's add interaction between year and region as well
interaction.fit2 <- lm(log(Suicides.per100+1) ~ Region + GDP.per.capita + Year + GDP.per.capita:Region + Year:Region, data = aggregate.suicides)
interaction.fit2.sum <- summary(interaction.fit2)

# variance is fine
plot(interaction.fit2, 1)
# normality is broken so we will get p-values through bootstrapping
plot(interaction.fit2, 2)


# let's do bootstrapping to get better p-values since we have the residual normality assumption broken
n <- nrow(aggregate.suicides)
i <- sample(1:n, n, replace=T)
set.seed(12)
p.values <- c()
confidence.lower <- c()
confidence.upper <- c()
beta.estimates <- c()

for (index in 2:23){
  beta1 <- function(x,i) { coef(lm(log(Suicides.per100 + 1) ~ Region + GDP.per.capita + Year + GDP.per.capita:Region + Year:Region,
                                   data = x,
                                   subset = i))[index]}
  
  
  res <- boot(data = aggregate.suicides,
              statistic = beta1,
              R = 1000)
  
  beta1.full <- coef(lm(log(Suicides.per100 + 1) ~ Region + GDP.per.capita + Year + GDP.per.capita:Region + Year:Region,
                        data=aggregate.suicides))[index]
  
  bias <- mean(res$t) - beta1.full
  
  unbiased.est <- res$t - bias
  
  conf.low <- quantile(unbiased.est, 0.025)
  conf.up <- quantile(unbiased.est, 0.975)
  
  beta.H0 <-res$t - mean(res$t) + bias
  
  p.val <- mean(abs(beta.H0) >= abs(beta1.full))
  
  beta.estimates <- append(beta.estimates, beta1.full)
  p.values <- append(p.values, p.val)
  confidence.lower <- append(confidence.lower, conf.low)
  confidence.upper <- append(confidence.upper, conf.up)
  
  
}

boot.info <- data.frame( beta.estimates, p.values)
colnames(boot.info) <- c( "Estimates", "P.value", "Conf.lower", "Conf.upper")
# more accurate p-values
boot.info

# dataframe with old and new p-values
p.val.compare5 <- data.frame(beta.estimates, interaction.fit2.sum$coefficients[2:23,4], p.values)
colnames(p.val.compare5) <- c("estimates", "lm() p-values", "bootstrap p-values")

p.val.compare5

# this model is now the best so far
AIC(interaction.fit2)

# although this model seems the best and the bootstrap p-values are signifcant for about every variable, this seems to be getting close
# to overfitting... for his reason, as well as interpretability, the model with year, region, GDP, and interaction between region and GDP will be used




### Final Interaction Model ###

interaction.fit1
interaction.fit1.sum
p.val.compare4
AIC(interaction.fit1)

# summaries

# R^2
interaction.fit1.sum$r.squared

# constant variance
plot(interaction.fit1, 1)

# the normality assumption for residuals is broken so we did bootstrapping for the p-values
plot(interaction.fit1, 2)

# p-values
p.val.compare4

## Graphing the model with year held constant

  

## Graphing the model with year and GDP changing

  
  
  
  
  
  
  
  
#########################################################################
sex.seq <- c("male", "female")

predict.data.demo <- data.frame(Region = "North America", Development = "high", age = "25-34 years", gdp_per_capita = gdp.seq,
                                population = mean.pop, sex = "female", year = 2015)

demo.predict <- predict(demo.fit.all, newdata = predict.data.demo)

demo.regular <- exp(demo.predict)

plot(demo.regular ~ gdp.seq)

demo.predict

z <- outer(gdp.seq, sex.seq, function(a, b) predict(demo.fit.all2, newdata = data.frame(Region = "North America",
                                                                                        Development = "high", age = "25-34 years", 
                                                                                        gdp_per_capita = a, population = mean.pop, sex = b, year = 2015)))
#################################################################################################
## interpretations







## Let's make a model with the interaction between year and Region (removing GDP) for the sake of interpretation
interaction.fit3 <- lm(log(Suicides.per100+1) ~ Region + Year + Year:Region, data = aggregate.suicides)
interaction.fit3.sum <- summary(interaction.fit3)

# variance is fine
plot(interaction.fit3, 1)
# normality is broken so we will get p-values through bootstrapping
plot(interaction.fit3, 2)


# let's do bootstrapping to get better p-values since we have the residual normality assumption broken
n <- nrow(aggregate.suicides)
i <- sample(1:n, n, replace=T)
set.seed(12)
p.values <- c()
confidence.lower <- c()
confidence.upper <- c()
beta.estimates <- c()

for (index in 2:16){
  beta1 <- function(x,i) { coef(lm(log(Suicides.per100 + 1) ~ Region + Year + Year:Region,
                                   data = x,
                                   subset = i))[index]}
  
  
  res <- boot(data = aggregate.suicides,
              statistic = beta1,
              R = 1000)
  
  beta1.full <- coef(lm(log(Suicides.per100 + 1) ~ Region +Year + Year:Region,
                        data=aggregate.suicides))[index]
  
  bias <- mean(res$t) - beta1.full
  
  unbiased.est <- res$t - bias
  
  conf.low <- quantile(unbiased.est, 0.025)
  conf.up <- quantile(unbiased.est, 0.975)
  
  beta.H0 <-res$t - mean(res$t) + bias
  
  p.val <- mean(abs(beta.H0) >= abs(beta1.full))
  
  beta.estimates <- append(beta.estimates, beta1.full)
  p.values <- append(p.values, p.val)
  confidence.lower <- append(confidence.lower, conf.low)
  confidence.upper <- append(confidence.upper, conf.up)
  
  
}

boot.info <- data.frame( beta.estimates, p.values)
colnames(boot.info) <- c( "Estimates", "P.value", "Conf.lower", "Conf.upper")
# more accurate p-values
boot.info

# dataframe with old and new p-values
p.val.compare6 <- data.frame(beta.estimates, interaction.fit3.sum$coefficients[2:16,4], p.values)
colnames(p.val.compare6) <- c("estimates", "lm() p-values", "bootstrap p-values")

p.val.compare6



## Graphing the interaction between year and region holding GDP constant



## interpreting the interactions and comparing the trends of suicides over the years by region



## other


interaction.fit1 <- lm(sqrt(Suicides.per100) ~ Region + GDP.per.capita + Year + GDP.per.capita:Region, data = aggregate.suicides)
summary(interaction.fit1)
plot(interaction.fit1, 1)
plot(interaction.fit1, 2)

interaction.fit2 <- lm(log(Suicides.per100 +.1) ~ Region + GDP.per.capita + Year + GDP.per.capita:Region, data = aggregate.suicides)
interaction.fit2.sum <- summary(interaction.fit2)


n <- nrow(aggregate.suicides)
i <- sample(1:n, n, replace=T)
set.seed(12)
p.values <- c()
confidence.lower <- c()
confidence.upper <- c()
beta.estimates <- c()

for (index in 2:17){
  beta1 <- function(x,i) { coef(lm(log(Suicides.per100 + .1) ~ Region + Year + GDP.per.capita:Region,
                                   data = x,
                                   subset = i))[index]}
  
  
  res <- boot(data = aggregate.suicides,
              statistic = beta1,
              R = 1000)
  
  beta1.full <- coef(lm(log(Suicides.per100 + .1) ~ Region +Year + GDP.per.capita:Region,
                        data=aggregate.suicides))[index]
  
  bias <- mean(res$t) - beta1.full
  
  unbiased.est <- res$t - bias
  
  conf.low <- quantile(unbiased.est, 0.025)
  conf.up <- quantile(unbiased.est, 0.975)
  
  beta.H0 <-res$t - mean(res$t) + bias
  
  p.val <- mean(abs(beta.H0) >= abs(beta1.full))
  
  beta.estimates <- append(beta.estimates, beta1.full)
  p.values <- append(p.values, p.val)
  confidence.lower <- append(confidence.lower, conf.low)
  confidence.upper <- append(confidence.upper, conf.up)
  
  
}

boot.info <- data.frame( beta.estimates, p.values)
colnames(boot.info) <- c( "Estimates", "P.value", "Conf.lower", "Conf.upper")
# more accurate p-values
boot.info

# dataframe with old and new p-values
p.val.compare7 <- data.frame(beta.estimates, interaction.fit2.sum$coefficients[2:17,4], p.values)
colnames(p.val.compare7) <- c("estimates", "lm() p-values", "bootstrap p-values")

p.val.compare7

####beta hats graphs of distribution for bootstrapping to check normality... compare + .1 and + 1 and mention difference you weren't sure with what
# to go with
# take into aaccount we cant predict too much gdp so limit to +- 10% of min and max GDP in the graphs... explain and think about why this mkes sense