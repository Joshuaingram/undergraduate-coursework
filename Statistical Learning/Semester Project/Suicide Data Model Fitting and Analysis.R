### Suicide Data Visualization and Model Fitting ###

library(tidyverse)
library(RColorBrewer)
library(pscl)
library(boot)
library(bootstrap)
library(car)
library(knitr)

# number of suicides in the US over time
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

# percent change from 1990 to 2015... 43.03%
(aggregate.suicides[aggregate.suicides$Year == 2015 & aggregate.suicides$Country == "United States",]$Suicides - 
  aggregate.suicides[aggregate.suicides$Year == 1990 & aggregate.suicides$Country == "United States",]$Suicides) /
  aggregate.suicides[aggregate.suicides$Year == 1990 & aggregate.suicides$Country == "United States",]$Suicides * 100

# number of suicides in the UK, for comparison
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


## plotting demographic fitting ##

ls(demographic.suicides)

demo.fit.all1 <- lm(suicides.100k.pop  ~ Region + Development + age + gdp_per_capita + population + sex + year, data = demographic.suicides)


demo.fit.all2 <- lm(log(suicides.100k.pop +1) ~ Region + Development + age + gdp_per_capita + population + sex + year, data = demographic.suicides)

demo.fit.all3 <- lm(log(suicides.100k.pop) ~ Region + Development + age + gdp_per_capita + population + sex + year, data = demographic.suicides,
                   subset = suicides.100k.pop != 0)

summary(demo.fit.all1)
summary(demo.fit.all2)
summary(demo.fit.all3)


plot(demo.fit.all1,1)
plot(demo.fit.all2,1)
plot(demo.fit.all3,1)


ggplot(aes(x = suicides.100k.pop), data = demographic.suicides[demographic.suicides$age == "5-14 years",]) + geom_histogram()
ggplot(aes(x = suicides.100k.pop), data = demographic.suicides[demographic.suicides$age == "15-24 years",]) + geom_histogram()
ggplot(aes(x = suicides.100k.pop), data = demographic.suicides[demographic.suicides$age == "25-34 years",]) + geom_histogram()
ggplot(aes(x = suicides.100k.pop), data = demographic.suicides[demographic.suicides$age == "35-54 years",]) + geom_histogram()
ggplot(aes(x = suicides.100k.pop), data = demographic.suicides[demographic.suicides$age == "55-74 years",]) + geom_histogram()
ggplot(aes(x = suicides.100k.pop), data = demographic.suicides[demographic.suicides$age == "75+ years",]) + geom_histogram()

# The distribution of suicides per 100k shows that the majoirty of the them are 0 ir very close for all age ranges... this could be the cause of
# the odd residual vs fitted plot... we should use a zero-inflated model... but we can still gain valuable insight into trends even if this model
# is not perfect... this should be considered and inference should be made with caution

# final model for demographic information
plot(demo.fit.all2, 1)
plot(demo.fit.all2, 2)
summary(demo.fit.all2)$coef

# interpret... plot? blah blah blah (do this in the powerpoint)



## aggregate fitting ##

# Note... I went through this process and found that I need to take the log of my response. I am going to remove the values with 0 suicides
# from the data... this is 2% of the data so it is very low... if I shouldn't do this maybe just add .01 to the log?
subset.aggregate.data <- aggregate.suicides[aggregate.suicides$Suicides.per100 != 0,]

# Let's check for any correlations between the variables
cor(subset(subset.aggregate.data, select = c(Year, GDP.per.capita, Population, Suicides.per100)))
# good news... no! not for my numerical variables

ls(aggregate.suicides)

agg.fit1 <- lm(Suicides.per100 ~ Development + Region + GDP.per.capita + Population + Year, data = aggregate.suicides)

nrow(aggregate.suicides[aggregate.suicides$Suicides.per100 == 0,])

# we only have 56 rows where the suicides == 0 (out of 2070), so we are removing them in the model so we just have a log response
agg.fit2 <- lm(log(Suicides.per100) ~ Development + Region + GDP.per.capita + Population + Year, data = aggregate.suicides, subset = Suicides.per100 != 0)
summary(agg.fit1)

plot(agg.fit1, 1)
plot(agg.fit1, 2)

plot(agg.fit2, 1)
plot(agg.fit2, 2)


# look at interaction between year and region (see how the slopes may differ and compare trends)
# maybe try a different model? Why are there groups of data in the logged response model shown in the residual vs fitted plot?
# 
# move on to next model and check for interactions... make sure this doesn't happen again... if it does it could be the region... maybe
# there is a region with only 1-5 countries in it?

# dont forget to look and see which categorical variables are the baselines by checking to see which are not included
# if i do the bootstraps... check to make sure AIC is still appropriate with the differing p-values... if not find another way for model selection
# im going to have to do bootstrap sampling when checking for the signicance of interaction in my model

# check for correlation between variables first!!!!!! maybe development and Region are correlated, or GDP and Development? 
# check multiple linear regression slides for AIC to see if its appropriate when Bootstrap is necessary... should be?



######## Normality and equal variance assumption of residuals is broken... going to attempt bootstrapping for Beta estimates ##########

# model to be used so the equal variance assumption isn't broken:
agg.fit2 <- lm(log(Suicides.per100) ~ Development + Region + GDP.per.capita + Population + Year, data = aggregate.suicides, subset = Suicides.per100 != 0)


# here's what we are working with:
plot(agg.fit2, 1)
plot(agg.fit2, 2)

#################################################################################################################################
# let's try bootstrapping
#n <- nrow(aggregate.suicides)
#i <- sample(1:n, n, replace=T)

#subset.aggregate.data <- aggregate.suicides[aggregate.suicides$Suicides.per100 != 0,]

#beta1 <- function(x,i) { coef(lm(log(Suicides.per100) ~ Development + Region + GDP.per.capita + Population + Year,
#                                 data=x,
#                                 subset=i))[12]}
#res <- boot(data = subset.aggregate.data, 
#            statistic = beta1, 
#            R = 1000)

# distribution of residuals for bootstrap
#res$t
#hist(res$t)
# versus non bootstrap
#hist(agg.fit2$residuals)

# standard errors
#res

# confidence intervals for beta hats
#beta1.full <- coef(lm(log(Suicides.per100) ~ Development + Region + GDP.per.capita + Population + Year,
#                      data=subset.aggregate.data))[12]

# dealing with bias
#bias <- mean(res$t) - beta1.full

#unbiased.est <- res$t - bias

#c(quantile(unbiased.est, 0.025), quantile(unbiased.est, 0.975))


#hist(res$t, breaks=30)

#beta1.under.H0 <- res$t - mean(res$t) + bias 


#h.obj <- hist(beta1.under.H0, breaks=30)

# p-value calculation
#mean(abs(beta1.under.H0) >= abs(beta1.full))


#h.obj <- hist(beta1.under.H0,  breaks=30,
#              col = ifelse(abs(h.obj$breaks) >= abs(beta1.full), 1,0))
####################################################################################

# trying to calculate all the p-values through a bootstrap sample #
# (copying some code to make it easier to figure out... ignore redundancy)
n <- nrow(subset.aggregate.data)
i <- sample(1:n, n, replace=T)
set.seed(12)
coef.names <- c("Developmentlow or very low", "Developmentmedium", "Developmentunknown", "RegionAsia", "RegionCaribbean", "RegionCentral America",
                "RegionEurope", "RegionNorth America", "RegionOceania", "RegionSouth America", "GDP.per.capita", "Population", "Year")
p.values <- c()
confidence.lower <- c()
confidence.upper <- c()
beta.estimates <- c()

for (index in 2:14){
  beta1 <- function(x,i) { coef(lm(log(Suicides.per100) ~ Development + Region + GDP.per.capita + Population + Year,
                                data = x,
                                subset = i))[index]}
  
  
  res <- boot(data = subset.aggregate.data,
              statistic = beta1,
              R = 1000)
  
  beta1.full <- coef(lm(log(Suicides.per100) ~ Development + Region + GDP.per.capita + Population + Year,
                        data=subset.aggregate.data))[index]
  
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

boot.info <- data.frame( beta.estimates, p.values, round(confidence.lower, 4), round(confidence.upper, 4))
colnames(boot.info) <- c( "Estimates", "P.value", "Conf.lower", "Conf.upper")
boot.info

# now that the bootstrap samples are done... look at p-values and see which variables are significant in prediction... consider using VIF or manually
# removing variables, then compare AICs
# Once that's done and the model is prettier, check for interaction (or maybe do this before model selection?) between GDP and development/region, as
# well as year and region/development
# After this, I'll need to do the bootstrapping for the interaction variables and go from there
# once the final model is selected, hold some variables constant and then graph the the suicides per 100k with the interaction effects
# after that, go ahead and interpret the model beta estimates (check to see if log + 1 or log with removed 0s is better) (don't forget baselines for categorical predictors)
# after that, breifly analyze the importants of the findings, etc. and then finish the powerpoint!

# analysis off full model

# firstly, we know that we broke the resiudal normality assumption so we performed bootstrapping to check the significance of each variable
# However, we get a very tiny p-value (basically 0) for the overall model... With this in mind, we know our model performed well but there 
# could be some  redundancy in our variable selection
summary(agg.fit1)

# here are the post-bootstrap p-values and confidence intervals for the beta estimates
boot.info


# we see that the development variable and population seem to be pretty poor predictor variables... let's do bootstrapping for a model without these two
# and then use AIC for model comparison
agg.fit3 <- lm(log(Suicides.per100) ~ Region + GDP.per.capita + Year, data = subset.aggregate.data)
summary(agg.fit3)
# this model is still very significant


n <- nrow(subset.aggregate.data)
i <- sample(1:n, n, replace=T)
set.seed(12)
coef.names <- c( "RegionAsia", "RegionCaribbean", "RegionCentral America",
                "RegionEurope", "RegionNorth America", "RegionOceania", "RegionSouth America", "GDP.per.capita", "Year")
p.values <- c()
confidence.lower <- c()
confidence.upper <- c()
beta.estimates <- c()

for (index in 2:10){
  beta1 <- function(x,i) { coef(lm(log(Suicides.per100) ~ Region + GDP.per.capita + Year,
                                   data = x,
                                   subset = i))[index]}
  
  
  res <- boot(data = subset.aggregate.data,
              statistic = beta1,
              R = 1000)
  
  beta1.full <- coef(lm(log(Suicides.per100) ~  Region + GDP.per.capita + Year,
                        data=subset.aggregate.data))[index]
  
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

boot.info2 <- data.frame( beta.estimates, p.values, round(confidence.lower, 4), round(confidence.upper, 4))
colnames(boot.info2) <- c( "Estimates", "P.value", "Conf.lower", "Conf.upper")
boot.info2
# well... region seems to be a significant preictor and year is on the line... GDP per capita, which is what I'm interested in, isn't very signifcant
# maybe adding an intercation effect between GDP and region would be s ignifcant? Let's find out and look at the plot below:
plot(Suicides.per100 ~ GDP.per.capita, data = subset.aggregate.data, col = Region)

# firstly comparing AICs between models with/without region/development... region is much better than Development... so development is being dropped.
# comparing model with region and GDP and without, AIC is only marginally better... we will move on to add the interaction effect with GDP and Region since AICs are close
AIC(agg.fit3)

agg.fit5 <- lm(log(Suicides.per100) ~ Development + Year + GDP.per.capita, data = subset.aggregate.data)
summary(agg.fit5)
AIC(agg.fit5)

# without GDP
agg.fit4 <- lm(log(Suicides.per100) ~ Region+ Year, data = subset.aggregate.data)
summary(agg.fit4)
AIC(agg.fit4)



# interaction effect with region and GDP #
interaction.fit1 <- lm(log(Suicides.per100) ~ Region + GDP.per.capita + Year + Region:GDP.per.capita, data = subset.aggregate.data)
summary(interaction.fit1)

plot(interaction.fit1, 2)
# normality assumption still broken so we need to bootstrap for p-values


n <- nrow(subset.aggregate.data)
i <- sample(1:n, n, replace=T)
set.seed(12)

p.values <- c()
confidence.lower <- c()
confidence.upper <- c()
beta.estimates <- c()

for (index in 2:17){
  beta1 <- function(x,i) { coef(lm(log(Suicides.per100) ~ Region + GDP.per.capita + Year + Region:GDP.per.capita,
                                   data = x,
                                   subset = i))[index]}
  
  
  res <- boot(data = subset.aggregate.data,
              statistic = beta1,
              R = 1000)
  
  beta1.full <- coef(lm(log(Suicides.per100) ~  Region + GDP.per.capita + Year + Region:GDP.per.capita,
                        data=subset.aggregate.data))[index]
  
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

boot.info3 <- data.frame( beta.estimates, p.values, round(confidence.lower, 4), round(confidence.upper, 4))
colnames(boot.info3) <- c( "Estimates", "P.value", "Conf.lower", "Conf.upper")
boot.info3

# interaction effect is significant and now GDP is a significant predictor
# here's the AIC which is the best so far
AIC(interaction.fit1)





# next look at the interaction effects of year and region to see the trends of suicides over the years

# adding year and region interaction #
interaction.fit2 <- lm(log(Suicides.per100) ~ Region + GDP.per.capita + Year + Region:GDP.per.capita + Year:Region, data = subset.aggregate.data)
summary(interaction.fit2)

plot(interaction.fit2, 2)
# normality assumption still broken so we need to bootstrap for p-values


n <- nrow(subset.aggregate.data)
i <- sample(1:n, n, replace=T)
set.seed(12)

p.values <- c()
confidence.lower <- c()
confidence.upper <- c()
beta.estimates <- c()

for (index in 2:24){
  beta1 <- function(x,i) { coef(lm(log(Suicides.per100) ~ Region + GDP.per.capita + Year + Region:GDP.per.capita + Year:Region,
                                   data = x,
                                   subset = i))[index]}
  
  
  res <- boot(data = subset.aggregate.data,
              statistic = beta1,
              R = 1000)
  
  beta1.full <- coef(lm(log(Suicides.per100) ~  Region + GDP.per.capita + Year + Region:GDP.per.capita + Year:Region,
                        data=subset.aggregate.data))[index]
  
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

boot.info4 <- data.frame( beta.estimates, p.values, round(confidence.lower, 4), round(confidence.upper, 4))
colnames(boot.info4) <- c("Estimates", "P.value", "Conf.lower", "Conf.upper")
boot.info4


# here's the AIC
AIC(interaction.fit2)
# this is the best so far and now all of our predictors are siginificant... however this may be over fitting... and I'm starting to believe it is


interaction.fit3 <- lm(log(Suicides.per100) ~ Region + GDP.per.capita + Year + Year:Region, data = subset.aggregate.data)
summary(interaction.fit3)
AIC(interaction.fit3)

# by this time I have compared all of my models and I will either go with the model with an interaction between region and gdp, or the
# one with the interaction effects between region and GDP, as well as region and year... the larger model may be close to overfitting and I want to avoid that
# for this purpose, I may just go with the region and gdp interaction model for the bulk of the project (remember to look at R^2)
# if I do that, which I think I will to avoid overfitting, I will make a model without the gdp-region interaction, but rather the
# year-region interaction to look at the trends of suicides in the different regions
# So once the models are selected, I should look at the summaries and residuals, then it's ability to model the data.
# After this, I should look at the comparisons to different models and talk about why I went with this
# Then I need to graph the the model... try holding year constant (the year with the least missing data) and then look at a 2d plot and multiple lines for the regions
# after this, interpret the effects of GDP on suicide counts and how region affects this (which has the greatest effect, the least?)
# Then, attempt to make a 3d model with year as the x axis, gdp as the y, and z as the suicides... then have different planes for the different regions
# conclude the prohect with talking about the differences between regions and how gdp effects this... is there a large effect?  how is this useful?


### Final Model Selection ###

cor(subset(subset.aggregate.data, select = c(Year, GDP.per.capita, Population, Suicides.per100)))

final.fit <- interaction.fit1
final.fit.summary <- summary(final.fit)

# there is basically constant variance
plot(interaction.fit1, 1)

# the normality assumption of the residuals is broken... thus we did bootstrapping for the beta estimates and p-values
plot(interaction.fit1, 2)

# table of estimates
boot.info3

# R^2 of .2789... 27.89% of the variance is explained by our model
final.fit.summary$r.squared

# this is the lowest AIC beyond the model with the additional interaction between GDP and year... which may be overfitting
AIC(final.fit)

# Graph of the model with year held constant.... 2015... then interpretation

fit.without <- lm(log(Suicides.per100) ~ GDP.per.capita + Year, data = subset.aggregate.data)

x <- seq(min(subset.aggregate.data$GDP.per.capita), max(subset.aggregate.data$GDP.per.capita), length = 1000)
y <- seq(min(subset.aggregate.data$GDP.per.capita), max(subset.aggregate.data$GDP.per.capita), length = 1000)

z <- outer(x, y, function(a,b) predict(fit.without, newdata = data.frame(Year = a, GDP.per.capita = b)))



# Graph  of the 3-dimensional model