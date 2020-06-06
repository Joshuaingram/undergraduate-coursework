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


# removing 0s for log transformation
demo.subset.rm <- demographic.suicides[demographic.suicides$suicides.100k.pop != 0,]

# number of rows with observations of 0 suicides per 100k
nrow(demographic.suicides[demographic.suicides$suicides.100k.pop == 0,])

# proportion of observations with 0 suicides... 15.04%
nrow(demographic.suicides[demographic.suicides$suicides.100k.pop == 0,]) / nrow(demographic.suicides)

# fitting the log model without a + 1
demo.fit.all3 <- lm(log(suicides.100k.pop) ~ Region + Development + age + gdp_per_capita + population + sex + year, data = demo.subset.rm)
summary(demo.fit.all3)

# residual diagnostics are fine... let's compare the prediction plots between with 0s and without
plot(demo.fit.all3, 1)
plot(demo.fit.all3, 2)

# plotting the two
# plot for log + 1
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

ggplot(demo.pred.data, aes(x = gdp_per_capita, y = exp(suicides.100k.pop)-1, color = sex)) + 
  geom_line() + 
  labs(x = "GDP per capita", y = "Predicted Suicides per 100k", title = "Predicted Suicides for Males and Females") +
  lims(x = c(0, 120000), y = c(0, 100))

# this model is not predicting properly... at all... this is because it is modeling a totally different response log(y +1) not log(y)
mean(aggregate.suicides[aggregate.suicides$Year == 2015 & aggregate.suicides$Region == "North America",]$Suicides.per100, na.rm = TRUE)

# plot for removed 0s
mean.pop <- mean(aggregate.suicides[aggregate.suicides$Region == "North America" & aggregate.suicides$Year == 2015,]$Population, na.rm = TRUE)
year.pred <- 2015
age.pred <- "25-34 years"
region.pred <- "North America"
develop.pred <- "high"
gdp.seq <- seq(min(aggregate.suicides$GDP.per.capita), max(aggregate.suicides$GDP.per.capita), length.out = 1000)
sex.pred <- c("male", "female")

expand.grid.demo <- expand.grid(region.pred, develop.pred, age.pred, gdp.seq, mean.pop, sex.pred, year.pred)
colnames(expand.grid.demo) <- c("Region", "Development", "age", "gdp_per_capita", "population", "sex", "year")

demo.expand.pred <- predict(demo.fit.all3, newdata = expand.grid.demo)

demo.pred.data <- cbind(expand.grid.demo, demo.expand.pred)
colnames(demo.pred.data) <- c("Region", "Development", "age", "gdp_per_capita", "population", "sex", "year", "suicides.100k.pop")

ggplot(demo.pred.data, aes(x = gdp_per_capita, y = exp(suicides.100k.pop), color = sex)) + 
  geom_line() + 
  labs(x = "GDP per capita", y = "Predicted Suicides per 100k", title = "Predicted Suicides for Males and Females") +
  lims(x = c(0, 120000), y = c(0, 6))

# this model does much better at predicting realistic values... although a zero inflated model would be much prefferred.... I'm going with 
# removing 0s for the rest of the project
mean(aggregate.suicides[aggregate.suicides$Year == 2015 & aggregate.suicides$Region == "North America",]$Suicides.per100, na.rm = TRUE)

### Final Demographic Model ###
summary(demo.fit.all2)


