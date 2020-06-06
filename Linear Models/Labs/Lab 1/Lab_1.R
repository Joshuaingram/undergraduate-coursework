#########################################################################
### TASK #1: Education-Crime data (see "Data sets" folder on Canvas) ####
#########################################################################
library(readr)
library(tidyverse)
library(rgl)

# - Read in the data (via "Import Dataset"). Make sure it's called "fl_crime"
fl_crime <- read.csv("C:/Users/joshi/OneDrive/Education/New College of Florida/By-Year Folders/Spring 2020/Linear Models/fl_crime.csv")

# - Explore the data set.
head(fl_crime)

# - Clean the column names.
ls(fl_crime)
colnames(fl_crime) <- c("county", "crime", "education", "urbanization", "income")
head(fl_crime)

# - Proceed to study relationship between crime & education 
#   (plotting + linear regression)
ggplot(data=fl_crime,
       aes(x=education, y=crime)) + geom_point() + geom_abline()

lm_obj_crime <- lm(crime ~ education, data = fl_crime)
summary(lm_obj_crime)

plot(lm_obj_crime, 1)
plot(lm_obj_crime, 2)

# - Interpret the coefficients (slope & intercept). Anything weird?

# intercept - doesn't make too much sense to interpret the intercept

# education - On average, for a one percentage point increase in education we expect to see a 1.486 unit increase in crime rate per 1000 people.


# - Check for "lurking variables" (How?)
# crime rate increases with education... that's odd

plot(fl_crime[,-1])
cor(fl_crime[,-1])
# perhaps urbanization could be this lurking variable and it seems it would make the most sense. It has a higher correlation with both crime and education

# - Include the potentially causal lurking (or "confounding") variable as a second predictor.

lm_obj_crime2 <- lm(crime ~ education + urbanization, data = fl_crime)
summary(lm_obj_crime2)

plot(lm_obj_crime2, 1)
plot(lm_obj_crime2, 2)

# - Provide the interactive plot of the fitted plane.
attach(fl_crime)

plot3d(lm_obj_crime2, size=5, col=1, data = fl_crime)

# - Add the lines showing the residuals (see "Lecture_2.R")

plot3d(lm_obj_crime2, size=5, col=1, data = fl_crime)

segments3d(rep(education, each=2), rep(urbanization, each=2), 
           z=matrix(t(cbind(crime,predict(lm_obj_crime2))), nc=1), 
           add=T, 
           lwd=2, 
           col=2)



##################################
### TASK #2: PRESTIGE DATA.  #####
##################################


# - Read in the data (via "Import Dataset").
Prestige <- read.csv("C:/Users/joshi/Downloads/Prestige.txt", sep="")

# - Provide a multivariate plot of all QUANTITATIVE variables
#   (will need to dispose of one column).
head(Prestige)

plot(Prestige[,c(-5, -6)])

cor(Prestige[,c(-5, -6)])

# - Which TWO variables ables show pronounced relationships with "prestige"?

# census doesn't really make sense to look at a relationship with prestige... "education" and "income" have the two most pronounced relationship with prestige..
# not followed very closely by "women"

# - Proceed to fit a multiple linear regression of prestige on those three variables.

lm_obj_prest <- lm(prestige ~ education + income + women, data = Prestige)
summary(lm_obj_prest)

# - Print out the fitted multiple linear regression equation.

# will use markdown to print this out to look nice
coef(lm_obj_prest)

# - Obtain and interpret:
#      1. All three slope estimates.
#      2. Residual Standard Error.
#      3. R^2.

# 1. 
# education - 
# income - 
# women - 

# 2. RSE = 7.846

# 3. R^2 = 0.7982


# - Obtain and compare the standardized slopes for all three variables.

scaled_Prestige <- data.frame(scale(Prestige[,c(-5,-6)]))
lm_obj_prest2 <- lm(prestige ~ education + income + women, data = scaled_Prestige)
summary(lm_obj_prest2)

# to scale a variable x, we take (x_i - mean) / (SD(x))