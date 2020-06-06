#################################################################
### Categorical Data Analysis Final Project Analysis and Cod e###
#################################################################
### Joshua D. Ingram ############################################
#################################################################

set.seed(1)

# Package and Initial Data Loading
library(tidyverse)
library(RColorBrewer)
library(vcd)
library(plyr)
library(VGAM)
library(car)

lc.loans <- read.csv("C:\\Users\\joshi\\Documents\\CDA Final Project Loan Data\\loan.csv")

ls(lc.loans)

a.grades <- c("A")
b.grades <- c("B")
c.grades <- c("C")
d.grades <- c("D")
e.less.grades <- c("E", "F", "G")

lc.loans <- mutate(lc.loans, grade = factor(case_when(grade %in% a.grades ~ "A",
                                                grade %in% b.grades ~ "B",
                                                grade %in% c.grades ~ "C",
                                                grade %in% d.grades ~ "D",
                                                grade %in% e.less.grades ~ "E-G")))

# dropping ANY category from home_ownership because it is an NA value
any.subset <- subset(lc.loans, home_ownership != "ANY")
any.subset
lc.loans <- droplevels(any.subset)
levels(lc.loans$home_ownership)

levels(lc.loans$grade)
### Missing Values, Data Cleaning, and Variable Selection ###

# proportion of NA's before variable selection... 23.84%
mean(is.na(lc.loans))

# variables of interest selection
loans.selection <- data.frame(lc.loans$grade, lc.loans$annual_inc, lc.loans$application_type, lc.loans$dti,
                    lc.loans$home_ownership, lc.loans$loan_amnt,  lc.loans$mort_acc)


# proportion of NA's after variable selection... .33%
mean(is.na(loans.selection))

# removal of rows with NA's
loans <- na.omit(loans.selection)

# renaming columns
colnames(loans) <- c("Grade", "Annual_Income", "Application_Type", "Debt_to_Income",
                                 "Home_Ownership", "Loan_Amount", "Mortgage_Accounts")

loans$Grade <- factor(loans$Grade, ordered = TRUE)

head(loans)
ls(loans)

# due to the data being so big and my computer being unable to calculate a model that is 12gb,  I have no choice but to 
# randomly select data from the whole dataset that will be used for my models... I will keep population data for visualizations
loans.sample <- loans[sample(nrow(loans), 20000, replace = FALSE),]
head(loans.sample)
### Data Overview, Visualizations, and Tables ###

# possible categories for Loan Grade
levels(loans$Grade)

# count table and prop table and prop into data frame to be graphed
grade.counts.table <- table(loans$Grade)
grade.prop.long <- as.data.frame(as.table(prop.table(grade.counts.table)))
colnames(grade.prop.long) <- c("Grade", "Proportion")
grade.counts.table
grade.prop.table <- prop.table(grade.counts.table)

# distribution of grades visualized (darker red means greater risk)
ggplot(data = grade.prop.long, aes(x = Grade, y = Proportion, fill = Grade)) + 
  geom_bar(stat = "identity", width = .8) + 
  geom_bar(stat = "identity", width = .8, color = "black", show.legend = F, alpha = 1) + 
  labs(y = "Proportion", title = "Distribution of Loan Grades") + 
  scale_fill_manual(name = "Grade", values = brewer.pal(7, "Reds")[1:7])

# let's compare the conditional distribution of grades given application type (single or joint)
# number of application types
nrow(loans[which(loans$`Application Type` == "Individual"),])
nrow(loans[which(loans$`Application Type` == "Joint App"),])

grades.apptype.table <- table(loans$Application_Type, loans$Grade)
grades.apptype.matrix <- as.matrix(grades.apptype.table)
grades.apptype.prop <- prop.table(grades.apptype.table, 1)
grades.apptype.long <- as.data.frame(as.table(grades.apptype.prop))
colnames(grades.apptype.long) <- c("Type", "Grade", "Proportion")

grades.apptype.table
grades.apptype.prop

ggplot(data = grades.apptype.long, aes(x = Type, y = Proportion, fill = Grade)) + 
  geom_bar(stat = "identity", width = .8) + 
  geom_bar(stat = "identity", width = .8, color = "black", show.legend = F, alpha = 1) + 
  labs(y = "Proportion", title = "Conditional Distribution of Loan Grades by Application Type") + 
  scale_fill_manual(name = "Grade", values = brewer.pal(5, "Reds")[1:5])

# Next, Let's look at the conditional distribution of loan grades given home ownership
loans$Home_Ownership <- factor(loans$Home_Ownership, levels = c("OWN", "MORTGAGE", "RENT", "OTHER", "NONE"))
grades.ownership.table <- table(loans$Home_Ownership, loans$Grade)
grades.ownership.matrix <- as.matrix(grades.ownership.table)
grades.ownership.prop <- prop.table(grades.ownership.table, 1)
grades.ownership.long <- as.data.frame(as.table(grades.ownership.prop))
colnames(grades.ownership.long) <- c("Ownership", "Grade", "Proportion")

grades.ownership.table
grades.ownership.prop

ggplot(data = grades.ownership.long, aes(x = Ownership, y = Proportion, fill = Grade)) + 
  geom_bar(stat = "identity", width = .8) + 
  geom_bar(stat = "identity", width = .8, color = "black", show.legend = F, alpha = 1) + 
  labs(y = "Proportion", title = "Conditional Distribution of Loan Grades by Home Ownership") + 
  scale_fill_manual(name = "Grade", values = brewer.pal(5, "Reds")[1:5])

# distribution of loan amounts
mean(loans$Loan_Amount)

ggplot(loans, aes(x=Loan_Amount, fill = Application_Type)) + 
  geom_area(stat = "bin", binwidth = 1500, color = "black")

ggplot(loans, aes(x=Loan_Amount, fill = Grade)) + 
  geom_area(stat = "bin", binwidth = 1500, color = "black") +
  scale_fill_manual(name = "Grade", values = brewer.pal(5, "Reds")[1:5])


a.mean <- mean(loans[which(loans$Grade == "A"),]$Loan_Amount)
b.mean <- mean(loans[which(loans$Grade == "B"),]$Loan_Amount)
c.mean <- mean(loans[which(loans$Grade == "C"),]$Loan_Amount)
d.mean <- mean(loans[which(loans$Grade == "D"),]$Loan_Amount)
etog.mean <- mean(loans[which(loans$Grade == "E-G"),]$Loan_Amount)


# average loan amount by grade
mean.loans.df <- data.frame(a.mean, b.mean, c.mean, d.mean, etog.mean)
rownames(mean.loans.df) <- c("Average Loan Amount ($)")
colnames(mean.loans.df) <- c("A", "B", "C", "D", "E-F")

mean.loans.df



### Initial Cumulative Logit Model ###
fit <- vglm(Grade~., family = cumulative(parallel = TRUE), data = loans.sample)
fit.summary <- summary(fit)
fit.summary

fit2 <- vglm(Grade ~.-Home_Ownership, family = cumulative(parallel = TRUE), data = loans.sample)
fit2.summary <- summary(fit2)
fit2.summary

### Variable and Model Selection ###
anova(fit)
anova(fit2)
AIC(fit)
AIC(fit2)
AIC(fit) - AIC(fit2)
# the AIC for the first model has a lower value than that for the second model (without home ownership)
# all variables are significant according to the anova test for the first fit


### Final Cumulative Logit Model ###
fit
fit.summary


### Model Visualization and Interpretation ###



# due to the complexity of this model, I will hold the home ownership type constant in order to graph the probabilities
# category that home ownership will be held constant at: Mortgage (because it is the most common in the dataset)
dti.seq <- seq(0, 35, length.out = 100) # if too big, make it 3
income.seq <- mean(loans$Annual_Income)
amount.seq <- mean(loans$Loan_Amount)
accounts.seq <- mean(loans$Mortgage_Accounts)
app.type1 <- "Individual"
app.type0 <- "Joint App"
house.type <- "MORTGAGE"


expand.grid.type1 <- expand.grid(income.seq, app.type1, dti.seq, house.type, amount.seq, accounts.seq)
colnames(expand.grid.type1) <- c("Annual_Income", "Application_Type", "Debt_to_Income", "Home_Ownership", "Loan_Amount", "Mortgage_Accounts")
expand.grid.type1

expand.grid.type0 <- expand.grid(income.seq, app.type0, dti.seq, house.type, amount.seq, accounts.seq)
colnames(expand.grid.type0) <- c("Annual_Income", "Application_Type", "Debt_to_Income", "Home_Ownership", "Loan_Amount", "Mortgage_Accounts")
expand.grid.type0

# graphs for cumuative probabilities
fit.logit.indv <- predict(fit, newdata = expand.grid.type1, untransform = TRUE)
fit.logit.joint <- predict(fit, newdata = expand.grid.type0, untransform = TRUE)

expanded.predicted1 <- cbind(expand.grid.type1, fit.logit.indv)
expanded.predicted0 <-cbind(expand.grid.type0, fit.logit.joint)

predicted.indv <- pivot_longer(expanded.predicted1, cols = c("P[Y<=1]", "P[Y<=2]", "P[Y<=3]", "P[Y<=4]"), names_to = "Type", values_to = "cumProb")
predicted.joint <- pivot_longer(expanded.predicted0, cols = c("P[Y<=1]", "P[Y<=2]", "P[Y<=3]", "P[Y<=4]"), names_to = "Type", values_to = "cumProb")

# make ggplot but make sure to add colors by P[] or the category for regular probabilities... also make  sure to have two graphs separated by app type
ggplot(predicted.indv, aes(x = Debt_to_Income, y = cumProb, color = Type)) + geom_line() + labs(x = "Debt to Income Ratio", y = "Cumulative Probability", title = "Cummulative Probabilties for Individual Applications")
ggplot(predicted.joint, aes(x = Debt_to_Income, y = cumProb, color = Type)) + geom_line() + labs(x = "Debt to Income Ratio", y = "Cumulative Probability", title = "Cummulative Probabilties for Joint Applications")

# graphs for probabiltiies
fit.probs.indv <- predict(fit, newdata = expand.grid.type1, type = "response")
fit.probs.joint <- predict(fit, newdata = expand.grid.type0, type = "response")

expanded.predicted1.probs <- cbind(expand.grid.type1, fit.probs.indv)
expanded.predicted0.probs <-cbind(expand.grid.type0, fit.probs.joint)
expanded.predicted0.probs
predicted.probs.indv <- pivot_longer(expanded.predicted1.probs, cols = c("A", "B", "C", "D", "E-G"), names_to = "Grades", values_to = "Probability")
predicted.probs.joint <- pivot_longer(expanded.predicted0.probs, cols = c("A", "B", "C", "D", "E-G"), names_to = "Grades", values_to = "Probability")

# make ggplot but make sure to add colors by P[] or the category for regular probabilities... also make  sure to have two graphs separated by app type
ggplot(predicted.probs.indv, aes(x = Debt_to_Income, y = Probability, color = Grades)) + geom_line() + labs(x = "Debt to Income Ratio", y = "Probability", title = "Probabilties for Individual Applications")
ggplot(predicted.probs.joint, aes(x = Debt_to_Income, y = Probability, color = Grades)) + geom_line() + labs(x = "Debt to Income Ratio", y = "Probability", title = "Probabilties for Joint Applications")
 
### Other ###


