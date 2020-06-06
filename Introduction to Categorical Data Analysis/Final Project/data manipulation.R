# Data Manipulation for Categorical Data Analysis Project Loan Data

# First, let's load in the giant dataset
library(readr)
lc.loans <- read.csv("C:\\Users\\joshi\\Documents\\CDA Final Project Loan Data\\loan.csv")

# Ok, so at this point I'm not too worried about missing data, but I want to remove specific columns because they are 
# irrelevent, uninteresting, or too complex (e.g. laon description, job, zip code, etc.) and they make the dataset much larger than need be

# Let's look at all of the first few rows and all of the variables in the dataset, as well as the number of rows and the number of columns
head(lc.loans)
ls(lc.loans)
nrow(lc.loans)
ncol(lc.loans)

lc.loans.mutated <- data.frame(lc.loans$grade, lc.loans$annual_inc, 
                               lc.loans$application_type, lc.loans$avg_cur_bal, lc.loans$dti,
                               lc.loans$earliest_cr_line, lc.loans$emp_length, lc.loans$home_ownership, lc.loans$loan_amnt,
                               lc.loans$mort_acc)
head(lc.loans.mutated)

sum(is.na(lc.loans.mutated))
mean(is.na(lc.loans.mutated))

lc.loans.mutated2 <- na.omit(lc.loans.mutated)
sum(is.na(lc.loans.mutated2))
mean(is.na(lc.loans.mutated2))
# removed all na values (.5% NA values beforehand)

colnames(lc.loans.mutated2) <- c("Grade", "Annual Income", "Application Type", "Average Current Balance", "Debt to Income", "Earliest Credit Line", "Employment Length",
                                                  "Home Ownership", "Loan Amount", "Mortgage Accounts")
head(lc.loans.mutated2)

sum(is.na(lc.loans.mutated2))
# no missing data