## Joshua Ingram

## Generate a 100 values for explanatory variable x, 
## uniformly distributed from -50 to 50.
set.seed(1)
x <- runif(100, min = -50, max = 50)
x

## Generate y from the model
#        y = 3 + 2*x + eps, eps ~ N(0, sigma^2),
## where sigma=40.
eps <- rnorm(100, 0, 40)
hist(eps)

y <- 3 + (2 * x) + eps

## Fit least squares regression y ~ x, plot the resulting fit.
lm_fit <- lm(y ~ x)
summary(lm_fit)

plot(y ~ x)
abline(lm_fit)


## Conduct 1,000 simulations of the following:
##      1. Generate y from the model:
##            y = 3 + 2*x + eps, eps ~ N(0, sigma^2),
##        where sigma = 40.
##      2. Calculate least squares estimates for y ~ x regression,
##        record the beta.hat estimate (KEEP TRACK of them).
##      3. Add the fitted line to existing plot.
##      4. Calculate the confidence interval for beta (KEEP TRACK of them).

beta_hats <- numeric(1000)
beta_ci <- matrix(0, nrow = 1000, ncol = 2)


for (i in 1:1000){
  
  eps <- rnorm(100, 0, 40)
  
  y <- 3 + (2 * x) + eps
  
  ## Fit least squares regression y ~ x, plot the resulting fit.
  lm_fit <- lm(y ~ x)
  
  #plot(y ~ x)
  abline(lm_fit)
  
  beta_hats[i] <- lm_fit$coefficients[2]
  
  beta_ci[i,] <- confint(lm_fit)[2,]
}


## Overlay a thick red population regression line over.

abline(3, 2, lw = 3, col = "red")

## Next, proceed to compare:
##    1. Practical and theoretical expected value of beta^hat (check unbiasedness).
##       (In Markdown report, make sure to write down the theoretical 'formula' for E[beta^hat])

# Practical:

mean(beta_hats)

# Theoretical:

# E[beta^hat] = beta = 2

# These two are nearly the same

##    2. Practical and theoretical sampling variance of beta^hat.
##       (In Markdown report, make sure to write down the theoretical formula for V[beta^hat])

# Practical:
var(beta_hats) 

# Theoretical:
# V[beta^hat] = sigma^2/sum(x_i - x^bar)^2 =
theor.var <- 1600/sum((x - mean(x))^2)

##    3. Practical and theoretical sampling distribution of beta^hat.
##       (In Markdown report, make sure to write down the theoretical formula for V[beta^hat])

# Practical:
hist(beta_hats, freq = F)

# Theoretical:
# beta^hat ~ N(beta, V[beta^hat])
# this would look very similar to the histogram outputted from above except with the center at 2 and a variance of .2257158
# make a normal density function over the beta_hats histogram
my.dnorm <- function(z) dnorm(z, 2, sqrt(theor.var))
curve(my.dnorm,
      from = 1.4, to = 2.6,
      add = T, col = "red")


##    4. Practical and theoretical coverage of 95% confidence intervals.

# Practical:
mean(beta_ci[,1] < 2 & beta_ci[,2] > 2)


# Theoretical:
# .95

# these are almost exactly the same