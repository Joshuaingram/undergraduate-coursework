# Income dataset can be found here:
# http://faculty.marshall.usc.edu/gareth-james/ISL/data.html

Income1 <- read.csv("~/Downloads/Income1.csv")
Income1

head(Income1)
plot(Income ~ Education, 
     data=Income1,
     col="red",
     pch=16)


## Unfortunately the book doesn't specify the formula
## for true curve, hence I had to use polynomial regression to try and estimate it.

lm.obj <- lm(Income ~ poly(Education,3), 
             data=Income1)
lines(x=Income1$Education, y=predict(lm.obj),
      col="blue",
      lwd=2)


## 2D surface plot for the bi-variate scenario

Income2 <- read.csv("~/Downloads/Income2.csv")
Income2
attach(Income2)

# Fitting linear model
lm.obj <- lm(Income ~ Education + Seniority, 
             data=Income2)

# Setting up the coordinates of the surface
x <- seq(min(Education), max(Education), length=nrow(Income1))
y <- seq(min(Seniority), max(Seniority), length=nrow(Income2))

# Calculates z = f(x,y) for a 2D grid of (x,y) values
z <- outer(x, y,
           function(Education,Seniority) predict(lm.obj, data.frame(Education,Seniority)))

# Plotting the surface
p <- persp(x,y,z, 
           theta=30, 
           phi=30, 
           col="yellow", 
           expand = 0.5, 
           xlab="Education", ylab="Seniority", zlab="Income")

# Putting the data points "into perspective"
obs <- trans3d(Income2$Education, Income2$Seniority,Income2$Income,p)
points(obs, col="red",pch=16)

# Putting the connecting the data points with predictions via vertical lines
pred <- trans3d(Income2$Education, Income2$Seniority,predict(lm.obj),p)
segments(obs$x, obs$y, pred$x, pred$y)
