## Project Analysis ##

# Set WD for project
repoLoc <- ("/Repositories/STAT6021_project")
setwd(repoLoc)

# load libraries
library(car)
library(perturb)

#load in training data
train.data <- read.csv("traindata_subset.csv", header = TRUE)
train.data <- train.data[,c(3:23)]

# Look at full model
lm.train.full <- lm(winPlacePerc ~., data = train.data)

summary(lm.train.full)

### Evaluate Multi-collinearities in the model

# After looking at initial VIF numbers, we see
# high correlation between kills and damage dealt
# Since Kills is likely an indicator of the damage dealt
# We removed that from our model

train.data.int <- train.data[,c(1:2,4:10, 12:21)]
lm.train.data.int <- lm(winPlacePerc ~., data = train.data.int)

cor(train.data.int[,1:19]) 
vif(lm.train.data.int)

# Residual analysis

train.data.resid <- train.data[,c(1:2,4:21)]
lm.train.resid <- lm(winPlacePerc ~., data = train.data.resid)

# -------------------------------------------------------+
#####          Residual-by-regressor plots               |
# -------------------------------------------------------+

rstudent_resid <- rstudent(lm.train.resid)

plot(train.data.resid$weaponsAcquired, rstudent_resid, pch=16, cex=1, xlab="x_var", ylab="R-student residual", main = "Residual-by-regressor plot")
abline(h=0, lty=1, lwd=3)

# -------------------------------------------------------+
#              Partial regression plots                  |
# -------------------------------------------------------+

y.x2 <- resid(lm(time ~ distance, data=delivery.data))
x1.x2 <- resid(lm(cases ~ distance, data=delivery.data))

y.x1 <- resid(lm(time ~ cases, data=delivery.data))
x2.x1 <- resid(lm(distance ~ cases, data=delivery.data))

par(mfrow = c(2, 2), mai=c(0.7,0.7,0.2,0.1)) # mai=c(bottom, left, top, right)
plot(delivery.data$cases, delivery.data$time, pch=16, cex=1, xlab="cases", ylab="time", main = "raw data")
plot(delivery.data$distance, delivery.data$time, pch=16, cex=1, xlab="distance", ylab="time", main = "raw data")
plot(x1.x2, y.x2, pch=16, cex=1, xlab="cases", ylab="time", main = "partial regression plot")
plot(x2.x1, y.x1, pch=16, cex=1, xlab="distance", ylab="time", main = "partial regression plot")
par(mfrow = c(1, 1))
