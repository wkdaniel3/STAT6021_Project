## Project Analysis ##

# Set WD for project
repoLoc <- ("/Repositories/STAT6021_project")
setwd(repoLoc)

# load libraries
library(car)
library(perturb)
library(MASS)
library(tidyverse)

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

plot(train.data.resid$matchType, rstudent_resid, pch=16, cex=1, xlab="x_var", ylab="R-student residual", main = "Residual-by-regressor plot")
abline(h=0, lty=1, lwd=3)

plot(train.data.resid$revives, train.data.resid$winPlacePerc, pch=16, cex=1, xlab="revives", ylab="WinPlacePerc", main = "raw data")

summary(lm(train.data.resid$winPlacePerc~train.data.resid$revives, data=train.data.resid))


# -------------------------------------------------------+
#              Box-Tidwell Function                      |
# -------------------------------------------------------+

train.subset <- train.data.resid %>% select(-heals, -rideDistance, -swimDistance, -vehicleDestroys, -maxPlace)

train.boxTid <- train.subset %>% select(winPlacePerc, walkDistance) %>% 
                                 filter(walkDistance > 0)

boxTidwell <- boxTidwell(winPlacePerc ~ walkDistance, data=train.boxTid)
boxTidwell

train.subset <- train.subset %>% mutate(walkDistance = walkDistance^0.5)

lm.train.new <- lm(winPlacePerc ~., data = train.subset)

summary(lm.train.new)

rstudent_resid <- rstudent(lm.train.new)

plot(train.subset$walkDistance, rstudent_resid, pch=16, cex=1, xlab="x_var", ylab="R-student residual", main = "Residual-by-regressor plot")
abline(h=0, lty=1, lwd=3)

plot(train.data.resid$walkDistance, rstudent_resid, pch=16, cex=1, xlab="x_var", ylab="R-student residual", main = "Residual-by-regressor plot")
abline(h=0, lty=1, lwd=3)