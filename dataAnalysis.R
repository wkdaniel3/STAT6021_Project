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
#####   Here is where we looked for irregularities in 
#####   The individual regressors
# -------------------------------------------------------+

rstudent_resid <- rstudent(lm.train.resid)

plot(train.data.resid$matchType, rstudent_resid, pch=16, cex=1, xlab="x_var", ylab="R-student residual", main = "Residual-by-regressor plot")
abline(h=0, lty=1, lwd=3)

plot(train.data.resid$revives, train.data.resid$winPlacePerc, pch=16, cex=1, xlab="revives", ylab="WinPlacePerc", main = "raw data")

summary(lm(train.data.resid$winPlacePerc~train.data.resid$revives, data=train.data.resid))


# -------------------------------------------------------+
#              Box-Tidwell Processing                    |
### Once we found irregularities above, we used BT
### To try to figure out the transformation
# -------------------------------------------------------+

# Create subset train data set that only has the variables we want
train.subset <- train.data.resid %>% select(-heals, -rideDistance, -swimDistance, -vehicleDestroys, -maxPlace)

# Subset that data set to prepare for box-tidwell function (can not have regressor values that are 0 or negative)
train.boxTid <- train.subset %>% select(roadKills, winPlacePerc) %>% 
                                 filter(roadKills > 0)

# Run box-tidwell and review results
boxTidwell <- boxTidwell(winPlacePerc ~ roadKills, data=train.boxTid)
boxTidwell

# Transform the regressor using the lambda found in box-tidwell
train.subset <- train.subset %>% mutate(killPlace = killPlace^2,
                                        walkDistance = walkDistance^0.5) %>% 
                                 select(-revives, -teamKills, -roadKills, -DBNOs, -headshotKills)

# Re-run linear model and review data
lm.train.new <- lm(winPlacePerc ~., data = train.subset)
summary(lm.train.new)

# Review the new Residual-by-regressor plot
rstudent_resid_new <- rstudent(lm.train.new)
plot(train.subset$teamKills, rstudent_resid_new, pch=16, cex=1, xlab="x_var", ylab="R-student residual", main = "Residual-by-regressor plot")
abline(h=0, lty=1, lwd=3)

# Comments on linearizing data

# Walk distance appeared to be non-linear in our Residual-by-Regressor analysis. Running Box-Tidwell got a lambda of .38881
# To simplify, we used 0.5 and updated the data. Upon re-running a Residual-by-Regressor plot, 
# We saw a much better band

# Team Kills appeared to be non-linear in our Residual-by-Regressor analysis. Upon further Analysis, 
# we are seeing a ton of 0's in the data and a few outliers. Because we see mostly uniformity with a number 
# of outliers, we are going to exclude it from our model for now. 

# -------------------------------------------------------+
#              Normal Prob Plot                    |
### Once we found irregularities above, we used BT
### To try to figure out the transformation
# -------------------------------------------------------+

qqnorm(rstudent_resid_new, datax=TRUE, pch=16, cex=1, xlab="percent", ylab="R-student residual", main = "Normal probability plot of residuals")
qqline(rstudent_resid_new)

# -------------------------------------------------------+
# Residual-by-fitted-value plot             |
# -------------------------------------------------------+

y.hat <- fitted(lm.train.new)

plot(y.hat, rstudent_resid_new, pch=16, cex=1, xlab="fitted value", ylab="R-student residual", main = "Residual-by-fitted-value plot")
abline(h=0, lty=1, lwd=3)

# -------------------------------------------------------+
# Review outliers and remove them for model             |
# -------------------------------------------------------+

game.inf <- dffits(lm.train.new)

n <- nrow(train.subset)
p <- length(coefficients(lm.train.new))
cut.inf <- 2*sqrt(p/n)

game.dffits<-as.data.frame(cbind(game.inf,cut.inf))
names(game.dffits)<-c("game","cut")
outlier.rows<-as.data.frame(which(game.dffits$game > game.dffits$cut))
names(outlier.rows)<-c("rownum")

reduced.trainset<-train.subset[-(outlier.rows$rownum),]
reduced.game.lm<-lm(winPlacePerc~.,data=reduced.trainset)

y.hat <- fitted(reduced.game.lm)
rstudent_resid_new <- rstudent(reduced.game.lm)

plot(y.hat, rstudent_resid_new, pch=16, cex=1, xlab="fitted value", ylab="R-student residual", main = "Residual-by-fitted-value plot")
abline(h=0, lty=1, lwd=3)

summary(reduced.game.lm)

#Here is the model & data we are currently working with
####   reduced.trainset
####   reduced.game.lm