## Project Analysis ##

# Set WD for project
repoLoc <- ("/Repositories/STAT6021_project")
setwd(repoLoc)

# load libraries
library(car)
library(perturb)
#library(MASS)
library(tidyverse)

####### PART 1 - LOAD IN TRAINING SET AND REVIEW SIMPLE LINEAR REGRESSION MODEL ----

#load in training data
train.data <- read.csv("traindata_subset.csv", header = TRUE)
train.data <- train.data[,c(3:23)]

# Look at full model
lm.train.full <- lm(winPlacePerc ~., data = train.data)

summary(lm.train.full)

####### PART 2 - REVIEW CORRELATED REGRESSORS AND MULTI-COLLINEARITY ----

### Evaluate Multi-collinearities in the model

# After looking at initial VIF numbers, we see
# high correlation between kills and damage dealt
# Since Kills is likely an indicator of the damage dealt
# We removed that from our model

train.data.int <- train.data[,c(1:7,9:10, 12:21)]
lm.train.data.int <- lm(winPlacePerc ~., data = train.data.int)

cor(train.data.int[,1:19]) 
vif(lm.train.data.int)

# Remove kills in favor of damage dealt
train.data <- train.data[,c(1:7,9:21)]

#### PART 3 - EVALUATE IRREGULARITIES IN INDIVIDUAL REGRESSORS ----

# Create subset train data set that only has the variables we want
train.subset <- train.data %>% mutate(top10finish = ifelse(winPlacePerc >= 0.90,1,0)) %>% 
                                 select(-winPlacePerc, -heals, -rideDistance, -swimDistance, -vehicleDestroys, -maxPlace)

############################ Below is in progress ###############################################################

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

# Team Kills, Road Kills, headShotKills
# Grouping these columns and plotting them against the percentage of top 10 finishes, we see a negative correlation. 
# However, the data is very heavily skewed to the left, so this information is likely unreliable. As a result, 
# We will throw it out

# Revives, DBNOs
# We do see some sort of quadratic relationship between Revives and finishing in the top 10
# However, we see the same skewness in this column

# Also removing longest kill as it is said to be misleading
# Match duration & Match Type, since that won't indicate whether you finished in top 10


# -------------------------------------------------------+
#              Box-Tidwell Processing                    |
### Once we found irregularities above, we used BT
### To try to figure out the transformation
# -------------------------------------------------------+



# Subset that data set to prepare for box-tidwell function (can not have regressor values that are 0 or negative)
train.boxTid <- train.test %>% dplyr::select(headshotKills, top10finish) 

# Run box-tidwell and review results
boxTidwell <- boxTidwell(winPlacePerc ~ DBNOs, data=train.boxTid)
boxTidwell

# Update 0's to 0.0001 for the needed data points
train.subset <- train.subset %>% mutate(revives = ifelse(revives == 0, 0.0001, revives),
                                        teamKills = ifelse(teamKills == 0, 0.0001, teamKills),
                                        DBNOs = ifelse(DBNOs == 0, 0.0001, DBNOs),
                                        headshotKills = ifelse(headshotKills == 0, 0.0001, headshotKills))

# Transform the regressor using the lambda found in box-tidwell
train.subset <- train.subset %>% mutate(killPlace = killPlace^2,
                                        walkDistance = walkDistance^0.5,
                                        revives = revives^0.5,
                                        teamKills = teamKills^-2,
                                        headshotKills = headshotKills^-0.25) %>% 
                                 select(-roadKills)

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

# Subset that data set to prepare for box-tidwell function (can not have regressor values that are 0 or negative)
train.boxTid <- train.test %>% dplyr::select(revives, logtrans) %>% mutate(revives = ifelse(revives == 0, 0.0001, revives))

# Run box-tidwell and review results
boxTidwell <- boxTidwell(logtrans ~ revives, data=train.boxTid)
boxTidwell


# Revives
train.test <- train.subset %>% group_by(revives) %>% summarise(top10count = sum(top10perc), n = n(), final = ifelse(sum(top10perc) == 0, 0.0001, ifelse(sum(top10perc) == 1, 0.9999, sum(top10perc)/n())))
train.test <- train.test %>% mutate(logtrans = log(final/(1-final)))

plot(train.test$revives, train.test$logtrans, pch=16, cex=1, xlab="revives", ylab="Percentage in top 10", main = "")
abline(h=0, lty=1, lwd=3)

# DBNOs
train.test <- train.subset %>% group_by(boosts) %>% summarise(top10count = sum(top10perc), n = n(), final = sum(top10perc)/n())
train.test <- train.test %>% mutate(logtrans = log(final/(1-final)))

plot(train.test$DBNOs, train.test$logtrans, pch=16, cex=1, xlab="revives", ylab="Percentage in top 10", main = "")
abline(h=0, lty=1, lwd=3)

##################################################################################################################

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

#### PART 4 - REVIEW OUTLIERS - IGNORE FOR NOW ----

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

#### PART 5 - EVALUATE MODELS ----

# Pair down the data set
train.subset.complex <- train.subset %>% select(-roadKills, -teamKills, -headshotKills, -matchDuration, -matchType, -longestKill)
train.subset.middle <- train.subset %>% select(-roadKills, -revives, -teamKills, -headshotKills, -DBNOs, -matchDuration, -matchType, -longestKill)
train.subset.simple <- train.subset %>% select(assists, damageDealt, killPlace,top10finish)

# Models: Complex model (8 vars, with Revives & DBNOs), Middle model (6 vars, no transforms), Simple model(Assists, damageDealt, killPlace)
glm.complex <- glm(top10finish ~., family=binomial, data = train.subset.complex)
glm.middle <- glm(top10finish ~., family=binomial, data = train.subset.middle)
glm.simple <- glm(top10finish ~., family=binomial, data = train.subset.simple)