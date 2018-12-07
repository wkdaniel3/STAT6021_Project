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

# DBNOs - looking at regressor vs. response plots to detect non-linearities
train.test <- train.subset %>% mutate(killPlace = killPlace,
                                      DBNOs = DBNOs^-0.5)
train.test <- train.test %>% group_by(DBNOs) %>% summarise(top10count = sum(top10finish), n = n(), final = ifelse(sum(top10finish) == 0, 0.0001, sum(top10finish)/n()))
train.test <- train.test %>% mutate(logtrans = log(final/(1-final)))

plot(train.test$DBNOs, train.test$logtrans, pch=16, cex=1, xlab="weaponsAcquired", ylab="Y var", main = "")

# Subset that data set to prepare for box-tidwell function (can not have regressor values that are 0 or negative)
train.boxTid <- train.test %>% dplyr::select(killPlace, logtrans) %>% mutate(killPlace = ifelse(killPlace == 0, 0.0001, killPlace))

# Run box-tidwell to see if we get better transforms than when we just view the regressor vs. response plots
boxTidwell <- boxTidwell(logtrans ~ killPlace, data=train.boxTid)
boxTidwell

##################################################################################################################

#### PART 4 - REVIEWing OUTLIERS -----

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
train.subset.complex <- train.subset %>% select(-roadKills, -teamKills, -headshotKills, -matchDuration, -matchType, -longestKill, -kills)
train.subset.middle <- train.subset %>% select(-roadKills, -revives, -teamKills, -headshotKills, -DBNOs, -matchDuration, -matchType, -longestKill, -kills)
train.subset.simple <- train.subset %>% select(assists, damageDealt, killPlace,top10finish)

# Models: Complex model (8 vars, with Revives & DBNOs), Middle model (6 vars, no transforms), Simple model(Assists, damageDealt, killPlace)
glm.complex <- glm(top10finish ~., family=binomial, data = train.subset.complex)
glm.middle <- glm(top10finish ~., family=binomial, data = train.subset.middle)
glm.simple <- glm(top10finish ~., family=binomial, data = train.subset.simple)

#### PART 6 - PREDICTIVE MODELS ----

# Create a test data set with 1M rows
test.data <- read.csv("trainset.csv", header = TRUE)

# Remove the index column
test.data <- test.data[2000001:3000000,-1]

# Create a column for top10finish
test.data <- test.data %>% mutate(top10finish = ifelse(winPlacePerc >= 0.90,1,0)) 

# Calculate the hit-rate for the complex model 
complex.fitted.results <- predict(glm.complex,newdata=subset(test.data,select=c('assists','boosts','damageDealt','DBNOs','killPlace','revives','walkDistance','weaponsAcquired')),type='response')
complex.fitted.results_binary <- round(complex.fitted.results)
misClasificError <- mean(complex.fitted.results_binary != test.data$top10finish)
print(paste('Accuracy',1-misClasificError)) #Accuracy 0.248073, "Accuracy 0.917197"

# Calculate the hit-rate for the middle model 
middle.fitted.results <- predict(glm.middle,newdata=subset(test.data,select=c('assists','boosts','damageDealt','killPlace','walkDistance','weaponsAcquired')),type='response')
middle.fitted.results_binary <- round(middle.fitted.results)
misClasificError <- mean(middle.fitted.results_binary != test.data$top10finish)
print(paste('Accuracy',1-misClasificError)) #Accuracy 0.249744

# Calculate the hit-rate for the simple model 
simple.fitted.results <- predict(glm.simple,newdata=subset(test.data,select=c('assists','damageDealt','killPlace'),type='response'))
simple.fitted.results_binary <- round(simple.fitted.results)
misClasificError <- mean(simple.fitted.results_binary != test.data$top10finish)
print(paste('Accuracy',1-misClasificError)) #Accuracy 0.044643