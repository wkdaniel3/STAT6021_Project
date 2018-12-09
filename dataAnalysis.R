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

full.data <- read.csv("trainset.csv", header = TRUE)

#Remove outliers
full.data$'cheaters' <- (full.data$'kills' > 0) & (full.data$'walkDistance' == 0)
sum(full.data$'cheaters') #1549
full.data <- full.data[full.data$'cheaters' == FALSE,]

#Create training subset
train.data <- full.data[1:1000000,-1]
drops <- c('X','cheaters')
train.data <- train.data[ , !(names(train.data) %in% drops)]

# Look at full model
lm.train <- lm(winPlacePerc ~., data = train.data)
summary(lm.train)
#RSQ .8158
anova(lm.train)

# Review our response data
response.analysis <- train.data %>% group_by(winPlacePerc) %>% summarize(count = n()) %>% select(winPlacePerc, count)
plot(response.analysis$winPlacePerc, response.analysis$count, pch=16, cex=1, xlab="WinPlacePerc", ylab="Count", main = "Win placement percentage vs. Data Count")

####### PART 2 - REVIEW CORRELATED REGRESSORS AND MULTI-COLLINEARITY ----

### Evaluate Multi-collinearities in the model

# After looking at initial VIF numbers, we see
# high correlation between kills and damage dealt
# Since Kills is likely an indicator of the damage dealt
# We removed that from our model

kills <- train.data$kills
damageDealt <- train.data$damageDealt
matchType <- train.data$matchType
winPlacePerc <- train.data$winPlacePerc
drops <- c("damageDealt", "matchType")
train.data.int <- train.data[ , !(names(train.data) %in% drops)]
lm.train.data.int <- lm(winPlacePerc ~., data = train.data.int)
summary(lm.train.data.int)

cor(train.data.int[,1:18]) 
vif(lm.train.data.int)

#### PART 3 - EVALUATE IRREGULARITIES IN INDIVIDUAL REGRESSORS ----

# Reviewing initial linear model and residuals associated with model
rstudent_resid_linear <- rstudent(lm.train)
linear.fitted <- fitted.values(lm.train)

plot(linear.fitted, rstudent_resid_linear, pch=16, cex=1, xlab="Fitted Values", ylab="R-student residual", main = "Residual-by-fitted value plot")
abline(h=0, lty=1, lwd=3)

# Create subset train data set that only has the variables we want
train.subset <- train.data %>% mutate(top10finish = ifelse(winPlacePerc >= 0.90,1,0)) %>% select(-winPlacePerc, -heals, -rideDistance, -swimDistance, -vehicleDestroys, -maxPlace)

# -------------------------------------------------------+
#####          Response-by-regressor plots               |
#####   Here is where we looked for irregularities in 
#####   The individual regressors
# -------------------------------------------------------+

# revives - One example
train.test <- train.subset %>% mutate(boosts = boosts)
train.test <- train.test %>% group_by(boosts) %>% summarise(top10count = sum(top10finish), n = n(), final = ifelse(sum(top10finish) == 0, 0.0001, ifelse(sum(top10finish)/n()==1, 0.999, sum(top10finish)/n())))
train.test <- train.test %>% mutate(logtrans = log(final/(1-final)))

plot(train.test$boosts, train.test$logtrans, pch=16, cex=1, xlab="Boosts", ylab="Transformed Y", main = "Response-by-Regressor")

# -------------------------------------------------------+
#####          Residual-by-regressor plots               |
# -------------------------------------------------------+

lm.residual <- lm(logtrans~boosts, data = train.test)

rstudent_resid <- rstudent(lm.residual)

plot(train.test$logtrans, rstudent_resid, pch=16, cex=1, xlab="boosts", ylab="R-student residual", main = "Residual-by-regressor plot")
abline(h=0, lty=1, lwd=3)

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
train.boxTid <- train.test %>% dplyr::select(boosts, logtrans) 

# Run box-tidwell and review results
boxTidwell <- boxTidwell(logtrans ~ boosts, data=train.boxTid)
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
train.subset.lm <- train.data %>% select(-roadKills, -vehicleDestroys)
train.subset.complex <- train.subset %>% select(-roadKills, -teamKills, -headshotKills, -matchDuration, -matchType, -longestKill, -kills)
train.subset.middle <- train.subset %>% select(-roadKills, -revives, -teamKills, -headshotKills, -DBNOs, -matchDuration, -matchType, -longestKill, -kills)
train.subset.simple <- train.subset %>% select(assists, damageDealt, killPlace,top10finish)

# Models: Complex model (8 vars, with Revives & DBNOs), Middle model (6 vars, no transforms), Simple model(Assists, damageDealt, killPlace)
lm.complex <- lm(train.data$winPlacePerc ~., data = train.data)
glm.complex <- glm(top10finish ~., family=binomial, data = train.subset.complex)
glm.middle <- glm(top10finish ~., family=binomial, data = train.subset.middle)
glm.simple <- glm(top10finish ~., family=binomial, data = train.subset.simple)

summary(glm.complex)
summary(glm.middle)
summary(glm.simple)

#### PART 6 - PREDICTIVE MODELS ----

# Create a test data set with 1M rows
test.data <- read.csv("trainset.csv", header = TRUE)

# Remove the index column
test.data <- test.data[2000001:3000000,-1]

# Create a column for top10finish
test.data <- test.data %>% mutate(top10finish = ifelse(winPlacePerc >= 0.90,1,0))
test.data.2 <- test.data %>% mutate(chickendinner.test = ifelse(winPlacePerc == 1,1,0)) 

# MAE for full multiple linear regression
lm.fitted.results <- predict(lm.train,newdata=subset(test.data,select=c('assists','boosts','kills', 'damageDealt','DBNOs','killPlace','revives','walkDistance','weaponsAcquired','heals','longestKill','headshotKills','matchDuration','maxPlace','rideDistance','roadKills','swimDistance','teamKills','vehicleDestroys','matchType')),type='response')
MeanAbsError <- mean(abs(test.data$winPlacePerc-lm.fitted.results))
print(paste('Error',MeanAbsError)) #"Error 0.12357484382543" "0.0979514835272643"

# MAE testing
lm.fitted.results <- predict(lm.complex,newdata=subset(test.data,select=c('assists','boosts','kills','revives','walkDistance','swimDistance','rideDistance','weaponsAcquired','longestKill','heals','DBNOs','killPlace','matchDuration','teamKills','matchType','headshotKills','damageDealt','maxPlace','roadKills','vehicleDestroys')),type='response')
MeanAbsError <- mean(abs(test.data$winPlacePerc-lm.fitted.results))
print(paste('Error',MeanAbsError))

#Hit Rate for original full model
lm.fitted.results <- predict(lm.train, newdata=subset(test.data,select=c('assists','boosts','kills', 'damageDealt','DBNOs','killPlace','revives','walkDistance','weaponsAcquired','heals','longestKill','headshotKills','matchDuration','maxPlace','rideDistance','roadKills','swimDistance','teamKills','vehicleDestroys','matchType')), interval="prediction", level=0.95)
lowerbound <- lm.fitted.results[,2] 
upperbound <- lm.fitted.results[,3] 
withinbound <- data.frame(cbind(lowerbound<test.data$winPlacePerc,test.data$winPlacePerc<upperbound))
sum(withinbound$X1 == TRUE & withinbound$X2==TRUE)/1000000
#full model: 94.4% hit rate within prediction interval

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