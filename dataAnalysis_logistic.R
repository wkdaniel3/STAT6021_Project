## Project Analysis - GLM Logistic Version #

# -------------------------------------------------------+
# GLM Logistic Model Version             |
# -------------------------------------------------------+

train.subset <- train.subset %>% mutate(top10finish = ifelse(winPlacePerc >= 0.90,1,0)) %>% 
                                 select(-winPlacePerc)

# Team Kills, Road Kills, headShotKills
# Grouping these columns and plotting them against the percentage of top 10 finishes, we see a negative correlation. 
# However, the data is very heavily skewed to the left, so this information is likely unreliable. As a result, 
# We will throw it out

# Revives, DBNOs
# We do see some sort of quadratic relationship between Revives and finishing in the top 10
# However, we see the same skewness in this column

# Also removing longest kill as it is said to be misleading
# Match duration & Match Type, since that won't indicate whether you finished in top 10

############################ Below is in progress ###############################################################

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

# Pair down the data set
train.subset.complex <- train.subset %>% select(-roadKills, -teamKills, -headshotKills, -matchDuration, -matchType, -longestKill)
train.subset.middle <- train.subset %>% select(-roadKills, -revives, -teamKills, -headshotKills, -DBNOs, -matchDuration, -matchType, -longestKill)
train.subset.simple <- train.subset %>% select(assists, damageDealt, killPlace,top10finish)

# Models: Complex model (8 vars, with Revives & DBNOs), Middle model (6 vars, no transforms), Simple model(Assists, damageDealt, killPlace)
glm.complex <- glm(top10finish ~., family=binomial, data = train.complex)
glm.middle <- glm(top10finish ~., family=binomial, data = train.middle)
glm.simple <- glm(top10finish ~., family=binomial, data = train.simple)