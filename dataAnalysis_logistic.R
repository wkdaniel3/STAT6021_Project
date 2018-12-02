## Project Analysis - GLM Logistic Version #

# -------------------------------------------------------+
# GLM Logistic Model Version             |
# -------------------------------------------------------+

lm.train.new <- glm(winPlacePerc ~., family=binomial(link='logit'), data = train.subset)
summary(lm.train.new)

# Review the new Residual-by-regressor plot
rstudent_resid_new <- rstudent(lm.train.new)
plot(train.subset$teamKills, rstudent_resid_new, pch=16, cex=1, xlab="x_var", ylab="R-student residual", main = "Residual-by-regressor plot")
abline(h=0, lty=1, lwd=3)

y.hat <- fitted(lm.train.new)

plot(y.hat, rstudent_resid_new, pch=16, cex=1, xlab="fitted value", ylab="R-student residual", main = "Residual-by-fitted-value plot")
abline(h=0, lty=1, lwd=3)