# STAT6021_Project
PUBG Finish Placement Prediction Project

# Linear Vars
assists

boosts

kills

longestKill

matchDuration

matchType

weaponsAcquired

winPlacePerc

# Non-linear vars
killPlace - Non-linear (lambda of 2)

walkDistance - Not linear (lambda of 0.5)


# Non-linear, but don't know how to transform	
revives - suggests ~-1 lambda, but we have a lot of 0's, so they had to be excluded from the model

teamKills - suggests -2 lambda, similar to revives

roadKills - Not Linear > bombs out the box-tidwell function

DBNOs - showing a lambda of -0.27

headshotKills - -0.3
