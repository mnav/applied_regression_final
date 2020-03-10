# TODO: Do we need an instrumental variable in any of these?!! 

# WASHINGTON STATE POLICE TRAFFIC STOPS
df <- read.csv('WSPTrafficStops.csv')
head(df)
attach(df)
boxplot(Stops ~ Race)
# It seems white people get stopped the most
boxplot(Radar/Stops ~ Race)
# If you look at proportion of radar traps to stops, this trend disappears

# 1. Fit model
bad.model <- lm(Stops ~ Radar)
summary(bad.model)
# Estimate  Std. Error  t value      Pr(>|t|)
# (Intercept) 126.512694 97.15009395  1.30224  1.943174e-01
# Radar         1.958437  0.03840551 50.99365 2.543865e-117
# R.squared = 92.8%

# TODO: Why is this model no good? (any more reasons??)
plot(Stops ~ Radar)
# The scatterplot for this data has a trumpet shape, which means that a proper model would likely look at log(Radar) to account for high dispersion at high values of x.
# This model assumes that stops and radar are linearly correlated. They may be, but the dispersion makes it difficult to prove.

# TODO: decide if you're going to use QQ plot or not.
fit <- predict(bad.model)
qqplot(Stops, fit)
# TODO: If using QQplot, explain better what is wrong with the QQ plot
# The qqplot of Stops vs fit doesn't look right


# 2. 
plot(Stops ~ Radar) # Trumpet shape; must take log of Radar
log.radar <- log(Radar)
plot(Stops ~ log.radar) # Looks polynomial 
log.stops <- log(Stops)
plot(log.stops ~ log.radar) # Finally looks linear!

# Build a better model after log/log transformation
better.model <- lm(log.stops ~ log.radar)
summary(better.model)
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  0.72448    0.07463   9.708   <2e-16 ***
#   log.radar    1.00038    0.01353  73.913   <2e-16 ***
# R.squared = 0.964
# Seems to be an almost perfectly linear relationship between radar and stops.

fit.2 <- predict(better.model)
qqplot(log.stops, fit.2)
# TODO: Figure out a good way to say the qq-plot is better, but still wonky across the middle
# TODO: "Convince me that we have not violated any model assumptions. Are there any borderline?"


# 3. Test for racial bias in traffic stops.
# Note that we are only testing for a race dependent relationship between the  benchmark Radar counts 
# & self-initiated Stops

summary(lm(log.stops ~ Race))

summary(lm(log.stops ~ log.radar*Race - Race - log.radar))

