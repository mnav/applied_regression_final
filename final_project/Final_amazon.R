########################################################################
########################################################################
########################################################################
# AMAZON

amazon <- read.csv('amazon.csv')
attach(amazon)
head(amazon)

boxplot(bought ~ connection) # Looks ugly
plot(bought) # Looks ugly
log.bought <- log(bought)
plot(log.bought) # Looks a little more reasonable (note: we can have negative values because this is LOG, not raw number, of bought items)
boxplot(log.bought ~ connection) # see very small uplift in purchases if customer has DSL. However, many more outliers on that as well.

# Let's look for other factors that impact purchasing behavior
# TODO: Don't just eyeball this shit. Find statistical relevance as well
boxplot(log.bought ~ region) # no real difference
boxplot(log.bought ~ size) # no real difference
boxplot(log.bought ~ oldest) # hard to say, visually no difference
boxplot(log.bought ~ income) # doesn't appear to be MUCH difference. But likely is...
boxplot(log.bought ~ children) # no apparent difference
boxplot(log.bought ~ race) # slight difference across races (many outliers in race == 1)
boxplot(log.bought ~ hispanic) # sliiiight difference if hispanic 
# TODO: What other methods of teasing out statistical relevance can you use?

# Build a model to see what's what
am.reg <- lm(log.bought ~ region + size + oldest + income + children + race + hispanic)
summary(am.reg)
# TODO: include interaction variables (if they make sense)

# From the looks of it, only a handful of variables have statistical relevance here (discounting interaction variables)
# region
# income
# race

# Build a model using only these variables:
am.reg.2 <- lm(log.bought ~ region + income + race)
summary(am.reg.2)
# Extremely low R.squared (0.5%), which makes me think there's multicollinearity at play.

# Build a model holding all these variables constant and only varying connection

# FINAL POINT:
# This introduces selection bias. We do not know the impact of internet access on amazon purchasing behavior for families that already buy from amazon. It is entirely likely that new trends about purchasing behavior (or more correctly: the decision not to purchase off amazon.com) can be elucidated by looking at the dataset of people who have never made a purchase from amazon.com
