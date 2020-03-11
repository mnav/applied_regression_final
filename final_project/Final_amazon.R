# AMAZON
# TODO: This is an IV modeling problem

amazon <- read.csv('amazon.csv')
attach(amazon)
head(amazon)

# Convert numeric variables to categorical where appropriate
c.region <- as.factor(region)
c.oldest <- as.factor(oldest)
c.income <- as.factor(income)
c.children <- as.factor(children)
c.race <- as.factor(race)
c.connection <- as.factor(connection)
c.hispanic <- ifelse(amazon$hispanic==TRUE,1,0)

# Search for an IV
xr1 <- lm(
  connection ~ 
    c.region + size + c.oldest + c.income + c.children + c.race + c.hispanic
  )

xr2 <- lm(
  connection ~ 
    c.region + c.income + c.children + c.hispanic
)

bic <- c(
  first=extractAIC(xr1, k=log(n))[2], 
  second=extractAIC(xr2, k=log(n))[2]
)
ebic <- exp(-.5*(bic - min(bic)))
round(probs <- ebic/sum(ebic), 2)
# the second one is better (100%)

is.region.2 <- ifelse(amazon$region == 2, 1, 0)
high.income <- ifelse(amazon$income >= 6, 1, 0)

xr3 <- lm(
  connection ~ 
    is.region.2 + high.income + c.hispanic
)
bic <- c(
  first=extractAIC(xr2, k=log(n))[2], 
  second=extractAIC(xr3, k=log(n))[2]
)
ebic <- exp(-.5*(bic - min(bic)))
round(probs <- ebic/sum(ebic), 2)
# Almost 100% certain xr3 is better.


plot(bought)
# Taking log to normalize the spread a littel
log.bought <- log(bought)

# MLR predicting log.bought with all variables (no interactions)
r1 <- lm(
  log.bought ~ 
    c.region + size + c.oldest + c.income + 
    c.children + c.race + c.connection + c.hispanic
  )
n = nrow(amazon)

r2 <- lm(
  log.bought ~ c.region + c.income + c.race + c.connection
)

bic <- c(
  reg1=extractAIC(r1, k=log(n))[2], 
  reg2=extractAIC(r2, k=log(n))[2]
)
ebic <- exp(-.5*(bic - min(bic)))
round(probs <- ebic/sum(ebic), 2)
# Almost 100% certain r2 is better (despite lower R.squared)

# Isolate the impactful races
is.asian <- ifelse(amazon$race == 3, 1, 0)
is.other <- ifelse(amazon$race == 5, 1, 0)

r3 <- lm(
  log.bought ~ c.region + c.income + c.connection + 
    is.asian + is.other
)
bic <- c(
  first=extractAIC(r2, k=log(n))[2], 
  second=extractAIC(r3, k=log(n))[2]
)
ebic <- exp(-.5*(bic - min(bic)))
round(probs <- ebic/sum(ebic), 2)
# 97% certain the latter model is better
# At this point, it appears that we won't get a high enough R.squared no matter what. So let's try to find an IV. 
# (done up top)


#############################################
##################       ####################
########### More Happening Below ############
##################       ####################
#############################################



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
boxplot(log.bought ~ amazon$hispanic) # sliiiight difference if hispanic 
# TODO: What other methods of teasing out statistical relevance can you use?

# Build a model holding all these variables constant and only varying connection

# Checking out: Is connection == 1 related to the others
summary(c.region.lm <- lm(connection ~ region)) # nothing
summary(c.size.lm <- lm(connection ~ size)) # nothing
summary(c.oldest.lm <- lm(connection ~ size)) # nothing
summary(c.race.lm <- lm(connection ~ race)) # nothing

summary(c.income.lm <- lm(connection ~ income)) # liiiittle bit (probably some multicollinearity)
summary(c.children.lm <- lm(connection ~ children)) # liiiittle bit (probably some multicollinearity)=
summary(c.hispanic.lm <- lm(connection ~ amazon$hispanic)) # liittle bit (probably some multicollinearity)


# Use income, children, and hispanic as interaction vars with connection
int.reg <- lm(
  log.bought ~ 
    connection + 
    connection*income - income + 
    connection*children - children + 
    connection*amazon$hispanic - amazon$hispanic)

summary(int.reg)

# Try connection & income only (the others aren't great)
int.reg.2 <- lm(
  log.bought ~ 
    connection + 
    connection*income - income
)

summary(int.reg.2)
n = nrow(amazon)
bic <- c(
  reg1=extractAIC(int.reg, k=log(n))[2], 
  reg2=extractAIC(int.reg.2, k=log(n))[2]
  )
ebic <- exp(-.5*(bic - min(bic)))
round(probs <- ebic/sum(ebic), 2)
# Almost 100% certain that int.reg.2 is better

# just connection
int.reg.3 <- lm(log.bought ~ connection)

bic <- c(
  reg1=extractAIC(int.reg.2, k=log(n))[2],
  reg2=extractAIC(int.reg.3, k=log(n))[2]
)
ebic <- exp(-.5*(bic - min(bic)))
round(probs <- ebic/sum(ebic), 2)
# Turns out int.reg.2 is better

# Not excluding income on its own
int.reg.4 <- lm(log.bought ~ connection + connection*income)
bic <- c(
  reg1=extractAIC(int.reg.2, k=log(n))[2],
  reg2=extractAIC(int.reg.4, k=log(n))[2]
)
ebic <- exp(-.5*(bic - min(bic)))
round(probs <- ebic/sum(ebic), 2)
# 99% certain the connection + connection*income var is better... 


# Use income as a categorical variable:
cat.income <- as.factor(income)

# Building new regression with income as categorical var
int.reg.5 <- lm(log.bought ~ 
                  connection + connection*cat.income - cat.income)
bic <- c(
  reg1=extractAIC(int.reg.2, k=log(n))[2],
  reg2=extractAIC(int.reg.5, k=log(n))[2]
)
ebic <- exp(-.5*(bic - min(bic)))
round(probs <- ebic/sum(ebic), 2)
# reg 2 is better



#############################################
##################       ####################
# FINAL POINT:
# This introduces selection bias. 
# We do not know the impact of internet access on amazon purchasing behavior for 
# families that already buy from amazon. It is entirely likely that new trends 
# about purchasing behavior (or more correctly: the decision not to purchase off 
# amazon.com) can be elucidated by looking at the dataset of people who have never 
# made a purchase from amazon.com
# The sample size of dial-up users is also relatively small (only 217 rows). This 
# may not be sufficiently large to make a statistically significant inference.
##################       ####################
#############################################