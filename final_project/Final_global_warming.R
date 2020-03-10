########################################################################
########################################################################
########################################################################
# GLOBAL WARMING
li <- read.csv('LandIce.csv')
# TODO: Find an appropriate time-series regression model for Greenland, 
# Antarctica, and Ocean masses. 
# Analysis should include:
# development of a small number (up to 3) of candidate models, 
# selection of a best model for each dataset
# justification of each model selection choice 
# forecast for April 2016
# How would you forecast these levels in 10 years? 
# Note: You can ignore the data that are missing

head(li)
plot(li$Greenland.mass..Gt. ~ li$TIME..year.decimal.)
# Somewhat curvy downtrend
plot(li$Antarctica.mass..Gt. ~ li$TIME..year.decimal.)
# Generally downward with some dispersion
plot(li$Ocean.mass..mm. ~ li$TIME..year.decimal.)
# Generally rising, slightly polynomial looking

# Starting with Greenland only:
n <- length(unique(li$TIME..year.decimal.))

# Take ACF with as many lags as there are distinct times
acf(li$Antarctica.mass..Gt., lag=n)
# Can not use log here (some growths are negative)
# Not observing oscillations, so nothing applied to deal with them

