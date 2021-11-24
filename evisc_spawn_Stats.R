# Statistics: Modelling the impact of treatment (and other variables) on 
# evisceration and spawning variables.

# Be sure to run BinaryVariables.R prior to running this script!
library(DHARMa)
library(performance)
library(gamlss)
library(tidyverse)

# Drop the final row (all NAs) and the death_time and in_activity columns 
# (contain NAs) from IndividualData, so we can use gamlss models.
IndividualData <- IndividualData %>%
  select(-c(death_time, in_activity)) %>%
  head(-1)

# 1. EVISCERATION: modelling the impact of treatment, weight, and guts status, 
# along with random effects, on evisceration. Based on data frame 
# `IndividualData`.

# Determining the distribution of the data, based on our knowledge that it 
# follows a binomial distribution.
fitDist(evisceration, data = IndividualData, type = "binom", try.gamlss = T)

# The FULL MODEL. Evisceration is dependent on treatment, and also cucumber 
# weight and pooping status. Sea table and table position are included as 
# random effects.
evisc.mod.full <- gamlss(evisceration ~ 
                      treatment + weight_g + poop + 
                      random(sea_table) + random(table_position),
                    family = BI(),
                    data = IndividualData)

# The NULL MODEL.
evisc.mod.null <- gamlss(evisceration ~ 1,
                         family = BI(),
                         data = IndividualData)

# Forwards selection.
fwd.evisc.mod <- stepGAIC(evisc.mod.null, 
                              scope = list(lower = evisc.mod.null,
                                           upper = evisc.mod.full),
                              direction = "forward", 
                              trace = F)
formula(fwd.evisc.mod)
## evisceration ~ weight_g + poop
summary(fwd.evisc.mod)
## weight_g is the only significant effect!

# Backwards selection
bwd.evisc.mod <- stepGAIC(evisc.mod.null, 
                          direction = "backward", 
                          trace = F)
formula(fwd.evisc.mod)
## evisceration ~ weight_g + poop
summary(fwd.evisc.mod)

# Sanity check: simulating residuals from the full model using the DHARMa 
# package.
evisc_residuals <- simulateResiduals(evisc.mod.full)
plot(evisc_residuals)
# Result: QQ plot looks great, proceeding with this model.



#-----------------------------------------------------------------------------
# 2. RESP_EVISC: modelling the impact of treatment, weight, and guts status, 
# along with random effects, on respiratory evisceration, which occurred only 
# twice, in the 22C treatment Based on data frame `IndividualData`.

# Determining the distribution of the data, based on our knowledge that it 
# follows a binomial distribution.
fitDist(resp_evisc, data = IndividualData, type = "binom", try.gamlss = T)

# The FULL MODEL. Respiratory evisceration is dependent on treatment, and also 
# cucumber weight and pooping status. Sea table and table position are included 
# as random effects.
respEvisc.mod.full <- gamlss(resp_evisc ~ 
                           treatment + weight_g + poop + 
                           random(sea_table) + random(table_position),
                         family = BI(),
                         data = IndividualData)

# The NULL MODEL.
respEvisc.mod.null <- gamlss(resp_evisc ~ 1,
                         family = BI(),
                         data = IndividualData)

# Forwards selection.
fwd.respEvisc.mod <- stepGAIC(evisc.mod.null, 
                          scope = list(lower = evisc.mod.null,
                                       upper = evisc.mod.full),
                          direction = "forward", 
                          trace = F)
formula(fwd.respEvisc.mod)
## resp_evisc ~ treatment
summary(fwd.respEvisc.mod)
## treatment is not significant but is the best explanation

# Sanity check: simulating residuals from the full model using the DHARMa 
# package.
respEvisc_residuals <- simulateResiduals(respEvisc.mod.full)
plot(respEvisc_residuals)
# Error!!!

#-----------------------------------------------------------------------------
# 3. SPAWN: determining if there is a significant correlation between spawning
# and treament.
fitDist(spawn, data = IndividualData, type = "binom", try.gamlss = T)

# The FULL MODEL. Respiratory evisceration is dependent on treatment, and also 
# cucumber weight and pooping status. Sea table and table position are included 
# as random effects.
spawn.mod.full <- gamlss(spawn ~ 
                           treatment + weight_g + poop + 
                           random(sea_table) + random(table_position),
                         family = BI(),
                         data = IndividualData)

# The NULL MODEL.
spawn.mod.null <- gamlss(spawn ~ 1,
                         family = BI(),
                         data = IndividualData)

# Forwards selection.
fwd.spawn.mod <- stepGAIC(evisc.mod.null, 
                          scope = list(lower = evisc.mod.null,
                                       upper = evisc.mod.full),
                          direction = "forward", 
                          trace = F)
formula(fwd.spawn.mod)
## evisceration ~ weight_g + poop
summary(fwd.spawn.mod)
## treatment is not the best explanation... again!?

# Sanity check: simulating residuals from the full model using the DHARMa 
# package.
spawn_residuals <- simulateResiduals(spawn.mod.full)
plot(spawn_residuals)
# Error!!!