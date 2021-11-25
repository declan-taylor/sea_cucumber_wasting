# Statistics: Modelling the impact of treatment (and other variables) on 
# evisceration and spawning variables.

# Be sure to run BinaryVariables.R prior to running this script!
library(gamlss)
library(tidyverse)

# Drop the final row (all NAs) and the death_time and in_activity columns 
# (contain NAs) from `IndividualData`, so we can use gamlss models.
EviscSpawnData <- IndividualData %>%
  dplyr::select(-c(death_time, in_activity))

# 1. EVISCERATION: modelling the impact of treatment, weight, and guts status, 
# along with random effects, on evisceration. Based on data frame 
# `EviscSpawnData`.

# Determining the distribution of the data, based on our knowledge that it 
# follows a binomial distribution.
fitDist(evisceration, data = EviscSpawnData, type = "binom", try.gamlss = T)

# The FULL MODEL. Evisceration is dependent on treatment, and also cucumber 
# weight and pooping status. Sea table and table position are included as 
# random effects.
evisc.mod.full <- gamlss(evisceration ~ 
                      treatment + weight_g + poop + 
                      random(tableID),
                    family = BI(),
                    data = EviscSpawnData)

# The NULL MODEL.
evisc.mod.null <- gamlss(evisceration ~ 1,
                         family = BI(),
                         data = EviscSpawnData)

# Forwards selection.
fwd.evisc.mod <- stepGAIC(evisc.mod.null, 
                              scope = list(lower = evisc.mod.null,
                                           upper = evisc.mod.full),
                              direction = "forward", 
                              trace = F)
formula(fwd.evisc.mod)
## evisceration ~ poop + weight_g
summary(fwd.evisc.mod)
# poop (p = 0.0163) and weight (p = 0.0383) are both significant in explaining 
# variation in evisceration. Weight estimate = -0.004309.

# Backwards selection
bwd.evisc.mod <- stepGAIC(evisc.mod.null, 
                          direction = "backward", 
                          trace = F)
formula(fwd.evisc.mod)
## evisceration ~ weight_g + poop
summary(fwd.evisc.mod)

#-----------------------------------------------------------------------------
# 2. RESP_EVISC: modelling the impact of treatment, weight, and guts status, 
# along with random effects, on respiratory evisceration, which occurred only 
# twice, in the 22C treatment Based on data frame `EviscSpawnData`.

# Determining the distribution of the data, based on our knowledge that it 
# follows a binomial distribution.
fitDist(resp_evisc, data = EviscSpawnData, type = "binom", try.gamlss = T)

# The FULL MODEL. Respiratory evisceration is dependent on treatment, and also 
# cucumber weight and pooping status. Sea table and table position are included 
# (via tableID) as random effects.
respEvisc.mod.full <- gamlss(resp_evisc ~ 
                           treatment + weight_g + poop + 
                           random(tableID),
                         family = BI(),
                         data = EviscSpawnData)

# The NULL MODEL.
respEvisc.mod.null <- gamlss(resp_evisc ~ 1,
                         family = BI(),
                         data = EviscSpawnData)

# Forwards selection.
fwd.respEvisc.mod <- stepGAIC(respEvisc.mod.null, 
                          scope = list(lower = respEvisc.mod.null,
                                       upper = respEvisc.mod.full),
                          direction = "forward", 
                          trace = F)
formula(fwd.respEvisc.mod)
## resp_evisc ~ treatment
summary(fwd.respEvisc.mod)
## treatment is not significant but is the best explanation. p = 0.995.

#-----------------------------------------------------------------------------
# 3. SPAWN: determining if there is a significant correlation between spawning
# and treament.
fitDist(spawn, data = EviscSpawnData, type = "binom", try.gamlss = T)

# The FULL MODEL. Respiratory evisceration is dependent on treatment, and also 
# cucumber weight and pooping status. Sea table and table position are included 
# (via tableID) as random effects.
spawn.mod.full <- gamlss(spawn ~ 
                           treatment + weight_g + poop,# + 
                           #random(tableID),
                         family = BI(),
                         data = EviscSpawnData)

# The NULL MODEL.
spawn.mod.null <- gamlss(spawn ~ 1,
                         family = BI(),
                         data = EviscSpawnData)

# Forwards selection.
fwd.spawn.mod <- stepGAIC(spawn.mod.null, 
                          scope = list(lower = spawn.mod.null,
                                       upper = spawn.mod.full),
                          direction = "forward", 
                          trace = F)
formula(fwd.spawn.mod)
## If random effects are included, spawn ~ random(tableID).
## If they are not, spawn ~ treatment. Intercept and room are significant but heat is not.
summary(fwd.spawn.mod)
## treatment is not the best explanation... again!?