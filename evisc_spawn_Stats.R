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

# 2. RESP_EVISC: modelling the impact of treatment, weight, and guts status, 
# along with random effects, on respiratory evisceration, which occurred only 
# twice, in the 22C treatment Based on data frame `IndividualData`.

fitDist

glm_resp_evisc <- glm(resp_evisc ~ treatment, 
                      # Tell the glm function that you're using a binomial distribution
                      # and a "logit" link function.
                      family = binomial(link = "logit"), 
                      data = FinalBinary)
check_model(glm_resp_evisc)

# 3. SPAWN:
glm_spawn <- glm(spawn ~ treatment, 
                 # Tell the glm function that you're using a binomial distribution
                 # and a "logit" link function.
                 family = binomial(link = "logit"), 
                 data = FinalBinary)
check_model(glm_spawn)