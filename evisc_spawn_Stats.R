# Statistics: Modelling the impact of treatment (and other variables) on 
# evisceration and spawning variables.

# Drop the final row (all NAs) and the death_time column (mostly NA's) from 
# IndividualData.
IndividualData <- IndividualData %>%
  select(-death_time) %>%
  head(-1)

# 1. EVISCERATION: modelling the impact of treatment, weight, and guts status, 
# along with random effects, on evisceration. Based on data frame 
# `IndividualData`.

# Determining the distribution of the data
fitDist(evisceration, data = IndividualData, type = "binom", try.gamlss = T)
# returns AIC value of 54.94


glm_evisc_null <- glm(evisceration ~ treatment + weight_g + poop + 
                 # Tell the glm function that you're using a binomial distribution
                 # and a "logit" link function.
                 family = binomial(link = "logit"), 
                 data = IndividualData)

summary(glm_evisc)
check_model(glm_evisc)

# 2. RESP_EVISC: modelling the impact of treatment, weight, and guts status, 
# along with random effects, on respiratory evisceration, which occured only 
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