library(gamlss)
library(tidyverse)

DeathData <- IndividualData %>%
  dplyr::mutate(death_time = gsub(".{1,}", 1, death_time)) %>%
  dplyr::select(-in_activity)

DeathData$death_time <- DeathData$death_time %>% 
  replace_na(0) %>%
  as.numeric(IndividualData$death_time)

DeathData <- DeathData %>%
  rename("death" = death_time)

death_aov <- aov(death ~ treatment, data = DeathData)
summary(death_aov)
TukeyHSD(death_aov)

# Looking at the distribution of death data to prepare a model.
fitDist(death, data = DeathData, type = "binom", try.gamlss = T)

# We looked at what non-treatment factors affected death to learn more about 
# what may increase a heated cucumber's chance of death.
# Full model, no random effects. BI() used as family as death is represented by
# a 1, and survival a 0. Respiratory evisceration is omitted because all those
# that eviscerated their respiratory tree died.
death.mod.full <- gamlss(death ~ evisceration + poop + spawn + in_droop + in_squeeze + weight_g +
                        random()
                    family = BI(),
                    data = DeathData)

death.mod.null <- gamlss(death ~ 1,
                         family = BI(),
                         data = DeathData)

# Forwards selection.
fwd.death.mod <- stepGAIC(death.mod.null, 
                          scope = list(lower = death.mod.null,
                                       upper = death.mod.full),
                          direction = "forward", 
                          trace = F)
formula(fwd.death.mod)
## death ~ spawn
summary(fwd.death.mod)
# WHY IS INCERCEPT ***?