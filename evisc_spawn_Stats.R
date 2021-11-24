# Statistics: Modelling the impact of treatment (and other variables) on 
# evisceration and spawning variables.


glm_evisc <- glm(evisceration ~ treatment, 
                 # Tell the glm function that you're using a binomial distribution
                 # and a "logit" link function.
                 family = binomial(link = "logit"), 
                 data = FinalBinary)
check_model(glm_evisc)

# RESP_EVISC
glm_resp_evisc <- glm(resp_evisc ~ treatment, 
                      # Tell the glm function that you're using a binomial distribution
                      # and a "logit" link function.
                      family = binomial(link = "logit"), 
                      data = FinalBinary)
check_model(glm_resp_evisc)

# SPAWN
glm_spawn <- glm(spawn ~ treatment, 
                 # Tell the glm function that you're using a binomial distribution
                 # and a "logit" link function.
                 family = binomial(link = "logit"), 
                 data = FinalBinary)
check_model(glm_spawn)