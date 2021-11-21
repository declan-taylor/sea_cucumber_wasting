library(here)
library(lubridate)
library(fitdistrplus)
library(performance)
library(gamlss)
library(tidyverse)

# Import Data
DailyLog <- read_csv("data/DailyLog.csv", col_names = TRUE) %>%
  # Format `Date` column to POSIX standard
  mutate("Date" = dmy(Date)) %>%
  mutate("dateTime" = paste(Date, Time, sep = "_")) %>%
  mutate(dateTime = ymd_hms(dateTime)) %>%
  # Make `Sea_Table` and `Bucket_ID` factorial data.
  mutate(Sea_Table = as.factor(Sea_Table),
         Bucket_ID = as.factor(Bucket_ID))

# Generate data frame to hold just the binary variables. Upper limit of 
# dataframe is intentionally too big (I'm just removing the top 35 rows).
SelectedData0 <- DailyLog[35:1118,] %>%
  # Select and rename variables
  select(date = Date,
         date_time = dateTime,
         sea_table = Sea_Table,
         table_position,
         bucketID = Bucket_ID,
         cukeID = Cuke_ID,
         treatment = Treatment,
         temp_C = Temp_C,
         alive = Alive,
         death_time = `Time of Death`,
         poop = Poop,
         evisceration = Evisceration,
         resp_evisc = respiratory_evisceration,
         spawn = Spawn) %>%
  mutate(combinedID = paste(bucketID, cukeID))
  # Select only rows with a value for at least one of the binary variables
SelectedData <- SelectedData0 %>%
  filter(FALSE == is.na(alive) |
         FALSE == is.na(death_time) |
         FALSE == is.na(poop) |
         FALSE == is.na(evisceration) |
         FALSE == is.na(resp_evisc) |
         FALSE == is.na(spawn))

DeathData <- SelectedData %>%
  # Filter for rows with death data
  filter(FALSE == is.na(alive) | FALSE == is.na(death_time)) %>%
  # Use POSIXct standard for death_time
  mutate(death_time = ymd_hms(paste(date, death_time))) %>%
  select(death_time, combinedID)

EviscData <- SelectedData %>%
  # Filter for rows with evisceration data
  filter(FALSE == is.na(evisceration)) %>%
  mutate(evisceration = gsub("[A-z]{3}", 1, evisceration))%>%
  select(combinedID, evisceration) %>%
  distinct(combinedID, .keep_all = TRUE)

Resp_EviscData <- SelectedData %>%
  # Filter for rows with evisceration data
  filter(FALSE == is.na(resp_evisc)) %>%
  mutate(resp_evisc = gsub("[A-z]{3}", 1, resp_evisc))%>%
  select(combinedID, resp_evisc) %>%
  distinct(combinedID, .keep_all = TRUE)

PoopData <- SelectedData %>%
  # Filter for rows with evisceration data
  filter(FALSE == is.na(poop)) %>%
  mutate(poop = gsub("[A-z]{3}", 1, poop)) %>%
  select(combinedID, poop) %>%
  distinct(combinedID, .keep_all = TRUE)

SpawnData <- SelectedData %>%
  mutate(spawn = gsub("[A-z]{3,4}", 1, spawn, ignore.case = TRUE)) %>%
  select(combinedID, spawn) %>%
  distinct(combinedID, .keep_all = TRUE)

#SpermData <- SelectedData %>%
  # Filter for rows with evisceration data
 # mutate(spawn = gsub("yes", 1, spawn, ignore.case = TRUE)) %>%
#  filter(spawn == 1) %>%
#  rename(sperm_spawn = spawn)%>%
#  select(combinedID, sperm_spawn) %>%
#  distinct(combinedID, .keep_all = TRUE)

#EggData <- SelectedData %>%
  # Filter for rows with evisceration data
 # mutate(spawn = gsub("e[A-z]{2,3}", 1, spawn, ignore.case = TRUE)) %>%
#  filter(spawn == 1) %>%
#  rename(egg_spawn = spawn)%>%
 # select(combinedID, egg_spawn) %>%
#  distinct(combinedID, .keep_all = TRUE)

BinaryVariables <- c("DeathData", "EviscData", "Resp_EviscData", "PoopData", "SpawnData")
BinaryData <- SelectedData %>%
  distinct(combinedID)

for(i in BinaryVariables) {
  variable <- get(i)
  BinaryData <- full_join(BinaryData, variable, by = "combinedID")
}

rm(variable, SpawnData, PoopData, EviscData)

FinalBinary <- SelectedData0 %>%
  select(date_time,
       sea_table,
       table_position,
       bucketID,
       cukeID,
       combinedID,
       treatment) %>%
  distinct(combinedID, .keep_all = TRUE) %>%
  drop_na()

FinalBinary <- left_join(FinalBinary, BinaryData, by = "combinedID")

FinalBinary$death_time <- FinalBinary$death_time %>% replace_na("0")
FinalBinary$evisceration <- FinalBinary$evisceration %>% 
  replace_na(0) %>%
  as.numeric(FinalBinary$evisceration)
FinalBinary$resp_evisc <- FinalBinary$resp_evisc %>% 
  replace_na(0) %>%
  as.numeric(FinalBinary$resp_evisc)
FinalBinary$poop <- FinalBinary$poop %>% 
  replace_na(0) %>%
  as.numeric(FinalBinary$poop)
FinalBinary$spawn <- FinalBinary$spawn %>% 
  replace_na(0) %>%
  as.numeric(FinalBinary$spawn)

# EVISCERATION
glm_evisc <- glm(evisceration ~ treatment, 
               # Tell the glm function that you're using a binomial distribution
               # and a "logit" link function.
               family = binomial(link = "logit"), 
               data = FinalBinary)-
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
