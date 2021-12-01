library(here)
library(lubridate)
library(tidyverse)

create_spawnData <- function(datafile){
  
  SelectedData <- read_csv(here(paste0("data/", datafile)), col_names = TRUE) %>%
    # Format `Date` column to POSIX standard
    mutate("Date" = dmy(Date)) %>%
    mutate("dateTime" = paste(Date, Time, sep = "_")) %>%
    mutate(dateTime = ymd_hms(dateTime))
  
  SelectedData <- SelectedData %>%
    # remove the first 34 lines of data frame (pre-experiment data).
    tail(-34) %>%
    # Select and rename variables
    dplyr::select(date = Date,
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
    dplyr::mutate(spawn = gsub("[A-z]{3,4}", 1, spawn, ignore.case = TRUE))
  
  spawnData <- SelectedData %>%
    # text entries may be "yes" or "eggs".
    filter(FALSE == is.na(spawn)) %>%
    select(bucketID, spawn) %>%
    distinct(bucketID, .keep_all = TRUE)
  
  bucketList <- SelectedData %>% 
    select(bucketID, treatment) %>%
    distinct(bucketID, .keep_all = TRUE) %>%
    drop_na()
  
  spawnData <- full_join(bucketList, spawnData, by = "bucketID") %>%
    mutate(spawn = as.numeric(spawn))
  spawnData$spawn <- replace_na(spawnData$spawn, 0)
  
  spawnData <<- spawnData
}

# This function is exactly the same as the above, but the regular expressions
# are slightly adjusted to only pick up sperm data instead of sperm and egg 
# spawning.
create_spermData <- function(datafile){
  
  SelectedData <- read_csv(here(paste0("data/", datafile)), col_names = TRUE) %>%
    # Format `Date` column to POSIX standard
    mutate("Date" = dmy(Date)) %>%
    mutate("dateTime" = paste(Date, Time, sep = "_")) %>%
    mutate(dateTime = ymd_hms(dateTime))
  
  SelectedData <- SelectedData %>%
    # remove the first 34 lines of data frame (pre-experiment data).
    tail(-34) %>%
    # Select and rename variables
    dplyr::select(date = Date,
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
    dplyr::mutate(spawn = gsub("y[A-z]{2}", 1, spawn, ignore.case = TRUE))
  
  spawnData <- SelectedData %>%
    # text entries may be "yes" or "eggs".
    filter(FALSE == is.na(spawn)) %>%
    select(bucketID, spawn) %>%
    distinct(bucketID, .keep_all = TRUE)
  
  bucketList <- SelectedData %>% 
    select(bucketID, treatment) %>%
    distinct(bucketID, .keep_all = TRUE) %>%
    drop_na()
  
  spawnData <- full_join(bucketList, spawnData, by = "bucketID") %>%
    mutate(spawn = as.numeric(spawn))
  spawnData$spawn <- replace_na(spawnData$spawn, 0)
  
  spermData <<- spawnData
}

create_spawnData("DailyLog_final.csv")
create_spermData("DailyLog_final.csv")

sum(spawnData$spawn)

# Statistics on spawn data as a whole.
kruskal.test(spawn ~ treatment, data = spawnData)
# p = 0.379

# Statistics on specifically sperm-spawning data.
kruskal.test(spawn ~ treatment, data = spermData)
# p-value = 0.4865