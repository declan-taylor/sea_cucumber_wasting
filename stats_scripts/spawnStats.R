library(here)
library(lubridate)
library(tidyverse)

# This function generates a dataframe that our spawning statistical analysis 
# uses. `dplyr` functions that conflict with other packages are captioned 
# explicity .
create_spawnData <- function(datafile){
  SelectedData <- read_csv(here(paste0("data/", datafile)), col_names = TRUE) %>%
    # Format `Date` column to POSIX standard
    dplyr::mutate("Date" = dmy(Date),
                  "dateTime" = paste(Date, Time, sep = "_"),
                  dateTime = ymd_hms(dateTime))
  
  SelectedData <- SelectedData %>%
    # remove the first 34 lines of data frame (improperly formatted 
    # pre-experiment data).
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
    # replace any word such as "Yes", "yes", "egg", or "Eggs" in the spawn 
    # column with a `1`.
    dplyr::mutate(spawn = gsub("[A-z]{3,4}", 1, spawn, ignore.case = TRUE))
  
  # Select only buckets and spawn (`1`) data for those buckets that spawned.
  spawnData <- SelectedData %>%
    dplyr::filter(FALSE == is.na(spawn)) %>%
    dplyr::select(bucketID, spawn) %>%
    distinct(bucketID, .keep_all = TRUE)
  
  # Generate a list (1-col dataframe) with all of the buckets (bins) in the 
  # experiment listed by their ID numbers.
  bucketList <- SelectedData %>% 
    dplyr::select(bucketID, treatment) %>%
    distinct(bucketID, .keep_all = TRUE) %>%
    drop_na()
  
  # Join the spawn information (`1`) counts with the total list of all buckets
  # to create a dataframe of 1s and 0s of spawn data.
  spawnData <- full_join(bucketList, spawnData, by = "bucketID") %>%
    dplyr::mutate(spawn = as.numeric(spawn))
  spawnData$spawn <- replace_na(spawnData$spawn, 0)
  
  # Save this dataframe to the RStudio global environment.
  spawnData <<- spawnData
}

# This function is exactly the same as the above, but the regular expressions
# are slightly adjusted to only pick up sperm data instead of sperm and egg 
# spawning.
create_spermData <- function(datafile){
  
  SelectedData <- read_csv(here(paste0("data/", datafile)), col_names = TRUE) %>%
    dplyr::mutate("Date" = dmy(Date)) %>%
    dplyr::mutate("dateTime" = paste(Date, Time, sep = "_")) %>%
    dplyr::mutate(dateTime = ymd_hms(dateTime))
  
  SelectedData <- SelectedData %>%
    tail(-34) %>%
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
    # Note the regular expressions, which must start with [Y, y], and must be
    # 3 total characters.
    dplyr::mutate(spawn = gsub("y[A-z]{2}", 1, spawn, ignore.case = TRUE))
  
  spawnData <- SelectedData %>%
    dplyr::filter(FALSE == is.na(spawn)) %>%
    dplyr::select(bucketID, spawn) %>%
    distinct(bucketID, .keep_all = TRUE)
  
  bucketList <- SelectedData %>% 
    dplyr::select(bucketID, treatment) %>%
    distinct(bucketID, .keep_all = TRUE) %>%
    drop_na()
  
  spawnData <- full_join(bucketList, spawnData, by = "bucketID") %>%
    dplyr::mutate(spawn = as.numeric(spawn))
  spawnData$spawn <- replace_na(spawnData$spawn, 0)
  
  spermData <<- spawnData
}

# Running the 2 functions above to generate the dataframes required to run the 
# statistics below.
create_spawnData("DailyLog_final.csv")
create_spermData("DailyLog_final.csv")

# Find the number of cucumber bins that spawned.
sum(spawnData$spawn)
# 11 bins spanwed.

# Statistics on spawn data as a whole.
kruskal.test(spawn ~ treatment, data = spawnData)
FSA::dunnTest(spawn ~ treatment, data = spawnData)
# p = 0.379

# Statistics on specifically sperm-spawning data.
kruskal.test(spawn ~ treatment, data = spermData)
# p-value = 0.4865