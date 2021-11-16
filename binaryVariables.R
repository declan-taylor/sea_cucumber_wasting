library(here)
library(lubridate)
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
BinaryData <- DailyLog[35:1000000,] %>%
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
         resp_evic = respiratory_evisceration,
         spawn = Spawn) %>%
  # Select only rows with a value for at least one of the binary variables
  filter(FALSE == is.na(alive) |
         FALSE == is.na(death_time) |
         FALSE == is.na(poop) |
         FALSE == is.na(evisceration) |
         FALSE == is.na(resp_evic) |
         FALSE == is.na(spawn))

BinaryVariables <- c("alive", "death_time", "poop", "eviceration", "resp_evic", "spawn")

DeathData <- BinaryData %>%
  # Filter for rows with death data
  filter(FALSE == is.na(alive) | FALSE == is.na(death_time)) %>%
  # Use POSIXct standard for death_time
  mutate(death_time = ymd_hms(paste(date, death_time)))
