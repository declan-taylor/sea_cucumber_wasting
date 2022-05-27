library(here)
library(lubridate)
library(grid)
library(tidyverse)

# Importing data and formatting the date/time information.
DailyLog <- read_csv(here("data/DailyLog.csv"), col_names = TRUE) %>%
  # Format `Date` column to POSIX standard
  mutate("Date" = dmy(Date)) %>%
  # Create column with date and time info
  mutate("dateTime" = paste(Date, Time, sep = "_")) %>%
  # Format `dateTime` to POSIX standard
  mutate(dateTime = ymd_hms(dateTime)) %>%
  # Make `Sea_Table` and `Bucket_ID` factorial data.
  mutate(Sea_Table = as.factor(Sea_Table),
         Bucket_ID = as.factor(Bucket_ID)) %>%
  # Renaming!
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
  mutate(tableID = paste(sea_table, table_position)) %>%
  tail(-34)

# Making a new dataframe to get some values about the range and variation in our
# temperature data.
tempRange <- DailyLog %>%
  filter(date == "2021-11-10" | date == "2021-11-11" | date == "2021-11-12") %>%
  dplyr::select(date, date_time, tableID, treatment, temp_C) %>%
  filter(treatment == "control")

# Investigating the range and average temperatures of our 3 treatments.
range(tempRange$temp_C)
mean(tempRange$temp_C)
# FOR CONTROL: 10.8 to 14.0, mean = 12.38
# FOR ROOM: 14.8 to 17.9, mean = 16.58
# FOR HEAT: 19.6 to 23.3, mean =  21.71

# A statistical test to determine if the temperature treatments are significantly 
# different from each other, on the 11th and 12th of November, when no temp
# ramping was done.
tempStats <- DailyLog %>%
  filter(date == "2021-11-10" | date == "2021-11-11" | date == "2021-11-12") %>%
  #group_by(treatment) %>%
  #mutate(mean_temp = mean(temp_C)) %>%
  dplyr::select(date,
         bucketID,
         treatment,
         temp_C)

shapiro.test(tempStats$temp_C)
# p < 2.2e-16
kruskal.test(temp_C ~ treatment, data = tempStats)
# p < 2.2e-16
FSA::dunnTest(temp_C ~ treatment, data = tempStats)
# control-room p = 39.96e-26
# heat-room p = 2.32e-27

# Generating a list of the times at which the 5 cucumbers died.
death_time <- DailyLog %>%
  # Filter for rows with death data
  filter(FALSE == is.na(alive) | FALSE == is.na(death_time)) %>%
  # Use POSIXct standard for death_time
  mutate(death_time = ymd_hms(paste(date, death_time))) %>%
  dplyr::select(death_time, bucketID, temp_C) %>%
  mutate(placehold = 1)

# Generating a dataframe for the temperature data over time so that we can
# create a plot.
Temp_Time <- DailyLog %>%
  dplyr::select(date_time, date, sea_table, table_position, bucketID, tableID, temp_C, treatment)