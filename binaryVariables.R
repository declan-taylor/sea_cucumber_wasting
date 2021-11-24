library(here)
library(lubridate)
library(tidyverse)

# This function generates our individual level data with death_time, spawning, 
# evisceration, and poop data. The function exists so the dataframe can be 
# easily made in one click.
create_individualData <- function(datafile){
  # Import Data
  DailyLog <- read_csv(paste0("data/", datafile), col_names = TRUE) %>%
    # Format `Date` column to POSIX standard
    mutate("Date" = dmy(Date)) %>%
    mutate("dateTime" = paste(Date, Time, sep = "_")) %>%
    mutate(dateTime = ymd_hms(dateTime))
  
  # Generate data frame to hold just the binary variables. Upper limit of 
  # dataframe is intentionally too big (I'm just removing the top 35 rows).
  SelectedData <- DailyLog %>%
    # remove the first 34 lines of data frame (pre-experiment data).
    tail(-34) %>%
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
  
  # For each individual-level variable, create a dataframe that selects only
  # the rows for which that variable's column has data. Each dataframe is named
  # after the variable it represents.
  # Format that data to change all the values to "1". Select only the 
  # combinedID and <variable> columns, and keep only the distinct combinedID
  # values. This effectivley generates a list of buckets in which the <varible>
  # process (i.e. pooping) occured.
  death_time <- SelectedData %>%
    # Filter for rows with death data
    filter(FALSE == is.na(alive) | FALSE == is.na(death_time)) %>%
    # Use POSIXct standard for death_time
    mutate(death_time = ymd_hms(paste(date, death_time))) %>%
    select(death_time, combinedID)
  
  evisceration <- SelectedData %>%
    filter(FALSE == is.na(evisceration)) %>%
    # sub text entries like "yes" for `1`.
    mutate(evisceration = gsub("[A-z]{3}", 1, evisceration))%>%
    select(combinedID, evisceration) %>%
    distinct(combinedID, .keep_all = TRUE)
  
  resp_evisc <- SelectedData %>%
    filter(FALSE == is.na(resp_evisc)) %>%
    mutate(resp_evisc = gsub("[A-z]{3}", 1, resp_evisc))%>%
    select(combinedID, resp_evisc) %>%
    distinct(combinedID, .keep_all = TRUE)
  
  poop <- SelectedData %>%
    filter(FALSE == is.na(poop)) %>%
    mutate(poop = gsub("[A-z]{3}", 1, poop)) %>%
    select(combinedID, poop) %>%
    distinct(combinedID, .keep_all = TRUE)
  
  spawn <- SelectedData %>%
    # text entries may be "yes" or "eggs".
    mutate(spawn = gsub("[A-z]{3,4}", 1, spawn, ignore.case = TRUE)) %>%
    select(combinedID, spawn) %>%
    distinct(combinedID, .keep_all = TRUE)
  
  # List the variables for which dataframes were created.
  SelectedVariables <- c("death_time", "evisceration", "resp_evisc", "poop", "spawn")
  
  # Generate a 1-column data frame which is a list of all the combinedID values
  # (i.e. all the unique bins in our experiment).
  IndividualData <- SelectedData %>%
    select(date_time,
           sea_table,
           table_position,
           bucketID,
           cukeID,
           combinedID,
           treatment) %>%
    distinct(combinedID, .keep_all = TRUE) %>%
    drop_na()
  
  # Join each variable's data to the Binary
  for(i in SelectedVariables) {
    variable <- get(i)
    IndividualData <- full_join(IndividualData, variable, by = "combinedID")
    
    IndividualData[,i] <- IndividualData[,i] %>%
      replace_na()
  }


  #IndividualData$death_time <- IndividualData$death_time %>% replace_na("0")
  IndividualData$evisceration <- IndividualData$evisceration %>% 
    replace_na(0) %>%
    as.numeric(IndividualData$evisceration)
  IndividualData$resp_evisc <- IndividualData$resp_evisc %>% 
    replace_na(0) %>%
    as.numeric(IndividualData$resp_evisc)
  IndividualData$poop <- IndividualData$poop %>% 
    replace_na(0) %>%
    as.numeric(IndividualData$poop)
  IndividualData$spawn <- IndividualData$spawn %>% 
    replace_na(0) %>%
    as.numeric(IndividualData$spawn)
  
  # Fix data types and assign IndividualData to the global environment
  IndividualData <<- IndividualData %>%
    mutate(treatment = as.factor(treatment),
           table_position = as.factor(table_position),
           cukeID = as.factor(cukeID),
           poop = as.factor(poop),
           sea_table = as.factor(sea_table),
           bucketID = as.factor(bucketID))
}
  
add_stressData <- function(datafile){
  # Import data
  StressData <- read_csv(paste0("data/", datafile), col_names = TRUE) %>%
    # Format `Date` column to POSIX standard
    mutate("Date" = dmy(Date)) %>%
    mutate("dateTime" = paste(Date, Time, sep = "_")) %>%
    mutate(dateTime = ymd_hms(dateTime)) %>%
    # Make `Sea_Table` and `Bucket_ID` factorial data.
    mutate(Sea_Table = as.factor(Sea_Table),
           Bucket_ID = as.factor(Bucket_ID)) %>%
    select(date = Date,
           time = Time,
           date_time = dateTime,
           sea_table = Sea_Table,
           table_position = Table_Position,
           bucketID = Bucket_ID,
           cukeID = Cuke_ID,
           treatment = Treatment,
           activity = Activity_Score,
           squeeze = Squeeze_score,
           droop = Droop_score) %>%
    mutate(combinedID = paste(bucketID, cukeID))
  
  # Generate initial data values for activiyt, droop, and squeeze based on the 
  # readings taken on the first day of the experiment.
  initial_stress_values <- StressData %>%
   # dplyr method of doing: filter(date_time == "2021-11-09 09:40:00") 
    mutate(date = as.character(date),
           time = as.character(time)) %>%
    filter(date == "2021-11-09" & 
           time == "09:40:00") %>%
    # Make droop and squeeze data factorial
    mutate(droop = as.factor(droop),
           squeeze = as.factor(squeeze)) %>%
    # Columns to be added to IndividualData, with droop and squeeze indicated
    # as initial data.
    select(combinedID,
           in_activity = activity,
           in_droop = droop,
           in_squeeze = squeeze)
  
  IndividualData <<- full_join(IndividualData, initial_stress_values, by = "combinedID")
}

add_weightData <- function(datafile){
  WeightData <- read_csv(paste0("data/", datafile), col_names = TRUE) %>%
    mutate(combinedID = paste(Bucket_ID, Cuke_ID)) %>%
    mutate(weight_g= (Weight_g + Weight_2)/2) %>%
    select(weight_g, combinedID)
  
  IndividualData <<- full_join(IndividualData, WeightData, by = "combinedID")
}


# Run all 3 functions once, sequentially. Returned data frame should be 16 
# variables across.
create_individualData("DailyLog.csv")
add_stressData("BehaviourData.csv")
add_weightData("SizeData.csv")