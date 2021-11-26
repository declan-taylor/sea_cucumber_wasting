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
    mutate(combinedID = paste(bucketID, cukeID),
           tableID = paste(sea_table, table_position))
  
  spawnData <<- SelectedData %>%
    # text entries may be "yes" or "eggs".
    filter(FALSE == is.na(spawn)) %>%
    select(date_time, bucketID, spawn, treatment) %>%
    distinct(bucketID, .keep_all = TRUE)
}

create_spawnData("DailyLog_final.csv")
