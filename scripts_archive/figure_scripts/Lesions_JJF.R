# Libraries for data and figures.
library(here)
library(tidyverse)
library(lubridate)
# Libraries for stats only.
library(Hmisc)
library(ordinal)
library(gamlss)

lesion <- read_csv(here("data/BehaviourData_Final.csv")) %>%
  mutate(
    Bucket_ID = as.factor(Bucket_ID),
    Cuke_ID = as.factor(Cuke_ID),
    Unique_ID = paste(Bucket_ID, Cuke_ID,  
                      sep = '_'), 
    Table_ID = paste(Sea_Table, Table_Position, 
                     sep='_'), 
    Date = as.Date(Date, format="%d-%m-%Y")) %>%
  dplyr::select(c(Unique_ID, Bucket_ID, Table_ID, Date, Treatment, Number_lesions)) %>%
  na.omit()


# find max number of lesions per individual
lesion_max <- lesion %>%
  group_by(Unique_ID) %>%
  mutate(max_lesions = max(Number_lesions)) %>%
  distinct(Unique_ID, .keep_all = TRUE) %>%
  dplyr::select(-"Number_lesions")

# reading in weight data
size <- read_csv(here("data/SizeData.csv")) %>%
  mutate(
    Bucket_ID = as.factor(Bucket_ID),
    Cuke_ID = as.factor(Cuke_ID),
    Unique_ID = paste(Bucket_ID, Cuke_ID,  sep = '_'),
    mean_weight = (Weight_g+Weight_2)/2) %>%
  dplyr::select(c(Unique_ID, mean_weight))

# Using functions from the `BinaryVariables.R` script to combine individual 
# data with lesion data. This function generates our **individual level data** 
# with death_time, spawning, evisceration, and poop data. The function exists 
# so the dataframe can be easily made in one click.
create_individualData <- function(datafile){
  # Import Data
  DailyLog <- read_csv(here(paste0("data/", datafile)), col_names = TRUE) %>%
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
    # Generate a `combinedID` from bucketID (1-30) and cukeID (A or B), which is 
    # unique to each individual cucumber in the study.
    mutate(combinedID = paste(bucketID, cukeID),
           tableID = paste(sea_table, table_position))
  
  # For each individual-level variable, create a dataframe that selects only
  # the rows for which that variable's column has data. Each dataframe is named
  # after the variable it represents.
  # Format that data to change all the values to "1". Select only the 
  # combinedID and <variable> columns, and keep only the distinct combinedID
  # values. This effectively generates a list of cucumbers in which the
  # <varible> process (i.e. pooping) occurred.
  death_time <- SelectedData %>%
    # Filter for rows with death data
    filter(FALSE == is.na(alive) | FALSE == is.na(death_time)) %>%
    # Use POSIXct standard for death_time
    mutate(death_time = ymd_hms(paste(date, death_time))) %>%
    dplyr::select(death_time, combinedID)
  
  evisceration <- SelectedData %>%
    filter(FALSE == is.na(evisceration)) %>%
    # sub text entries like "yes" for `1`.
    mutate(evisceration = gsub("[A-z]{3}", 1, evisceration))%>%
    dplyr::select(combinedID, evisceration) %>%
    distinct(combinedID, .keep_all = TRUE)
  
  resp_evisc <- SelectedData %>%
    filter(FALSE == is.na(resp_evisc)) %>%
    mutate(resp_evisc = gsub("[A-z]{3}", 1, resp_evisc))%>%
    dplyr::select(combinedID, resp_evisc) %>%
    distinct(combinedID, .keep_all = TRUE)
  
  poop <- SelectedData %>%
    filter(FALSE == is.na(poop)) %>%
    mutate(poop = gsub("[A-z]{3}", 1, poop)) %>%
    dplyr::select(combinedID, poop) %>%
    distinct(combinedID, .keep_all = TRUE)
  
  spawn <- SelectedData %>%
    # text entries may be "yes" or "eggs".
    mutate(spawn = gsub("[A-z]{3,4}", 1, spawn, ignore.case = TRUE)) %>%
    filter(FALSE == is.na(spawn)) %>%
    dplyr::select(combinedID, spawn) %>%
    distinct(combinedID, .keep_all = TRUE)
  
  # List the variables for which dataframes were created.
  SelectedVariables <- c("death_time", "evisceration", "resp_evisc", "poop", "spawn")
  
  # Generate a 1-column data frame which is a list of all the combinedID values
  # (i.e. all the unique bins in our experiment).
  IndividualData <- SelectedData %>%
    dplyr::select(date_time,
           tableID,
           bucketID,
           cukeID,
           combinedID,
           treatment) %>%
    distinct(combinedID, .keep_all = TRUE) %>%
    drop_na()
  
  # For each of the individual data frames made above, join the `1` data 
  # (selected occurance data) to the 1-column dataframe  for only the values 
  # with 1s. The rest are left as NAs.
  for(i in SelectedVariables) {
    variable <- get(i)
    IndividualData <- full_join(IndividualData, variable, by = "combinedID")
    
    IndividualData[,i] <- IndividualData[,i] %>%
      replace_na()
  }
  
  # Replace NA values with 0s and make each column numberic; turns each binary
  # response variable into a binary data column with 0s and 1s.
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
  
  # Fix data types to factors and assign IndividualData to the global environment
  IndividualData <<- IndividualData %>%
    mutate(treatment = as.factor(treatment),
           cukeID = as.factor(cukeID),
           poop = as.factor(poop),
           tableID = as.factor(tableID),
           bucketID = as.factor(bucketID))
}

# This function generates a dataframe of initial activity and stress scores and
# joins said dataframe to the 'master' IndividualData dataframe.
add_stressData <- function(datafile){
  # Import data
  StressData <- read_csv(here(paste0("data/", datafile)), col_names = TRUE) %>%
    # Format `Date` column to POSIX standard
    mutate("Date" = dmy(Date)) %>%
    mutate("dateTime" = paste(Date, Time, sep = "_")) %>%
    mutate(dateTime = ymd_hms(dateTime)) %>%
    dplyr::select(date = Date,
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
  
  # Generate initial data values for activity, droop, and squeeze based on the 
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
    dplyr::select(combinedID,
           in_activity = activity,
           in_droop = droop,
           in_squeeze = squeeze)
  
  IndividualData <<- full_join(IndividualData, initial_stress_values, by = "combinedID")
}

# This function adds weight data to the master IndividualData dataframe.
add_weightData <- function(datafile){
  WeightData <- read_csv(here(paste0("data/", datafile)), col_names = TRUE) %>%
    mutate(combinedID = paste(Bucket_ID, Cuke_ID)) %>%
    # Create an average weight from the two weight measurements taken at the 
    # start of teh experiment.
    mutate(weight_g = (Weight_g + Weight_2)/2) %>%
    dplyr::select(weight_g, combinedID)
  
  # Join the weight data to the full dataframe.
  IndividualData <<- full_join(IndividualData, WeightData, by = "combinedID")
}

# Run all 3 functions once, sequentially. Returned data frame should be 16 
# variables across.
create_individualData("DailyLog_final.csv")
add_stressData("BehaviourData_final.csv")
add_weightData("SizeData.csv")

#------------------------------------------------------------------------------
# STATS
# adding weight data to lesion data
individual_pooping <- IndividualData %>%
  mutate(Unique_ID = paste(bucketID, cukeID, sep="_"))%>%
  dplyr::select(c("Unique_ID", "poop", "evisceration"))


lesion_max <- merge(lesion_max, individual_pooping, by=c("Unique_ID"))
lesion_max <- merge(lesion_max, size, by=c("Unique_ID"))


# adding a column stating if a cucumber had lesions
lesion_max$lesion_presence <- ifelse(lesion_max$max_lesions==0, "N", "Y")

lesions_prop <- lesion_max %>%
  group_by(Treatment) %>%
  count(lesion_presence)

# create a data frame with counts for major lesions.
major <- read_csv(here("data/BehaviourData_Final.csv")) %>%
  mutate(
    Treatment = fct_relevel(Treatment, 
                            c("Control","Room","Heat")),
    Bucket_ID = as.factor(Bucket_ID),
    Cuke_ID = as.factor(Cuke_ID),
    Unique_ID = paste(Bucket_ID, Cuke_ID, 
                      sep = '_'), 
    Table_ID = paste(Sea_Table, Table_Position, 
                     sep='_'), 
    Date = as.Date(Date, format="%d-%m-%Y")) %>%
  dplyr::select(c(Unique_ID, Bucket_ID, Table_ID, Date, Treatment, 'Bodywall lesions')) %>%
  rename(major_lesions = 'Bodywall lesions') %>%
  na.omit() %>%
  group_by(Unique_ID) %>%
  mutate(major_lesions = max(major_lesions)) %>%
  distinct(Unique_ID, .keep_all=TRUE)

# finding the distribution that best fits our response variable (max_lesions)
gamlss::fitDist(max_lesions, data=lesion_max, type="counts", try.gamlss=TRUE)
gamlss::histDist(lesion_max$max_lesions, "GEOM", density=T) # BEST FIT FOR DATA
# geometric distribution is best (woohoo!)

lesions_Full = gamlss(max_lesions ~ mean_weight + Treatment + poop + evisceration+
                      random(as.factor(Table_ID)) + random(as.factor(Bucket_ID)),
                      family = GEOM(), data = lesion_max)
summary(lesions_Full)


step.lesions.backward <- stepGAIC(lesions_Full, 
                              direction = "backward", trace = F)
summary(step.lesions.backward)

str(lesion_max)

#------------------------------------------------------------------------------
# FIGURES

# Total lesion count box plots
minor_lesions <- ggplot(data = lesion_max, 
                      aes(x = Treatment, 
                          y = max_lesions, 
                          fill = Treatment)) +
  geom_boxplot(outlier.shape= NA, 
               color="black", alpha=0.8) +
  geom_point(alpha=0.5, position=position_dodge2(0.2), color = "black") +
  scale_y_continuous(expand = c(0,0), 
                     limits = c(-1,13.2))+
  scale_x_discrete(labels = c("Control", "Warm", "Heat Wave")) +
  scale_fill_manual(values = c("Gold", "Orange","Red")) +
  ylab("Total Lesions / Indiv.") +
  xlab("Treatment") +
  theme_bw() +
  theme(panel.grid=element_blank(), 
        legend.position="none")

# Box plots but just for major lesions.
major_lesions <- ggplot(data = major, 
                        aes(x = Treatment, 
                            y = major_lesions, 
                            fill = Treatment)) +
  geom_boxplot(outlier.shape= NA, 
               color="black", 
               alpha=0.8) +
  geom_point(alpha=0.5, 
             position=position_dodge2(0.2), 
             color="black") +
  scale_y_continuous(expand=c(0,0), 
                     limits = c(-1,13.2))+
  scale_x_discrete(labels = c("Control", "Warm", "Heat Wave")) +
  scale_fill_manual(values = c("Gold", "Orange","Red")) +
  ylab("Major Lesions / Indiv.") +
  xlab("Treatment") +
  theme_bw() +
  theme(panel.grid=element_blank(), 
        legend.position="none")

ggsave("MinorLesions_boxplot.pdf", 
       minor_lesions,
       height = 4, width = 4,
       device = "pdf",
       path = here("figures"))

ggsave("MajorLesions_boxplot.pdf", 
       major_lesions,
       height = 4, width = 4,
       device = "pdf",
       path = here("figures"))
