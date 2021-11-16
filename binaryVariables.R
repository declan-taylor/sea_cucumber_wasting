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
