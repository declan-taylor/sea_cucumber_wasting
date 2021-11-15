library(here)
library(lubridate)
library(tidyverse)

# Import the data and format the date/time information
DailyLog <- read_csv("data/DailyLog.csv", col_names = TRUE) %>%
  # Format `Date` column to POSIX standard
  mutate("Date" = dmy(Date)) %>%
  # Create column with date and time info
  mutate("dateTime" = paste(Date, Time, sep = "_")) %>%
  # Format `dateTime` to POSIX standard
  mutate(dateTime = ymd_hms(dateTime)) %>%
  # Make `Sea_Table` and `Bucket_ID` factorial data.
  mutate(Sea_Table = as.factor(Sea_Table),
         Bucket_ID = as.factor(Bucket_ID))

Temp_Time <- DailyLog[35:1062,] %>%
  select(dateTime, Sea_Table, table_position, Bucket_ID, Cuke_ID, Temp_C)

TempPlot <- ggplot(data = Temp_Time,
                   aes(x = dateTime,
                       y = Temp_C,
                       fill = Bucket_ID)) +
  geom_line() +
  labs(x = "Date",
       y = "Temperature (ºC)")

TempPlot
