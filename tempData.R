library(here)
library(lubridate)
library(grid)
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
         Bucket_ID = as.factor(Bucket_ID)) %>%
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
  mutate(tableID = paste(sea_table, table_position)) %>%
  tail(-34)

# Making a new dataframe to get some values about the range and varition in our
# temperature data.
tempRange <- DailyLog %>%
  filter(date == "2021-11-11" | date == "2021-11-12") %>%
  select(date, date_time, tableID, treatment, temp_C) %>%
  filter(treatment == "room")

range(tempRange$temp_C)
mean(tempRange$temp_C)
# FOR CONTROL: 11.2 to 14.0, mean = 12.5
# FOR ROOM: 14.9 to 17.9, mean = 16.79
# FOR HEAT: 20.0 to 23.3, mean =  21.86

death_time <- DailyLog %>%
  # Filter for rows with death data
  filter(FALSE == is.na(alive) | FALSE == is.na(death_time)) %>%
  # Use POSIXct standard for death_time
  mutate(death_time = ymd_hms(paste(date, death_time))) %>%
  select(death_time, bucketID, temp_C) %>%
  mutate(placehold = 1)

Temp_Time <- DailyLog %>%
  select(date_time, date, sea_table, table_position, bucketID, tableID, temp_C)

DateVector <- (c(ymd("2021-11-09"),"2021-11-10","2021-11-11", "2021-11-12","2021-11-13","2021-11-14","2021-11-15"))

TempPlot <- ggplot() +
  geom_line(data = Temp_Time,
            aes(x = date_time,
                y = temp_C,
                # Ignore warning about 'unknown aesthetic'... this code is 
                # still important and functioning!
                fill = bucketID)) +
  scale_x_datetime(date_breaks = "1 day",
                   date_labels = "%b %d") +
  # This is for the 5 dots indicating cucumber death.
  geom_hline(aes(yintercept = 24),
             colour = "lightgrey") +
  geom_point(data = death_time,
             aes(x = death_time,
                 y = 24),
             size = 2.5,
             shape = 7, 
             colour = "red") +
  geom_hline(aes(yintercept = 22,
             colour = "22ºC")) +
  geom_hline(aes(yintercept = 17,
             colour = "17ºC")) +
  geom_hline(aes(yintercept = 12,
             colour = "12ºC")) +
  labs(x = "Date",
       y = "Temperature (ºC)",
       colour = "Treatment Temperature") +
  scale_colour_manual(values = c("#0000CC", "#660066", "#CC0000")) +
    guides(colour = guide_legend(reverse = TRUE)) +
 #GEOM TEXT NOT SHOWING UP?
   geom_text(label = "Mortality",
            x = "2021-11-09 09:20:00",
            y = 24) + 
  theme_classic()

TempPlot

ggsave("temp_plot.png", 
       TempPlot,
       device = "png",
       path = here("figures"))

Temp_Time %>%
  select(date, bucketID, temp_C) %>%
  filter(temp_C > 20.5)
  
