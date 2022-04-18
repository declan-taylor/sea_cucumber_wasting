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

# Plot temperature over time, and plot the 5 death times with temp.
TempPlot <- ggplot() +
  geom_line(data = Temp_Time,
            aes(x = date_time,
                y = temp_C,
                # Ignore warning about 'unknown aesthetic'... this code is 
                # still important and functioning!
                fill = bucketID)) +
  scale_x_datetime(date_breaks = "1 day",
                   date_labels = "%b %d") +
#  This is for the 5 dots indicating cucumber death.
#  geom_hline(aes(yintercept = 24),
#             colour = "lightgrey") +
# geom_text(label = "Mortality",
#          x = "2021-11-09 09:20:00",
#          y = 24) + 
#  geom_point(data = death_time,
#             aes(x = death_time,
#                 y = 24),
#             size = 2.5,
#             shape = 7, 
#             colour = "red") +
  geom_hline(aes(yintercept = 22,
             colour = "22ºC"),
             size = 1,
             alpha = 0.8) +
  geom_hline(aes(yintercept = 17,
             colour = "17ºC"),
             size = 1,
             alpha = 0.8) +
  geom_hline(aes(yintercept = 12,
             colour = "12ºC"),
             size = 1,
             alpha = 0.8) +
  labs(x = "Date",
       y = "Temperature (ºC)",
       colour = "Treatment Temperature") +
  scale_colour_manual(values = c("#0000CC", "#660066", "#CC0000")) +
  guides(colour = guide_legend(reverse = TRUE)) +
  scale_y_continuous(breaks = c(12, 14, 16, 18, 20, 22, 24))+
  theme_classic()

ggsave("temp_plot.png", 
       TempPlot,
       device = "png",
       path = here("figures"))


#------------------------------------------------------------------------------
# As per Dan's suggestion, I am going to make an average line for each 
# treatment with a ribbon indicating variation around that average. Ideally
# this can get plotted with the Baynes sound data.

factored_temp <- Temp_Time %>%
  group_by(date_time, treatment) %>%
  summarise(average = mean(temp_C),
            min = min(temp_C),
            max = max(temp_C)) %>%
  drop_na()

AverageTemps <- ggplot() +
  # Ribbon features of the width of temperature data
  geom_ribbon(data = factored_temp,
              aes(x = date_time,
                  ymin = min,
                  ymax = max,
                  fill = treatment)) +
  scale_fill_manual(values = c("grey70", "grey70", "grey70")) +
  # Small/individual lines representing each treatment.
  geom_point(data = Temp_Time,
            aes(x = date_time,
                y = temp_C),
                # Ignore warning about 'unknown aesthetic'... this code is 
                # still important and functioning!
            position = position_jitter(width = 0.55,
                                       height = 0),
            size = 0.4) +
  geom_line(data = factored_temp,
            aes(x = date_time,
                y = average, 
                fill = treatment),
            colour = factored_temp$average,
            size = 1.5) +
  scale_x_datetime(date_breaks = "1 day",
                   date_labels = "%b %d") +
  # Jon's code to fix x axis labels, I cannot figure it out just yet!
  #scale_x_discrete(breaks=c("2021-11-09","2021-11-10","2021-11-11", "2021-11-12","2021-11-13","2021-11-14","2021-11-15"),
  #                 labels=c("1", "2", "3", "4", "5","6", "7"))
  labs(x = "Date",
       y = "Temperature (ºC)",
       colour = "Treatment") +
  scale_color_gradient2(low = "dodgerblue1",
                        mid = "lightyellow2",
                        high = "orangered1",
                        midpoint = 13) +
  theme_classic() +
  theme(legend.position = "none")

AverageTemps

ggsave("ExperimentTemps.pdf",
       AverageTemps,
       width = 6, height = 8,
       device = "pdf",
       path = here("figures"))

# I'm also going to create a figure which is a subsetted version of the above,
# which just includes the range of the temperature within each treatment, and
# stacks these as bars to indicate the frequency of each temperature in each
# treatment.
factored_temp_3day <- Temp_Time %>%
  filter(date == "2021-11-10" | date == "2021-11-11" | date == "2021-11-12") %>%
  dplyr::select(treatment, temp_C) %>%
  mutate("label" = "Treatment temperature")

average_temp <- factored_temp_3day %>%
  group_by(treatment) %>%
  summarise(average = mean(temp_C),
            min = min(temp_C),
            max = max(temp_C))

AverageTemps_3day <- 
  ggplot(data = factored_temp_3day,
         aes(x = label, y = temp_C, colour = temp_C)) + 
    geom_hline(aes(yintercept = temp_C,
                   colour = temp_C),
               size = 2) +
    geom_hline(data = average_temp,
               aes(yintercept = average),
               colour = "black",
               size = 0.7) +
    geom_point(colour = "black",
               size = 0.4,
               position = position_jitter(width = 0.55,
                                          height = 0)) +
    geom_hline(data = average_temp,
               aes(yintercept = min - 0.1),
               colour = "black",
               linetype = "dotted") +
    geom_hline(data = average_temp,
               aes(yintercept = max + 0.1),
               colour = "black",
               linetype = "dotted") +
    scale_color_gradient2(low = "dodgerblue1",
                          mid = "lightyellow2",
                          high = "orangered1",
                          midpoint = 13) +
    ylim(5,24) +
    theme_classic() +
  labs(x = "",
       y = "Temperature (ºC)") +
    theme(legend.position = "none") +
  theme(aspect.ratio=33/9)

AverageTemps_3day

ggsave("StackedTemperature.pdf",
       AverageTemps_3day,
       device = "pdf",
       path = here("figures"))
