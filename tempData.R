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
         Bucket_ID = as.factor(Bucket_ID))

Temp_Time <- DailyLog[35:1062,] %>%
  select(dateTime, Sea_Table, table_position, Bucket_ID, Cuke_ID, Temp_C)

# TEMPERATURE PLOT
# Create the colour gradient for the background:
#grad <- colorRampPalette(c("#CC0000", "#0000CC"), alpha = 1)(20)

TempPlot <- ggplot(data = Temp_Time,
                   aes(x = dateTime,
                       y = Temp_C,
                       fill = Bucket_ID)) +
  #annotation_custom(rasterGrob(grad, 
  #                             width=unit(1,"npc"), 
  #                             height=unit(1,"npc"))) +
  geom_line() +
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
  theme_classic()

TempPlot

ggsave("temp_plot.png", 
       TempPlot,
       device = "png",
       path = here("figures"), width=7, height=4)
