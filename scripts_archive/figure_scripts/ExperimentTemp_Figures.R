library(tidyverse)
library(here)

# ENSURE THAT tempData.R IS RUN BEFORE THIS SCRIPT! tempData.R creates the data
# frames necessary to create these figures.

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

TempPlot

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
                  fill = treatment),
              alpha = 0.7) +
  scale_fill_manual(values = c("#D0D5DD", "#E7B46C", "#D2615D")) +
  # Small/individual lines representing each treatment.
  geom_point(data = Temp_Time,
             aes(x = date_time,
                 y = temp_C,
                 colour = treatment),
             # Ignore warning about 'unknown aesthetic'... this code is 
             # still important and functioning!
             position = position_jitter(width = 0.55,
                                        height = 0.55),
             size = 0.4) +
  geom_line(data = factored_temp,
            aes(x = date_time,
                y = average, 
                fill = treatment),
            colour = "black",
            size = 0.8, alpha = 0.8) +
  scale_colour_manual(values = c("#D0D5DD", "#E7B46C", "#D2615D")) +
  scale_x_datetime(date_breaks = "1 day",
                   date_labels = c("1", "2", "3", "4", "5", "6", "7")) +
  labs(x = "Date",
       y = "Temperature (ºC)",
       colour = "Treatment") +
  theme_classic() +
  theme(legend.position = "none")

AverageTemps

ggsave("ExperimentTemps.pdf",
       AverageTemps,
       width = 6, height = 8,
       device = "pdf",
       path = here("figures"))

#------------------------------------------------------------------------------
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