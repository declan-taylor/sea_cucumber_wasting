library(tidyverse)
library(ggrepel)
library(here)
library(lubridate)
library(tibbletime)
#ENSURE THAT FOR "filter" and "select", you specify dyplr or another package. 

# Import and clean the data. Some manual data cleaning done in excel after 
# exporting from ONC database.
Baynes05 <- read_csv(here("data/ONC_Data/search24835759/BaynesSound_5mbss_edited.csv")) %>%
  drop_na(Temperature_C) %>%
  # Modifications to date colum: Time_UTC is date at noon, UTC. We can safely
  # drop the timestamp because it's unchanging.
  mutate (Date = date(Time_UTC),
          Year = as.character(Date)) %>%
  mutate (Month = str_extract(Year, "-[0-9]{2}-"),
          Month = as.numeric(str_extract(Month, "[0-9]{2}")),
          Year = as.numeric(str_extract(Year, "[0-9]{4}"))) %>%
  mutate (Roll_Temp = zoo::rollmean(Temperature_C, 30, fill = NA))

Baynes20 <- read_csv(here("data/ONC_Data/search24845992/BaynesSound_20mbss_edited.csv")) %>%
  drop_na(Temperature_C) %>%
  # Modifications to date colum: Time_UTC is date at noon, UTC. We can safely
  # drop the timestamp because it's unchanging.
  mutate (Date = date(Time_UTC),
          Year = as.character(Date)) %>%
  mutate (Month = str_extract(Year, "-[0-9]{2}-"),
          Month = as.numeric(str_extract(Month, "[0-9]{2}")),
          Year = as.numeric(str_extract(Year, "[0-9]{4}"))) %>%
  mutate (Roll_Temp = zoo::rollmean(Temperature_C, 30, fill = NA))

#-------------------------------------------------------------------------------

# Exploring the 05mbss temperature data: 
Baynes05_max <- Baynes05 %>%
  filter(Temperature_C == max(Baynes05$Temperature_C))

Baynes05_2020 <- Baynes05 %>%
  filter(Year == 2020)

Baynes05_2021 <- Baynes05 %>%
  filter(Year == 2021)

Baynes05_Summer2020 <- Baynes05 %>%
  filter(Year == 2020) %>%
  filter(Month == 7 | Month == 8)

Baynes05_Summer2021 <- Baynes05 %>%
  filter(Year == 2021) %>%
  filter(Month == 7 | Month == 8)

# Annual average values:
mean(Baynes05_2020$Temperature_C)
#11.628 (ºC)
mean(Baynes05_2021$Temperature_C)
# 11.006 (ºC)
mean(Baynes05$Temperature_C)
# 11.696 (ºC)

# Monthly average values:
mean(Baynes05_Summer2020$Temperature_C)
# 16.609 (ºC)
mean(Baynes05_Summer2021$Temperature_C)
# 18.283 (ºC)
# If you include September, it's 16.45ºC

(16.609 + 18.283)/2
# 17.446

#-------------------------------------------------------------------------------

# Exploring the 20mbss temperature data: 
Baynes20_max <- Baynes20 %>%
  filter(Temperature_C == max(Baynes20$Temperature_C))

Baynes20_2020 <- Baynes20 %>%
  filter(Year == 2020)

Baynes20_2021 <- Baynes20 %>%
  filter(Year == 2021)

Baynes20_Summer2020 <- Baynes20 %>%
  filter(Year == 2020) %>%
  filter(Month == 7 | Month == 8)

Baynes20_Summer2021 <- Baynes20 %>%
  filter(Year == 2021) %>%
  filter(Month == 7 | Month == 8)

# Annual average values:
mean(Baynes20_2020$Temperature_C)
#10.025 (ºC)
mean(Baynes20_2021$Temperature_C)
# 9.654 (ºC)
mean(Baynes20$Temperature_C)
# 10.189 (ºC)

# Monthly average values:
mean(Baynes20_Summer2020$Temperature_C)
# 11.916 (ºC)
mean(Baynes20_Summer2021$Temperature_C)
# 11.952 (ºC)

(11.916 + 11.952)/2
# 11.934

#-------------------------------------------------------------------------------

# Generate figures.
# 5mbss Baynes Sound
Baynes05_temp_plot <- ggplot(Baynes05) +
  geom_line(aes(x = Date, y = Temperature_C, color = Temperature_C),
            size = 1.3) +
  geom_line(aes(x = Date, y = Roll_Temp),
            size = 0.5) +
  scale_color_gradient2(low = "dodgerblue1",
                        mid = "lightyellow2",
                        high = "orangered1",
                        midpoint = 12) +
  geom_hline(yintercept = 17, color = "black", size = 1, alpha = 0.8) +
  geom_hline(yintercept = 12, color = "dodgerblue1", size = 1, alpha = 0.8) +
  geom_hline(yintercept = 22, color = "orangered1", size = 1, alpha = 0.8) +
  #ylim(-5,35) +
  labs(x = "Year",
       y = "Temperature (°C)",
       color = "Temperature (°C)") +
  theme_classic() +
  theme(legend.position = "none")

Baynes05_temp_plot

# 20mbss Baynes Sound
Baynes20_temp_plot <- ggplot(Baynes20) +
  geom_line(aes(x = Date, y = Temperature_C, color = Temperature_C),
            size = 1.3) +
  geom_line(aes(x = Date, y = Roll_Temp),
            size = 0.5) +
  scale_color_gradient2(low = "dodgerblue1",
                        mid = "lightyellow2",
                        high = "orangered1",
                        midpoint = 12) +
  geom_hline(yintercept = 17, color = "black", size = 1, alpha = 0.8) +
  geom_hline(yintercept = 12, color = "dodgerblue1", size = 1, alpha = 0.8) +
  geom_hline(yintercept = 22, color = "orangered1", size = 1, alpha = 0.8) +
  #ylim(-5,35) +
  labs(x = "Year",
       y = "Temperature (°C)",
       color = "Temperature (°C)") +
  theme_classic() +
  theme(legend.position = "none")

Baynes20_temp_plot

# Both depths on the same temperature plot
Baynes_temp_plot <- ggplot(data = NULL,
                           aes(x = Date, y = Temperature_C, color = Temperature_C)) +
  geom_line(data = Baynes05, 
            aes(x = Date, y = Temperature_C, color = Temperature_C),
            size = 1.3) +
  geom_line(data = Baynes20, 
            aes(x = Date, y = Temperature_C, color = Temperature_C),
            size = 1.3) +
  scale_color_gradient2(low = "dodgerblue1",
                        mid = "lightyellow2",
                        high = "orangered1",
                        midpoint = 12) +
    geom_line(data = Baynes05, 
              aes(x = Date, y = Roll_Temp),
              col = "black", alpha = 0.8,
              size = 0.5) +
    geom_line(data = Baynes20, 
              aes(x = Date, y = Roll_Temp),
              col = "black", alpha = 0.8,
              size = 0.5, linetype = "dashed") +
#  geom_hline(yintercept = 17, color = "black", size = 1, alpha = 0.8) +
#  geom_hline(yintercept = 12, color = "dodgerblue1", size = 1, alpha = 0.6) +
#  geom_hline(yintercept = 22, color = "orangered1", size = 1, alpha = 0.6) +
  geom_point(data = Baynes05_max, 
             aes(x = Date, y = Temperature_C)) +
  geom_label_repel(data = subset(Baynes05, Temperature_C > 21.6),
                   label = "Max Temp = 21.6ºC",
                   nudge_x = -150, nudge_y = 1,
                   label.r = 0.1,
                   size = 3) + 
  labs(x = "Year",
       y = "Temperature (°C)",
       color = "Temperature (°C)") +
  theme_classic() +
  theme(legend.position = "none")

Baynes_temp_plot

#

# Print the figures.
ggsave("Baynes Sound 5mbss Temp.pdf", 
       Baynes05_temp_plot,
       device = "pdf",
       path = here("figures"))
ggsave("Baynes Sound 20mbss Temp.pdf", 
       Baynes20_temp_plot,
       device = "pdf",
       path = here("figures"))
ggsave("Baynes_temp_plot.pdf",
       Baynes_temp_plot,
       device = "pdf",
       path = here("figures"))
