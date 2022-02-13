#### Temperature plots from collection sites ####
#### Data obtained from <https://www.ndbc.noaa.gov/station_history.php?station=bzbm3>

#libraries
library(tidyverse)
library(lubridate)
library(tibbletime)

# Read in data for Woods Hole, collection site for Astrangia poculata 
WH_2014 = read.delim("Data/Woods_hole/bzbm3h2014.txt", na.strings='999', sep = "", header = 1)
WH_2015 = read.delim("Data/Woods_hole/bzbm3h2015.txt", na.strings='999', sep = "", header = 1)
WH_2016 = read.delim("Data/Woods_hole/bzbm3h2016.txt", na.strings='999', sep = "", header = 1)
WH_2017 = read.delim("Data/Woods_hole/bzbm3h2017.txt", na.strings='999', sep = "", header = 1)
WH_2018 = read.delim("Data/Woods_hole/bzbm3h2018.txt", na.strings='999', sep = "", header = 1)
WH_2019 = read.delim("Data/Woods_hole/bzbm3h2019.txt", na.strings='999', sep = "", header = 1)
WH_2020 = read.delim("Data/Woods_hole/bzbm3h2020.txt", na.strings='999', sep = "", header = 1)

# Combine data and tidy
WH_temp = WH_2014 %>%
  bind_rows(WH_2015) %>%
  bind_rows(WH_2016) %>%
  bind_rows(WH_2017) %>%
  bind_rows(WH_2018) %>%
  bind_rows(WH_2019) %>%
  bind_rows(WH_2020) %>%
  dplyr::slice(-1) %>%
  mutate(Date = paste(paste(X.YY, MM,DD, sep="-")," ",paste(hh,mm, sep=":"),sep=""))%>%
  mutate(Date = strptime(Date, format = "%Y-%m-%d %H:%M")) %>%
  select(Date, WTMP) %>%
  mutate(WTMP = as.numeric(WTMP)) %>%
  as_tbl_time(Date) %>%
  mutate(Date = as.POSIXct(Date)) %>%
  collapse_by("hourly") %>%
  group_by(Date) %>%
  dplyr::summarise(mean_var = mean(WTMP)) %>%
  dplyr::rename(Temperature = mean_var) %>%
  filter(Temperature < 50)

# Plot it
wh_temp_plot = ggplot(WH_temp, aes(x = Date, y = Temperature, color = Temperature)) +
  geom_line(size = 1.1) +
  scale_color_gradient2(low = "dodgerblue1",
                        mid = "lightyellow2",
                        high = "orangered1",
                        midpoint = 12.35015) + #WH mean temp 12.35015
  geom_hline(yintercept = 18, color = "black", size = 1.5) +
  geom_hline(yintercept = 04, color = "dodgerblue1", size = 1.5) +
  geom_hline(yintercept = 30, color = "orangered1", size = 1.5) +
  ylim(-5,35) +
  labs(x = "Year",
       y = "Temperature (°C)",
       color = "Temperature (°C)") +
  theme_classic() +
  theme(legend.position = "none")


# Read in data for Radio Island, collection site of Oculina arbuscula
RI_2014 = read.delim("Data/Radio_island/2014.txt", na.strings='999', sep = "", header = 1)
RI_2015 = read.delim("Data/Radio_island/2015.txt", na.strings='999', sep = "", header = 1)
RI_2016 = read.delim("Data/Radio_island/2016.txt", na.strings='999', sep = "", header = 1) 
RI_2017 = read.delim("Data/Radio_island/2017.txt", na.strings='999', sep = "", header = 1)
RI_2018 = read.delim("Data/Radio_island/2018.txt", na.strings='999', sep = "", header = 1)
RI_2019 = read.delim("Data/Radio_island/2019.txt", na.strings='999', sep = "", header = 1)
RI_2020 = read.delim("Data/Radio_island/2020.txt", na.strings='999', sep = "", header = 1)

# Combine data and tidy
RI_temp = RI_2014 %>%
  bind_rows(RI_2015) %>%
  bind_rows(RI_2016) %>%
  bind_rows(RI_2017) %>%
  bind_rows(RI_2018) %>%
  bind_rows(RI_2019) %>%
  bind_rows(RI_2020) %>%
  dplyr::slice(-1) %>%
  mutate(Date = paste(paste(X.YY, MM,DD, sep="-")," ",paste(hh,mm, sep=":"),sep=""))%>%
  mutate(Date = strptime(Date, format = "%Y-%m-%d %H:%M")) %>%
  select(Date, WTMP) %>%
  mutate(WTMP = as.numeric(WTMP)) %>%
  as_tbl_time(Date) %>%
  mutate(Date = as.POSIXct(Date)) %>%
  collapse_by("hourly") %>%
  group_by(Date) %>%
  dplyr::summarise(mean_var = mean(WTMP)) %>%
  dplyr::rename(Temperature = mean_var) %>%
  filter(Temperature < 50)

# Plot
RI_temp_plot = ggplot(RI_temp, aes(x = Date, y = Temperature, color = Temperature)) +
  geom_line(size = 1.1) +
  scale_color_gradient2(low = "dodgerblue1",
                        mid = "lightyellow2",
                        high = "orangered1",
                        midpoint = 19.57785) +
  geom_hline(yintercept = 18, color = "black", size = 1.5) +
  geom_hline(yintercept = 06, color = "dodgerblue1", size = 1.5) +
  geom_hline(yintercept = 31, color = "orangered1", size = 1.5) +
  ylim(-5,35) +
  labs(x = "Year", y = "Temperature (°C)", color = "Temperature (°C)") +
  theme_classic() +
  theme(legend.position = "none")