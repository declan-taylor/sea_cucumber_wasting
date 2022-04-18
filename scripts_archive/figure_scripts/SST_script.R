###

# Cuke SST exploratory analysis
# Template code by Steven Brownlee
# Revised by Em Lim March 23, 2022

### Load packages -----

library(raster)
library(tidyverse)
library(lubridate)
library(ncdf4)

#set theme
theme_set(theme_classic())


### Read files ------

# Read in files from template mask, and list of raster MODIS SST files. 
setwd("~/Downloads/isa_cuke_project_new")

cuke_mask <- raster('mask_template_raster.tif')

temp_in <- list.files('~/Downloads/isa_cuke_project_new/imagery/full_file_list', full.names = TRUE)

cuke_table_seq <- seq(1, 550, 1) # Modify to total number of input rasters.

t <- read_csv('cuke_temp_table.csv')

# Read in template CSV.

###

# Reproject rasters, shorten names for ease of processing. 
# THIS LOOP DOESN'T WORK AND I DON'T KNOW WHY

#setwd("~/Downloads/isa_cuke_project_new/imagery/new_shortened")

#for (i in temp_in){
#  y <- raster(i)
#  z <- projectRaster(y, crs = '+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs')
#  a = substr(i, 86, 94) # adjust to your own path length such that the date is selected
#  raster::writeRaster(z, filename = i, format = 'GTiff')
#}
# OOOOOOKAY THIS SOMEHOW IS STILL NOT WORKING BUT SOMEHOW I MANAGED TO GET ALL THE .NC FILES IN THE SAME FULL_FILE_LIST FOLDER AS .TIFFS SO FUCK IT I USED RENAME TO CUT THEM DOWN TO THEIR DATE RANGES AND LETS GO FROM THERE

###

# Creates list that will become template for number of rows in our table.

temp_in_short <- list.files('~/Downloads/isa_cuke_project_new/imagery/TIFF_FILES', full.names = TRUE)

y = 1

for (i in cuke_table_seq){
  t[y, 1] <- substr(temp_in_short[i], 73, 92)
  y <- y + 1
}

###

# Cut the rasters down to region of interest polygon size ----

setwd('~/Downloads/isa_cuke_project_new/imagery/new_new_masked')

for (i in temp_in_short){
  r <- raster(i)
  cuke_mask_rast <- resample(cuke_mask, r)
  q <- mask(r, cuke_mask_rast)
  a = substr(i, 73, 92) # Modify to your own particular path length. 
  writeRaster(q, filename = a, format = 'GTiff')
}

# OH MY GOD IT WORKED
###

temp_in_masked <- list.files('~/Downloads/isa_cuke_project_new/imagery/new_masked', full.names = TRUE)

# Calculate avg and sd ------
t <- t %>%
  mutate(
    temp_avg = as.double(temp_avg),
    temp_sd = as.double(temp_sd)
  )

###

# Generate average and standard deviation 

y = 1

for (i in temp_in_masked){
  cuke_masked <- raster(i)
  t[y, 2] <-  cellStats(cuke_masked, stat = 'mean')
  t[y, 3] <-  cellStats(cuke_masked, stat = 'sd')
  y <- y + 1
}

t_2 <- t %>% 
  mutate(
    date = ymd(date),
    day_of_year = yday(date),
         year = year(date),
         year = as.character(year)) %>%
  drop_na() # drops 30 rows

# Averages

t_fin <- t_2 %>%
  group_by(day_of_year) %>%
  summarize(mean_temp = mean(temp_avg),
            sd = sd(temp_avg))

# Split data into 2010-2020 and 2021 ----

t_2021 <- t_2 %>% 
  filter(year == '2021') %>% 
  mutate(mean_temp = temp_avg)

t_n_2021 <- t_2 %>% 
  filter(year != '2021')

# Generate a mean and sd for each date across the 10 years
t_n_fin <- t_n_2021 %>%
  group_by(day_of_year) %>%
  summarize(sd = sd(temp_avg),
            mean_temp = mean(temp_avg))

### MAKE SOME PLOTS -----

ggplot(t_n_fin, aes(x = day_of_year, y = mean_temp)) + 
  geom_point(colour = 'salmon2') +
  geom_pointrange(aes(ymin = mean_temp - sd, ymax = mean_temp + sd), color = 'slateblue') +
  geom_line(data = t_n_fin, colour = 'slateblue') +
  geom_point(data = t_2021, colour = 'salmon4') +
  geom_line(data = t_2021, colour = 'salmon2') +
  ggtitle('2021 average SST time series compared to 2010-2020 composite') +
  xlab('Day of year') +
  ylab('Average temperature - degrees Celsius')

#write_csv(t_2021, file = "2021_SST_data.csv")
#write_csv(t_n_fin, file = "2010_2020_SST_avg_data.csv")
#write_csv(t_n_2021, file = "2010_2020_SST_data.csv")


# DECLAN SHOULD JUST SKIP TO HERE ----

# Load packages and set theme
library(ggplot2)
library(tidyverse)

theme_set(theme_classic()) # set ggplot theme

# Load data
t_n_fin <- read_csv(here("data/Em_SSTdata/2010_2020_SST_avg_data.csv"))
t_2021 <- read_csv(here("data/Em_SSTdata/2021_SST_data.csv"))
t_n_2021 <- read_csv(here("data/Em_SSTdata/2010_2020_SST_data.csv"))

# Add date column to the t_n_2021 data frame
t_n_fin <- t_n_fin %>%
  mutate(date = as.Date(day_of_year, origin = "2020-12-31"))

# Horizontal labelling bars use mapping contained in the following dataframe,
# which has dates carreid over from t_n_fin
t_n_fin <- t_n_fin %>%
  mutate(line1 = 12.4,
         line2 = 16.6,
         line3 = 21.7)
# remove some of the hline values so that the horizontal lines do not cross the
# width of the graph.
t_n_fin$line1[1:15] <- NA
t_n_fin$line2[1:31] <- NA
t_n_fin$line3[1:26] <- NA

# Graph
Baynes_SST <- ggplot() +
  geom_ribbon(data = t_n_fin,
              aes(x = date,
                  ymin = mean_temp - sd,
                  ymax = mean_temp + sd),
              fill = "grey70") +
  geom_line(data = t_n_fin,
            aes(x = date,
                y = mean_temp),
            colour = "black") +
  geom_ribbon(data = t_2021,
              aes(x = date,
                  ymin = mean_temp - temp_sd,
                  ymax = mean_temp + temp_sd),
              fill = "black",
              alpha = 0.5) +
  geom_line(data = t_n_fin,
            aes(x = date, y = line1,
                colour = line1),
            size = 1) +
  geom_line(data = t_n_fin,
            aes(x = date, y = line2,
                colour = line2),
            size = 1) +
  geom_line(data = t_n_fin,
            aes(x = date, y = line3,
                colour = line3),
            size = 1) +
  geom_line(data = t_2021,
            aes(x = date,
                y = mean_temp,
                colour = mean_temp),
            linejoin = "bevel",
            size = 1.7) +
  scale_colour_gradient2(low = "dodgerblue1",
                       mid = "lightyellow2",
                       high = "orangered1",
                       midpoint = 12) +
  # geom_hline(yintercept = 13, color = "black", size = 1, alpha = 0.8) +
  labs(x = "Date",
       y = "Average Temperature (ºC)") +
  ylim(5,24) +
  theme_classic() +
  theme(legend.position = "none")

Baynes_SST

ggsave("Baynes_SST.pdf",
       Baynes_SST,
       device = "pdf",
       path = here("figures"))
