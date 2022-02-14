library(tidyverse)
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
  mutate (Date = date(Time_UTC))

# Generate the figure.
Baynes05_temp_plot = ggplot(Baynes05, aes(x = Date, y = Temperature_C, color = Temperature_C)) +
  geom_line(size = 1.1) +
  scale_color_gradient2(low = "dodgerblue1",
                        mid = "lightyellow2",
                        high = "orangered1",
                        midpoint = 12.35015) + #WH mean temp 12.35015
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

ggsave("Baynes Sound 5mbss Temp.pdf", 
       BinaryPlot,
       device = "pdf",
       path = here("figures"))