library(here)
library(tidyverse)

# Import the data
SizeData <- read_csv("data/SizeData.csv") %>%
  # Average the first 2 days' weights
  mutate(Weight_g = (Weight_g + Weight_2)/2) %>%
  # Remove the notes and `Weight_2` column
  select(-c("...9", Weight_2))

# ggplot function to compare weight and length
weightLength <- ggplot(SizeData,
                     aes(x = Length_cm,
                         y = Weight_g)) +
  geom_point() +
  labs(x = "Weight (g)",
       y = "Length (cm)") +
  theme_classic()

sizeFigure

