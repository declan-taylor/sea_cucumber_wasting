library(here)
library(tidyverse)
library(Hmisc)
library(ordinal)
library(chron)

behav = read_csv("data/BehaviourData.csv") %>%
  mutate(
         Treatment=fct_relevel(Treatment, c("Control","Room","Heat")),
         Bucket_ID = as.factor(Bucket_ID),
         Cuke_ID=as.factor(Cuke_ID),
         Unique_ID=paste(Bucket_ID, Cuke_ID,  sep = '_'))

size = read_csv("data/SizeData.csv") %>%
  mutate(
         Bucket_ID = as.factor(Bucket_ID),
         Cuke_ID=as.factor(Cuke_ID),
         Unique_ID=paste(Bucket_ID, Cuke_ID,  sep = '_'))%>%
  select(-c(Bucket_ID, Length_cm, Dead_Weight, Pooping, Origin, ...9, Cuke_ID))


combined = behav %>%
  left_join(size,  by="Unique_ID", copy=FALSE) %>%
  mutate(Mean_Weight = (Weight_g + Weight_2)/2,
         DateTime = chron(dates=Date, times=Time, 
                          format=c('Y-m-d', 'h:m')))%>%
  relocate(Unique_ID)


str(combined)
