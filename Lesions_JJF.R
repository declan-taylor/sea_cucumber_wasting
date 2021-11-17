library(here)
library(tidyverse)
library(Hmisc)
library(ordinal)

lesion = read_csv("data/BehaviourData.csv") %>%
  mutate(Date = as.Date(Date, format="%d/%m/%Y"),
         Treatment=fct_relevel(Treatment, c("Control","Room","Heat")),
         Bucket_ID = as.factor(Bucket_ID),
         Cuke_ID=as.factor(Cuke_ID),
         Unique_ID=paste(Bucket_ID, Cuke_ID,  sep = '_'))%>%
  select(-c("Activity_Score", "Droop_score", "Bodywall_lesions", "Squeeze_score"))%>%
  na.omit()

Lesions_plot = 
  ggplot(data =lesion, aes(x=as.factor(Date), y=Number_lesions, fill=Treatment))+
  geom_boxplot(color="black") +
  scale_x_discrete(breaks=c("2021-11-13","2021-11-14","2021-11-15"),
                   labels=c("Nov 13","Nov14", "Nov15"))+
  scale_y_continuous(expand=c(0,0), limits = c(-1,15))+
  scale_fill_manual(labels=c("Control (12°C)","Room (17°C)","Heat (22°C)"), values=c("Gold", "Orange","Red"))+
  ylab("Number of Lesions")+xlab("Date")+
  theme_bw()+
  theme(panel.grid=element_blank())
Lesions_plot

ggsave("figures/ActivityScores.jpg",plot=Activity_plot, width=6, height=4)

