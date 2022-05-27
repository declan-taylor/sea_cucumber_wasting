library(here)
library(gamlss)
library(fitdistrplus)
library(tidyverse)


Act = read_csv("data/BehaviourData.csv") %>%
  mutate(Date = as.Date(Date, format="%d/%m/%Y"),
         Treatment=fct_relevel(Treatment, c("Control","Room","Heat")),
         Bucket_ID = as.factor(Bucket_ID),
         Cuke_ID=as.factor(Cuke_ID),
         Unique_ID=paste(Bucket_ID, Cuke_ID,  sep = '_')) %>%
  select(-c("Droop_score", "Squeeze_score", "Number_lesions", "Bodywall_lesions"))%>%
  na.omit()

str(Act)

Activity_plot = 
  ggplot(data =Act, aes(x=as.factor(Date), y=Activity_Score, fill=Treatment))+
  geom_boxplot(color="black") +
  scale_x_discrete(breaks=c("2021-11-09","2021-11-10","2021-11-11", "2021-11-12","2021-11-13","2021-11-15"),
                   labels=c("Nov 09", "Nov 10", "Nov 11", "Nov 12", "Nov 13", "Nov15"))+
  geom_text(label="Heat Treatment", x=3, y=14.4)+
  geom_segment(aes(x = 1.5, y = -1, xend = 1.5, yend = 15), size=1,linetype=2)+
  geom_segment(aes(x = 4.5, y = -1, xend = 4.5, yend = 15), size=1, linetype=2)+
  geom_segment(aes(x = 1.6, y = 13.7, xend = 4.4, yend = 13.7), size=1,arrow = arrow(length = unit(0.2, "cm")))+
  scale_y_continuous(expand=c(0,0), limits = c(-1,15))+
  scale_fill_manual(labels=c("Control (12?C)","Room (17?C)","Heat (22?C)"), values=c("Gold", "Orange","Red"))+
  ylab("Activity Scores")+xlab("Date")+
  theme_bw()+
  theme(panel.grid=element_blank())
Activity_plot

ggsave("figures/ActivityScores.jpg",plot=Activity_plot, width=7, height=4)


hist(Act$Activity_Score,col="firebrick")
shapiro.test(Act$Activity_Score)

