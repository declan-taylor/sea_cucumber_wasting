# making a figure to show changes in cucumber stiffness over time

library(here)
library(tidyverse)
library(Hmisc)
library(ordinal)

# load the stiffness data, rename and format columns
stiff <- read_csv(here("data/BehaviourData.csv")) %>%
  mutate(Droop_score = as.factor(Droop_score),
         Squeeze_score = as.factor(Squeeze_score), 
         Date = as.Date(Date, format="%d/%m/%Y"),
         Treatment=fct_relevel(Treatment, c("Control","Room","Heat")),
         Bucket_ID = as.factor(Bucket_ID),
         Cuke_ID=as.factor(Cuke_ID),
         Unique_ID=paste(Bucket_ID, Cuke_ID,  sep = '_'))%>%
  select(-c("Activity_Score", "Number_lesions","Bodywall_lesions"))%>%
  na.omit()

  str(stiff)
  
#### PLOTTING DROOP DATA
stiffness_plot <-
  ggplot(data =stiff, 
         aes(x=as.factor(Date), 
             fill=Droop_score))+
  geom_bar(alpha=0.8, 
           color="black", 
           size=0.5) +
  scale_x_discrete(breaks=c("2021-11-09","2021-11-10","2021-11-11", "2021-11-12","2021-11-13","2021-11-14","2021-11-15"),
                   labels=c("Nov09", "Nov10", "Nov11", "Nov12", "Nov13","Nov14", "Nov15")) +
  xlab("Day") +
  #scale_x_date(date_labels = "%b%d", date_breaks="1 day")+
  geom_vline(xintercept=1.5, 
             linetype=2) +
  geom_vline(xintercept=4.5, 
             linetype=2) +
  scale_y_continuous(expand=c(0,0)) +
  ylab("# Sea Cucumbers")+
  scale_fill_brewer(name="Droop Score",
                    labels=c("0 - Full Droop","1 - Partial Droop","2 - No Droop"), 
                    palette="OrRd", 
                    direction=1)+
  theme_bw()+
  theme(strip.text.y = element_text(size =12),
        panel.grid=element_blank())+
  facet_grid(Treatment ~ .)

stiffness_plot # show the plot!

ggsave("StiffnessPlot.pdf",
       stiffness_plot, 
       device = "pdf",
       path = here("figures"))

##### PLOTTING SQUEEZE DATA
squeeze_plot = 
  ggplot(data =stiff, aes(x=as.factor(Date), fill=Squeeze_score))+
  geom_bar(alpha=0.8, color="black", size=0.5) +
  scale_x_discrete(breaks=c("2021-11-09","2021-11-10","2021-11-11", "2021-11-12","2021-11-13","2021-11-14","2021-11-15"),
                   labels=c(1,2,3,4,5,6,7))+
  xlab("Day")+
  #scale_x_date(date_labels = "%b%d", date_breaks="1 day")+
  geom_vline(xintercept=1.5, linetype=2)+
  geom_vline(xintercept=4.5, linetype=2)+
  scale_y_continuous(expand=c(0,0))+
  ylab("# Sea Cucumbers")+
  scale_fill_brewer(name="Squeeze Score",labels=c("0 - No Stiffness","1 - Partial Stiffness","2 - Full Stiffness"), 
                    palette="OrRd", direction=1)+ 
  theme_bw()+
  theme(strip.text.y = element_text(size =12),
        panel.grid=element_blank(),
        legend.position="right")+
  facet_grid(Treatment~.)
squeeze_plot
ggsave("figures/squeeze.jpg",plot=squeeze_plot, width=5, height=4)



#### PLOTTING CORRELATION BETWEEN SQUEEZE AND DROOP
squeeze_droop_cor = ggplot(data=stiff, aes(x=Squeeze_score, y=Droop_score, color=Treatment))+
  geom_jitter(width=0.2, height=0.2)+
  scale_color_manual(labels=c("Control (12?C)","Room (17?C)","Heat (22?C)"), values=c("Gold", "Orange","Red"))+
  theme_bw()+
  theme(strip.text.y = element_text(size =12))+
  facet_grid(Treatment~.)
squeeze_droop_cor

ggsave("figures/squeeze_droop_correlation.jpg",plot=squeeze_droop_cor, width=6, height=4)

## CALCULATING CORRELATION 
rcorr(stiff$Squeeze_score,stiff$Droop_score, type="spearman") # looking at correlation 

##### SHATISTICS #####
# Running ordinal regressions on the data
str(stiff)

stiff_mod = clmm(Squeeze_score~ Treatment+as.factor(Date)+ (1|Unique_ID) +(1|Bucket_ID)+(1|Sea_Table),
                 data = stiff) # squeeze
tbl_regression(stiff_mod)



droop_mod= clmm(Droop_score~ Treatment + as.factor(Date)+ (1|Unique_ID) +(1|Bucket_ID)+(1|Sea_Table), data = stiff) # droop
tbl_regression(droop_mod)









