library(here)
library(FSA)
library(tidyverse)
library(ordinal)

behav = read_csv("data/BehaviourData_Final.csv") %>% # formatting the behaviour data frame
  mutate(
         Treatment=fct_relevel(Treatment, c("Control","Room","Heat")),
         Bucket_ID = as.factor(Bucket_ID),
         Cuke_ID=as.factor(Cuke_ID),
         Unique_ID=paste(Bucket_ID, Cuke_ID,  sep = '_'), 
         Table_ID = paste(Sea_Table, Table_Position, sep='_'), 
         Date = as.Date(Date, format="%d-%m-%Y")) %>%
  dplyr:: select(-c(Sea_Table, Table_Position))


str(behav) # checking to be sure that our data is looking solid


# Starting to run models on the data

####1) Activity score model ####
 
Act = behav %>% 
  dplyr::select(-c(Number_lesions, 'Bodywall lesions')) %>%
  na.omit()

# running kruskal-wallis tests to look for overall differences in treatments
kruskal.test(Activity_Score ~ Treatment, data=subset(Act, Date=="2021-11-09"))
kruskal.test(Activity_Score ~ Treatment, data=subset(Act, Date=="2021-11-10"))
kruskal.test(Activity_Score ~ Treatment, data=subset(Act, Date=="2021-11-13"))
kruskal.test(Activity_Score ~ Treatment, data=subset(Act, Date=="2021-11-20"))


# running Dunn tests to compare treatments on specific dates
dunnTest(Activity_Score ~ Treatment, data=subset(Act, Date=="2021-11-09")) 
dunnTest(Activity_Score ~ Treatment, data=subset(Act, Date=="2021-11-10"))
dunnTest(Activity_Score ~ Treatment, data=subset(Act, Date=="2021-11-13"))
dunnTest(Activity_Score ~ Treatment, data=subset(Act, Date=="2021-11-20"))



library(gamlss)

# finding the distribution that best fits our response variable (activity score) 
fitDist(Activity_Score, data=Act, type="counts", try.gamlss=TRUE)
histDist(Act$Activity_Score, "ZANBI", density=T) # BEST FIT FOR DATA
# zero adjusted negative binomial distribution is bomber


# only include dates from Nov 9 to Nov 13
# aka the day before, days during, and day after the heat treatment
Act_heat = subset(Act, Date < "2021-11-15")

str(Act_heat)

Act_Full = gamlss(Activity_Score ~  Treatment + as.factor(Date) + Treatment*as.factor(Date) 
                  + random(as.factor(Unique_ID)) + random(as.factor(Table_ID)) + random(as.factor(Bucket_ID)),
                  family = ZANBI(), data = Act_heat)
summary(Act_Full)

step.Act.backward <- stepGAIC(Act_Full, 
                                direction = "backward", trace = F)

?stepGAIC()

formula(step.Act.backward) # get the final formula
summary(step.Act.backward)

# plotting activity scores over time
Activity_plot = 
  ggplot(data =Act, aes(x=as.factor(Date), y=Activity_Score, fill=Treatment))+
  geom_boxplot(color="black") +
  scale_x_discrete(breaks=c("2021-11-09", "2021-11-10","2021-11-11","2021-11-12","2021-11-13","2021-11-15","2021-11-20"),
                   labels=c(1,2,3,4,5,7,12))+
  #geom_text(label="Heat Treatment", x=3, y=14.4)+
  geom_segment(aes(x = 1.5, y = -1, xend = 1.5, yend = 15), size=1,linetype=2)+
  geom_segment(aes(x = 4.5, y = -1, xend = 4.5, yend = 15), size=1, linetype=2)+
  #geom_segment(aes(x = 1.6, y = 13.7, xend = 4.4, yend = 13.7), size=1,arrow = arrow(length = unit(0.2, "cm")))+
  scale_y_continuous(expand=c(0,0), limits = c(-1,15))+
  scale_fill_manual(labels=c("12?C","17?C","22?C"), values=c("Gold", "Orange","Red"))+
  ylab("Activity Scores")+xlab("Day")+
  theme_bw()+
  theme(panel.grid=element_blank())
Activity_plot

ggsave("figures/activity.jpg",plot=Activity_plot, width=6, height=4)


########## SEA CUCUMBER STIFFNESS ##################

stiff = behav %>%
  dplyr::select(-c(Activity_Score, Number_lesions, 'Bodywall lesions')) %>%
  na.omit()

## PLOTTING CORRELATION BETWEEN SQUEEZE AND DROOP #

# making vector with facet labels
facet.labs=c("12°C", "17°C", "22°C")
names(facet.labs)=c("Control", "Room", "Heat")

squeeze_droop_cor = ggplot(data=stiff, aes(x=Squeeze_score, y=Droop_score, color=Treatment))+
  geom_jitter(width=0.1, height=0.1, alpha=0.6)+
  scale_color_manual(labels=c("Control (12?C)","Room (17?C)","Heat (22?C)"), values=c("Gold", "Orange","Red"))+
  theme_bw()+ 
  scale_x_continuous(breaks=seq(0,2.3,1), limits = c(-0.2,2.3))+
  scale_y_continuous(breaks=seq(0,2.3,1), limits = c(-0.2,2.3))+
  xlab("Antipredator Defense") + ylab("Structural Maintenance")+
  theme(strip.text.y = element_text(size =12),
        legend.position="NONE")+
  facet_grid(Treatment~., labeller=labeller(Treatment = facet.labs))
squeeze_droop_cor

ggsave("figures/squeeze_droop_correlation.jpg",plot=squeeze_droop_cor, width=6, height=4)

## CALCULATING CORRELATION 
Hmisc::rcorr(stiff$Squeeze_score,stiff$Droop_score, type="spearman") # looking at correlation between droop and squeeze


## PLOTTING SQUEEZE AND DROOP
str(stiff)

stiff_plot = stiff %>%
  mutate(Squeeze_score = as.factor(Squeeze_score),
         Droop_score = as.factor(Droop_score))

squeeze_plot = 
  ggplot(data =stiff_plot, aes(x=as.factor(Date), fill=Squeeze_score))+
  geom_bar(alpha=0.8, color="black", size=0.5) +
  scale_x_discrete(breaks=c("2021-11-09","2021-11-10","2021-11-11", "2021-11-12","2021-11-13","2021-11-14","2021-11-15", "2021-11-20"),
                   labels=c(1,2,3,4,5,6,7,12))+
  xlab("Experiment Day") + ylab("Number of Sea Cucumbers")+
  geom_vline(xintercept=1.5, linetype=2, size=1)+ geom_vline(xintercept=4.5, linetype=2, size=1)+
  scale_y_continuous(expand=c(0,0))+
  scale_fill_manual(name="Antipredator Stiffness",labels=c("0 - No Stiffness","1 - Partial Stiffness","2 - Full Stiffness"), 
                    values=c("burlywood4", "tan", "beige"))+ 
  theme_bw()+
  theme(strip.text.y = element_text(size =12),
        panel.grid=element_blank(),
        legend.position="right")+
  facet_grid(Treatment~.,labeller=labeller(Treatment = facet.labs))
squeeze_plot


droop_plot = 
  ggplot(data =stiff_plot, aes(x=as.factor(Date), fill=Droop_score))+
  geom_bar(alpha=0.8, color="black", size=0.5) +
  scale_x_discrete(breaks=c("2021-11-09","2021-11-10","2021-11-11", "2021-11-12","2021-11-13","2021-11-14","2021-11-15", "2021-11-20"),
                   labels=c(1,2,3,4,5,6,7,12))+
  xlab("Experiment Day") + ylab("Number of Sea Cucumbers")+
  geom_vline(xintercept=1.5, linetype=2, size=1)+ geom_vline(xintercept=4.5, linetype=2, size=1)+
  scale_y_continuous(expand=c(0,0))+
  scale_fill_manual(name="Structural Stiffness",labels=c("0 - No Stiffness","1 - Partial Stiffness","2 - Full Stiffness"), 
                    values=c("burlywood4", "tan", "beige"))+ 
  theme_bw()+
  theme(strip.text.y = element_text(size =12),
        panel.grid=element_blank(),
        legend.position="right")+
  facet_grid(Treatment~.,labeller=labeller(Treatment = facet.labs))
droop_plot

#### 2) Squeeze and Droop Test Models ####
stiff_heat = subset(stiff, Date < "2021-11-14")

Squeeze_Full = clmm(as.factor(Squeeze_score)~ Treatment + as.factor(Date) + # Treatment*as.factor(Date) + 
                   (1|Unique_ID) + (1|Bucket_ID) + (1|Table_ID), na.action="na.fail",
                 data = stiff_heat) # squeeze
summary(Squeeze_Full)

#### MODEL SELECTION
#library(MuMIn)
    # stepGAIC and stepAIC don't work on clmm objects, so I used the dredge function
    # it works almost the same as forward or backward selection 
    # it gives a model with NO interaction term 
    # based on the fact it's more than 2 AIC points better than the next best
#dredge.Squeeze <- MuMIn::dredge(Squeeze_Full) # top model includes all variables
#view(dredge.Squeeze)

## now, we run models for the droop score!
Droop_Full = clmm(as.factor(Droop_score)~ Treatment + as.factor(Date)+ # Treatment*as.factor(Date)+
                      (1|Unique_ID) +(1|Bucket_ID)+(1|Table_ID), na.action="na.fail",
                    data = stiff_heat) # droop
summary(Droop_Full)
#dredge.Droop <- MuMIn::dredge(Droop_Full) # top model includes all variables
#view(dredge.Droop)

# First, run models and save model objects (in this case Droop_Full and Squeeze_Full)
# make output data frame with coefficient and confidene intervals
droop_output = data.frame(coef = coef(Droop_Full), # coef
                          upper = confint(Droop_Full)[,2], # upper CI
                          lower = confint(Droop_Full)[,1], # lower CI
                          stiff = "Structural Maintenance")[-c(1,2),]
squeeze_output = data.frame(coef = coef(Squeeze_Full), # coef
                          upper = confint(Squeeze_Full)[,2], # upper CI
                          lower = confint(Squeeze_Full)[,1], # lower CI
                          stiff= "Antipredator Defense")[-c(1,2),]

stiff_output= rbind(droop_output, squeeze_output) # bind the two outputs together for plotting


stiff_output$Treatment=c("Temp:17°C", "Temp:22°C", "Day 2", # naming treatments (for gg)
                         "Day 3", "Day 4", "Day 5")
stiff_output$Variable = c("Temp", "Temp", "Date", "Date", "Date", "Date")# naming variables (for gg)


# plotting the output
ggplot(data=stiff_output, aes(x=coef, y=Variable, color=Treatment))+
  geom_point(position=position_dodge(0.8))+
  geom_errorbar(aes(xmin=lower, xmax=upper), size=1, width=0.4, position=position_dodge(0.8))+
  geom_vline(xintercept=0, linetype=2)+
  xlab("Coefficient (Log Odds) ± 95% Confidence Interval")+
  scale_color_manual("Variable Category",values=c("grey30", "grey45", "grey60", "grey75",
                              "goldenrod", "darkorange4"))+
  facet_grid(stiff~.)+
  theme_bw()







