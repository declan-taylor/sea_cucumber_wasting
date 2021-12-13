library(here)
library(lubridate)
library(tidyverse)
library(gamlss)
library(gtsummary)
library(mgcv)
library(lme4)

# Import Data
Surv <- read_csv("data/SurvivalData.csv", col_names = TRUE) %>%
  mutate(
         Treatment=fct_relevel(Treatment, c("Control","Room","Heat")),
         Bucket_ID = as.factor(Bucket_ID),
         Cuke_ID=as.factor(Cuke_ID),
         Unique_ID=paste(Bucket_ID, Cuke_ID,  sep = '_'),
         Mean_weight=(Weight_g+Weight_2)/2) %>%
  select(-c(...14, Dead_Weight )) 
str(Surv)



surv_plot = 
  ggplot(data =Surv, aes(x=Treatment, fill=Survival))+
  geom_bar(alpha=0.6, color="black", size=0.5) +
  scale_y_continuous(expand=c(0,0), limits=c(0,20))+
  ylab("# Sea Cucumbers")+
  scale_fill_manual(name="Survival",labels=c("Alive", "Dead"), 
                    values=c("orange", "red"))+
  theme_bw()+
  theme(strip.text.y = element_text(size =12),
        panel.grid=element_blank())
surv_plot # show the plot!

ggsave("figures/survival.jpg",plot=surv_plot, width=5, height=4)


  
### survival model - logistic regression first

heat = subset(Surv, Treatment =="Heat")

str(Surv)

Surv$Survival = as.factor(Surv$Survival)
Surv$Treatment = factor(Surv$Treatment, levels = c("Control", "Room", "Heat"))


str(heat)
heat$Survival = as.factor(heat$Survival)

surv_mod<- glm(Survival ~  Treatment +Pooping + Mean_weight,
               family = binomial(link = "logit"), 
               data = Surv)
summary(surv_mod)

tbl_regression(surv_mod)


## survival model - 

  
