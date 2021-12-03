library(here)
library(tidyverse)
library(Hmisc)
library(ordinal)
library(gamlss)

lesion = read_csv("data/BehaviourData_Final.csv") %>%
  mutate(
    Treatment=fct_relevel(Treatment, c("Control","Room","Heat")),
    Bucket_ID = as.factor(Bucket_ID),
    Cuke_ID=as.factor(Cuke_ID),
    Unique_ID=paste(Bucket_ID, Cuke_ID,  sep = '_'), 
    Table_ID = paste(Sea_Table, Table_Position, sep='_'), 
    Date = as.Date(Date, format="%d-%m-%Y")) %>%
  dplyr:: select(c(Unique_ID, Bucket_ID, Table_ID, Date, Treatment, Number_lesions)) %>%
  na.omit()


# find max number of lesions per individual
lesion_max = lesion %>%
  group_by(Unique_ID) %>%
  mutate(max_lesions = max(Number_lesions)) %>%
  distinct(Unique_ID, .keep_all=TRUE) %>%
  dplyr::select(-"Number_lesions")

# reading in weight data
size = read_csv("data/SizeData.csv") %>%
  mutate(
    Bucket_ID = as.factor(Bucket_ID),
    Cuke_ID=as.factor(Cuke_ID),
    Unique_ID=paste(Bucket_ID, Cuke_ID,  sep = '_'),
    mean_weight = (Weight_g+Weight_2)/2) %>%
  dplyr::select(c(Unique_ID, mean_weight))

# adding weight data to lesion data
lesion_max = merge(lesion_max, size, by=c("Unique_ID"))
# adding a column stating if a cucumber had lesions
lesion_max$lesion_presence = ifelse(lesion_max$max_lesions==0, "N", "Y")

lesions_prop = lesion_max %>%
  group_by(Treatment) %>%
  count(lesion_presence)


# finding the distribution that best fits our response variable (max_lesions)
gamlss::fitDist(max_lesions, data=lesion_max, type="counts", try.gamlss=TRUE)
gamlss::histDist(lesion_max$max_lesions, "GEOM", density=T) # BEST FIT FOR DATA
# geometric distribution is best (woohoo!)

lesions_Full = gamlss(max_lesions ~ mean_weight + Treatment +
                      random(as.factor(Table_ID)) + random(as.factor(Bucket_ID)),
                      family = GEOM(), data = lesion_max)
summary(lesions_Full)


step.lesions.backward <- stepGAIC(lesions_Full, 
                              direction = "backward", trace = F)
summary(step.lesions.backward)

str(lesion_max)

treatlabs = c("12°C", "17°C", "22°C")
names(treatlabs)=c("Control", "Room", "Heat")



ggplot(data=lesion_max, aes(x=Treatment, y=max_lesions, fill=Treatment))+
  geom_boxplot(outlier.shape= NA, color="black", alpha=0.8)+
  geom_point(alpha=0.5, position=position_dodge2(0.2), color="black")+
  scale_y_continuous(expand=c(0,0), limits = c(-1,13.2))+
  scale_x_discrete(labels=c("12°C", "17°C", "22°C"))+
  scale_fill_manual(values=c("Gold", "Orange","Red"))+
  ylab("Maximum Lesions / Indiv.")+xlab("Treatment")+
  theme_bw()+
  theme(panel.grid=element_blank(), 
        legend.position="none")




bodywall = read_csv("data/BehaviourData_Final.csv") %>%
  mutate(
    Treatment=fct_relevel(Treatment, c("Control","Room","Heat")),
    Bucket_ID = as.factor(Bucket_ID),
    Cuke_ID=as.factor(Cuke_ID),
    Unique_ID=paste(Bucket_ID, Cuke_ID,  sep = '_'), 
    Table_ID = paste(Sea_Table, Table_Position, sep='_'), 
    Date = as.Date(Date, format="%d-%m-%Y")) %>%
  dplyr:: select(c(Unique_ID, Bucket_ID, Table_ID, Date, Treatment, 'Bodywall lesion')) %>%
  na.omit()



