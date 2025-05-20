#where I will insert plots and graphs
library(ISLR)
library(ggridges)
library(tidyverse)
#ridgeplot for density of vehicle types by time (1)
Motor_crash_vehicle_slim %>% mutate(VEHICLE.TYPE.CODE.1 = fct_reorder(VEHICLE.TYPE.CODE.1,CRASH.DATE,mean)) %>% 
  ggplot() +
  geom_density_ridges(aes(x=CRASH.DATE,y=VEHICLE.TYPE.CODE.1),fill='lightblue')+theme_bw()+
  labs(x = "Date of crash", y = "Vehicle type", title = "Distribution of top 18 NYC vehicle types in crashes")

#plot showing the distribution of people killed (or injured) against crash date, with colour showing number of vehicles faceted by borough
#ggplot(Motor_crash_better) + 
 # geom_point(aes(CRASH.DATE, NUMBER.OF.PERSONS.KILLED, col = NO.VEHICLES),position = 'jitter', alpha = 0.8)+
  #facet_wrap(vars(BOROUGH))

#simplicity of this one works? Only need to show comparison between boroughs (2)
ggplot(filter(Motor_crash_better, NUMBER.OF.PERSONS.KILLED>0))+
  geom_jitter(aes(x=CRASH.DATE, y=NUMBER.OF.PERSONS.INJURED, size = NUMBER.OF.PERSONS.KILLED))+
  facet_wrap(vars(BOROUGH))+
  theme_bw()+
  labs(x="Number of Vehicles", y="Number of injured")

summary(Motor_crash_better$NUMBER.OF.PERSONS.INJURED,Motor_crash_better$BOROUGH)
summary(Motor_crash_better$NUMBER.OF.PERSONS.KILLED,Motor_crash_better$BOROUGH)
#this plot probably better than the geom point one
#does not work for Motor_crash_better, too many points
#ggplot(Motor_crash_better5, aes(x=BOROUGH, y=NUMBER.OF.PERSONS.INJURED))+
 # geom_point(position = 'jitter', alpha = 0.1)+
  #geom_boxplot(alpha=0.7, outlier.shape = NA)

#table(Motor_factor_10$Factor, Motor_factor_10$BOROUGH)

#table(Motor_factor_10$Factor, Motor_factor_10$NUMBER.OF.PERSONS.KILLED, Motor_factor_10$BOROUGH)
#this is a very good one, shows relationship between people injured, factors and number of vehicles (3)
ggplot(Motor_factor_10)+
  geom_jitter(aes(x=NUMBER.OF.PERSONS.INJURED, y=Factor, col = NO.VEHICLES), position = 'jitter')+
  theme_bw()+
  labs(x="Number of people injured", y="Collision Factor", col = "Number of Vehicles")+
  theme(legend.position = "top")

Bus_factor<-filter(Motor_factor_10, Vehicle=="Bus")
table(Bus_factor$Vehicle, Bus_factor$NUMBER.OF.PERSONS.INJURED)
