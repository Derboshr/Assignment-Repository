library(readr)
library(tidyverse)
setwd("C:\\Users\\riley\\OneDrive\\Desktop\\University\\2025 Autumn\\STAT373\\Assignment-Repository")
#find out how to do relative path for setwd
unzip("Motor_Vehicle_Collisions_-_Crashes.zip", "Motor_Vehicle_Collisions_-_Crashes.csv")
Motor_Vehicle_Collisions_Crashes<-read.csv("Motor_Vehicle_Collisions_-_Crashes.csv")
View(Motor_Vehicle_Collisions_Crashes)

Motor_crash_better <- filter(Motor_Vehicle_Collisions_Crashes, VEHICLE.TYPE.CODE.1 !="" & VEHICLE.TYPE.CODE.2 !=""
                              | VEHICLE.TYPE.CODE.1 !="" & VEHICLE.TYPE.CODE.2 !="" & VEHICLE.TYPE.CODE.3 !=""
                              | VEHICLE.TYPE.CODE.1 !="" & VEHICLE.TYPE.CODE.2 !="" & VEHICLE.TYPE.CODE.3 !="" & VEHICLE.TYPE.CODE.4 !=""
                              | VEHICLE.TYPE.CODE.1 !="" & VEHICLE.TYPE.CODE.2 !="" & VEHICLE.TYPE.CODE.4 !="" & VEHICLE.TYPE.CODE.3 !="" & VEHICLE.TYPE.CODE.5 !="")
#data frame that only includes entries with full vehicle lists
Motor_crash_better <- select(Motor_crash_better, CRASH.DATE, BOROUGH, NUMBER.OF.PERSONS.INJURED, NUMBER.OF.PERSONS.KILLED, VEHICLE.TYPE.CODE.1, VEHICLE.TYPE.CODE.2, VEHICLE.TYPE.CODE.3, VEHICLE.TYPE.CODE.4, VEHICLE.TYPE.CODE.5)

Motor_crash_better <- Motor_crash_better %>%
  mutate(NO.VEHICLES= ifelse(VEHICLE.TYPE.CODE.5 != "", 5, ifelse(VEHICLE.TYPE.CODE.4 != "", 4, 
                                                                  ifelse(VEHICLE.TYPE.CODE.3 != "", 3, 
                                                                         ifelse(VEHICLE.TYPE.CODE.2 != "", 2, 1)))))
#creating a new variable that shows how many vehicles where in each accident

Motor_crash_better <- filter(Motor_crash_better, BOROUGH!= "")
Motor_crash_better$CRASH.DATE <- as.Date(Motor_crash_better$CRASH.DATE, "%m/%d/%Y")
Motor_crash_better2 <- filter(Motor_crash_better, NO.VEHICLES ==2)
Motor_crash_better2 <- slice_min(Motor_crash_better, order_by = NO.VEHICLES)
Motor_crash_better3 <- filter(Motor_crash_better, NO.VEHICLES ==3)
Motor_crash_better4 <- filter(Motor_crash_better, NO.VEHICLES ==4)
Motor_crash_better5 <- filter(Motor_crash_better, NO.VEHICLES ==5)


plot(NUMBER.OF.PERSONS.KILLED ~ CRASH.DATE, Motor_crash_better5, xaxt = "n", main = "5 car collisions")

summary(Motor_crash_better5$NUMBER.OF.PERSONS.KILLED)
summary(Motor_crash_better2$NUMBER.OF.PERSONS.KILLED)

table(Motor_crash_better5$NUMBER.OF.PERSONS.KILLED)
table(Motor_crash_better2$NUMBER.OF.PERSONS.KILLED)

table(Motor_crash_better2$NUMBER.OF.PERSONS.INJURED)

plot(NUMBER.OF.PERSONS.INJURED ~ CRASH.DATE, Motor_crash_better5)

library(ISLR)
ggplot(Motor_crash_better, aes(CRASH.DATE, NUMBER.OF.PERSONS.INJURED))
ggplot(Motor_crash_better) + 
  geom_point(aes(CRASH.DATE, NUMBER.OF.PERSONS.KILLED, col = NO.VEHICLES), position = 'jitter', alpha = 0.1)

table(Motor_crash_better$NO.VEHICLES)

ggplot(Motor_crash_better5) + 
  geom_point(aes(CRASH.DATE, NUMBER.OF.PERSONS.KILLED, col = BOROUGH))
          
ggplot(Motor_crash_better2) + 
  geom_point(aes(CRASH.DATE, NUMBER.OF.PERSONS.KILLED, col = BOROUGH))

table(Motor_crash_better$VEHICLE.TYPE.CODE.1)
#want to make this data more useful
vehicle1 <- pull(Motor_crash_better, var = VEHICLE.TYPE.CODE.1)

view(vehicle1)
table(vehicle1)

summary(Motor_Vehicle_Collisions_Crashes$VEHICLE.TYPE.CODE.1)

summary(vehicle1)

vehicle2_sort <- sort(table(vehicle1), decreasing = TRUE)

view(vehicle2_sort)

vehicle2_sort[2]

vehicle2_sort[,1]

vehicle2_sort["vehicle1"]

cool<-sort(table(Motor_crash_better$VEHICLE.TYPE.CODE.1), decreasing = TRUE)
view(cool)
vehicle_names <- names(vehicle2_sort)
view(vehicle_names)

vehiclesorted <- tibble(vehicle_names, vehicle2_sort)
#making slice able to be used

vehiclesorted <- slice_head(vehiclesorted, n = 50)
#combining cases where the names are the same
Motor_crash_better$VEHICLE.TYPE.CODE.1[Motor_crash_better$VEHICLE.TYPE.CODE.1 == "TAXI"] = "Taxi"
Motor_crash_better$VEHICLE.TYPE.CODE.2[Motor_crash_better$VEHICLE.TYPE.CODE.2 == "TAXI"] = "Taxi"
Motor_crash_better$VEHICLE.TYPE.CODE.3[Motor_crash_better$VEHICLE.TYPE.CODE.3 == "TAXI"] = "Taxi"
Motor_crash_better$VEHICLE.TYPE.CODE.4[Motor_crash_better$VEHICLE.TYPE.CODE.4 == "TAXI"] = "Taxi"
Motor_crash_better$VEHICLE.TYPE.CODE.5[Motor_crash_better$VEHICLE.TYPE.CODE.5 == "TAXI"] = "Taxi"

Motor_crash_better$VEHICLE.TYPE.CODE.1[Motor_crash_better$VEHICLE.TYPE.CODE.1 == "SPORT UTILITY / STATION WAGON"] = "Station Wagon/Sport Utility Vehicle"
Motor_crash_better$VEHICLE.TYPE.CODE.2[Motor_crash_better$VEHICLE.TYPE.CODE.2 == "SPORT UTILITY / STATION WAGON"] = "Station Wagon/Sport Utility Vehicle"
Motor_crash_better$VEHICLE.TYPE.CODE.3[Motor_crash_better$VEHICLE.TYPE.CODE.3 == "SPORT UTILITY / STATION WAGON"] = "Station Wagon/Sport Utility Vehicle"
Motor_crash_better$VEHICLE.TYPE.CODE.4[Motor_crash_better$VEHICLE.TYPE.CODE.4 == "SPORT UTILITY / STATION WAGON"] = "Station Wagon/Sport Utility Vehicle"
Motor_crash_better$VEHICLE.TYPE.CODE.5[Motor_crash_better$VEHICLE.TYPE.CODE.5 == "SPORT UTILITY / STATION WAGON"] = "Station Wagon/Sport Utility Vehicle"

Motor_crash_better$VEHICLE.TYPE.CODE.1[Motor_crash_better$VEHICLE.TYPE.CODE.1 == "BUS"] = "Bus"
Motor_crash_better$VEHICLE.TYPE.CODE.2[Motor_crash_better$VEHICLE.TYPE.CODE.2 == "BUS"] = "Bus"
Motor_crash_better$VEHICLE.TYPE.CODE.3[Motor_crash_better$VEHICLE.TYPE.CODE.3 == "BUS"] = "Bus"
Motor_crash_better$VEHICLE.TYPE.CODE.4[Motor_crash_better$VEHICLE.TYPE.CODE.4 == "BUS"] = "Bus"
Motor_crash_better$VEHICLE.TYPE.CODE.5[Motor_crash_better$VEHICLE.TYPE.CODE.5 == "BUS"] = "Bus"

Motor_crash_better$VEHICLE.TYPE.CODE.1[Motor_crash_better$VEHICLE.TYPE.CODE.1 == "4 dr sedan"] = "Sedan"
Motor_crash_better$VEHICLE.TYPE.CODE.2[Motor_crash_better$VEHICLE.TYPE.CODE.2 == "4 dr sedan"] = "Sedan"
Motor_crash_better$VEHICLE.TYPE.CODE.3[Motor_crash_better$VEHICLE.TYPE.CODE.3 == "4 dr sedan"] = "Sedan"
Motor_crash_better$VEHICLE.TYPE.CODE.4[Motor_crash_better$VEHICLE.TYPE.CODE.4 == "4 dr sedan"] = "Sedan"
Motor_crash_better$VEHICLE.TYPE.CODE.5[Motor_crash_better$VEHICLE.TYPE.CODE.5 == "4 dr sedan"] = "Sedan"

Motor_crash_better$VEHICLE.TYPE.CODE.1[Motor_crash_better$VEHICLE.TYPE.CODE.1 == "MOTORCYCLE"] = "Motorcycle"
Motor_crash_better$VEHICLE.TYPE.CODE.2[Motor_crash_better$VEHICLE.TYPE.CODE.2 == "MOTORCYCLE"] = "Motorcycle"
Motor_crash_better$VEHICLE.TYPE.CODE.3[Motor_crash_better$VEHICLE.TYPE.CODE.3 == "MOTORCYCLE"] = "Motorcycle"
Motor_crash_better$VEHICLE.TYPE.CODE.4[Motor_crash_better$VEHICLE.TYPE.CODE.4 == "MOTORCYCLE"] = "Motorcycle"
Motor_crash_better$VEHICLE.TYPE.CODE.5[Motor_crash_better$VEHICLE.TYPE.CODE.5 == "MOTORCYCLE"] = "Motorcycle"

Motor_crash_better$VEHICLE.TYPE.CODE.1[Motor_crash_better$VEHICLE.TYPE.CODE.1 == "AMBULANCE"] = "Ambulance"
Motor_crash_better$VEHICLE.TYPE.CODE.2[Motor_crash_better$VEHICLE.TYPE.CODE.2 == "AMBULANCE"] = "Ambulance"
Motor_crash_better$VEHICLE.TYPE.CODE.3[Motor_crash_better$VEHICLE.TYPE.CODE.3 == "AMBULANCE"] = "Ambulance"
Motor_crash_better$VEHICLE.TYPE.CODE.4[Motor_crash_better$VEHICLE.TYPE.CODE.4 == "AMBULANCE"] = "Ambulance"
Motor_crash_better$VEHICLE.TYPE.CODE.5[Motor_crash_better$VEHICLE.TYPE.CODE.5 == "AMBULANCE"] = "Ambulance"

Motor_crash_better$VEHICLE.TYPE.CODE.1[Motor_crash_better$VEHICLE.TYPE.CODE.1 == "PICK-UP TRUCK"] = "Pick-up Truck"
Motor_crash_better$VEHICLE.TYPE.CODE.1[Motor_crash_better$VEHICLE.TYPE.CODE.2 == "PICK-UP TRUCK"] = "Pick-up Truck"
Motor_crash_better$VEHICLE.TYPE.CODE.1[Motor_crash_better$VEHICLE.TYPE.CODE.3 == "PICK-UP TRUCK"] = "Pick-up Truck"
Motor_crash_better$VEHICLE.TYPE.CODE.1[Motor_crash_better$VEHICLE.TYPE.CODE.4 == "PICK-UP TRUCK"] = "Pick-up Truck"
Motor_crash_better$VEHICLE.TYPE.CODE.1[Motor_crash_better$VEHICLE.TYPE.CODE.5 == "PICK-UP TRUCK"] = "Pick-up Truck"
#new frame with only top 20 vehicles
Motor_crash_vehicle_slim <- filter(Motor_crash_better, VEHICLE.TYPE.CODE.1 == "Station Wagon/Sport Utility Vehicle"|VEHICLE.TYPE.CODE.1 =="Sedan"|VEHICLE.TYPE.CODE.1 =="PASSENGER VEHICLE"|VEHICLE.TYPE.CODE.1 =="Taxi"|VEHICLE.TYPE.CODE.1 =="Bus"
                                   |VEHICLE.TYPE.CODE.1 =="VAN"|VEHICLE.TYPE.CODE.1 =="Pick-up Truck"|VEHICLE.TYPE.CODE.1 =="OTHER"|VEHICLE.TYPE.CODE.1 =="Box Truck"|VEHICLE.TYPE.CODE.1 =="SMALL COM VEH(4 TIRES)"|VEHICLE.TYPE.CODE.1 =="LARGE COM VEH(6 OR MORE TIRES)"
                                   |VEHICLE.TYPE.CODE.1 =="LIVERY VEHICLE"|VEHICLE.TYPE.CODE.1 =="Motorcycle"|VEHICLE.TYPE.CODE.1 =="Bike"|VEHICLE.TYPE.CODE.1 =="Van"|VEHICLE.TYPE.CODE.1 =="Ambulance"|VEHICLE.TYPE.CODE.1 =="Tractor Truck Diesel"|VEHICLE.TYPE.CODE.1 =="Dump"|VEHICLE.TYPE.CODE.1 =="Convertible")
                                       
                                       