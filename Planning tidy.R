library(readr)
library(tidyverse)
#setwd("\\Assignment-Repository")
#find out how to do relative path for setwd
unzip("Motor_Vehicle_Collisions_-_Crashes.zip", "Motor_Vehicle_Collisions_-_Crashes.csv")
Motor_Vehicle_Collisions_Crashes<-read.csv("Motor_Vehicle_Collisions_-_Crashes.csv")

Motor_crash_better <- filter(Motor_Vehicle_Collisions_Crashes, VEHICLE.TYPE.CODE.1 !="" & VEHICLE.TYPE.CODE.2 !=""
                             | VEHICLE.TYPE.CODE.1 !="" & VEHICLE.TYPE.CODE.2 !="" & VEHICLE.TYPE.CODE.3 !=""
                             | VEHICLE.TYPE.CODE.1 !="" & VEHICLE.TYPE.CODE.2 !="" & VEHICLE.TYPE.CODE.3 !="" & VEHICLE.TYPE.CODE.4 !=""
                             | VEHICLE.TYPE.CODE.1 !="" & VEHICLE.TYPE.CODE.2 !="" & VEHICLE.TYPE.CODE.4 !="" & VEHICLE.TYPE.CODE.3 !="" & VEHICLE.TYPE.CODE.5 !="")
#data frame that only includes entries with full vehicle lists
Motor_crash_better <- select(Motor_crash_better, CRASH.DATE, BOROUGH, NUMBER.OF.PERSONS.INJURED, NUMBER.OF.PERSONS.KILLED, VEHICLE.TYPE.CODE.1, VEHICLE.TYPE.CODE.2, VEHICLE.TYPE.CODE.3, VEHICLE.TYPE.CODE.4, VEHICLE.TYPE.CODE.5,
                             ,COLLISION_ID)
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

#combining vehicle variables that are identical

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



#dataset with factor variables
Motor_crash_factor <- filter(Motor_Vehicle_Collisions_Crashes, VEHICLE.TYPE.CODE.1 !="" & VEHICLE.TYPE.CODE.2 !=""
                             | VEHICLE.TYPE.CODE.1 !="" & VEHICLE.TYPE.CODE.2 !="" & VEHICLE.TYPE.CODE.3 !=""
                             | VEHICLE.TYPE.CODE.1 !="" & VEHICLE.TYPE.CODE.2 !="" & VEHICLE.TYPE.CODE.3 !="" & VEHICLE.TYPE.CODE.4 !=""
                             | VEHICLE.TYPE.CODE.1 !="" & VEHICLE.TYPE.CODE.2 !="" & VEHICLE.TYPE.CODE.4 !="" & VEHICLE.TYPE.CODE.3 !="" & VEHICLE.TYPE.CODE.5 !="")
Motor_crash_factor <- select(Motor_crash_factor, CRASH.DATE, BOROUGH, NUMBER.OF.PERSONS.INJURED, NUMBER.OF.PERSONS.KILLED, VEHICLE.TYPE.CODE.1, VEHICLE.TYPE.CODE.2, VEHICLE.TYPE.CODE.3, VEHICLE.TYPE.CODE.4, VEHICLE.TYPE.CODE.5,
                             CONTRIBUTING.FACTOR.VEHICLE.1, CONTRIBUTING.FACTOR.VEHICLE.2, CONTRIBUTING.FACTOR.VEHICLE.3, CONTRIBUTING.FACTOR.VEHICLE.4, CONTRIBUTING.FACTOR.VEHICLE.5, COLLISION_ID)
Motor_crash_factor <- filter(Motor_crash_factor, BOROUGH!= "")
Motor_crash_factor$CRASH.DATE <- as.Date(Motor_crash_factor$CRASH.DATE, "%m/%d/%Y")
Motor_crash_fvehicle_slim <- filter(Motor_crash_factor, VEHICLE.TYPE.CODE.1 == "Station Wagon/Sport Utility Vehicle"|VEHICLE.TYPE.CODE.1 =="Sedan"|VEHICLE.TYPE.CODE.1 =="PASSENGER VEHICLE"|VEHICLE.TYPE.CODE.1 =="Taxi"|VEHICLE.TYPE.CODE.1 =="Bus"
                                    |VEHICLE.TYPE.CODE.1 =="VAN"|VEHICLE.TYPE.CODE.1 =="Pick-up Truck"|VEHICLE.TYPE.CODE.1 =="OTHER"|VEHICLE.TYPE.CODE.1 =="Box Truck"|VEHICLE.TYPE.CODE.1 =="SMALL COM VEH(4 TIRES)"|VEHICLE.TYPE.CODE.1 =="LARGE COM VEH(6 OR MORE TIRES)"
                                    |VEHICLE.TYPE.CODE.1 =="LIVERY VEHICLE"|VEHICLE.TYPE.CODE.1 =="Motorcycle"|VEHICLE.TYPE.CODE.1 =="Bike"|VEHICLE.TYPE.CODE.1 =="Van"|VEHICLE.TYPE.CODE.1 =="Ambulance"|VEHICLE.TYPE.CODE.1 =="Tractor Truck Diesel"|VEHICLE.TYPE.CODE.1 =="Dump"|VEHICLE.TYPE.CODE.1 =="Convertible")
Motor_crash_fvehicle_slim<- filter(Motor_crash_fvehicle_slim, CONTRIBUTING.FACTOR.VEHICLE.1 !="Unspecified" & CONTRIBUTING.FACTOR.VEHICLE.2 !="Unspecified"
                                   | CONTRIBUTING.FACTOR.VEHICLE.1 !="Unspecified" & CONTRIBUTING.FACTOR.VEHICLE.2 !="Unspecified" & CONTRIBUTING.FACTOR.VEHICLE.3 !="Unspecified"
                                   | CONTRIBUTING.FACTOR.VEHICLE.1 !="Unspecified" & CONTRIBUTING.FACTOR.VEHICLE.2 !="Unspecified" & CONTRIBUTING.FACTOR.VEHICLE.3 !="Unspecified" & CONTRIBUTING.FACTOR.VEHICLE.4 !="Unspecified")

#seeing if pivot is useful at all                                                                      | CONTRIBUTING.FACTOR.VEHICLE.1 !="Unspecified" & CONTRIBUTING.FACTOR.VEHICLE.2 !="Unspecified" & CONTRIBUTING.FACTOR.VEHICLE.3 !="Unspecified" & CONTRIBUTING.FACTOR.VEHICLE.4 !="Unspecified" & CONTRIBUTING.FACTOR.VEHICLE.5 !="Unspecified")
Motor_factor_slim_long<-pivot_longer(Motor_crash_fvehicle_slim, cols =5:9, names_to = "Contributing_Vehicle", values_to = "Vehicle")

#making new variable factor for ease 
Motor_factor_slim_long<- Motor_factor_slim_long %>%
  mutate(Factor = ifelse(Contributing_Vehicle == "VEHICLE.TYPE.CODE.1", CONTRIBUTING.FACTOR.VEHICLE.1,
                         ifelse(Contributing_Vehicle == "VEHICLE.TYPE.CODE.2", CONTRIBUTING.FACTOR.VEHICLE.2, 
                                ifelse(Contributing_Vehicle == "VEHICLE.TYPE.CODE.3", CONTRIBUTING.FACTOR.VEHICLE.3,
                                ifelse(Contributing_Vehicle == "VEHICLE.TYPE.CODE.4", CONTRIBUTING.FACTOR.VEHICLE.4,CONTRIBUTING.FACTOR.VEHICLE.5)))))
#getting rid of old factor variables
Motor_factor_slim_long<- select(Motor_factor_slim_long, -CONTRIBUTING.FACTOR.VEHICLE.1, -CONTRIBUTING.FACTOR.VEHICLE.2, -CONTRIBUTING.FACTOR.VEHICLE.3, -CONTRIBUTING.FACTOR.VEHICLE.4, -CONTRIBUTING.FACTOR.VEHICLE.5)

