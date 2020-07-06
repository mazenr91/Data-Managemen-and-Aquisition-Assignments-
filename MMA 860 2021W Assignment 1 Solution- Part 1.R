library("tidyverse")
library("stringr")
library("dplyr")

?readxl  # used to learn about skip function to omit a number of rows

Weather_Data <- read_excel("C:\\Users\\rifai\\OneDrive\\Queens University\\MMA\\MMA 860\\Assignments\\Assignment 1\\MMA860_Assignment1_Data_vf.xlsx", sheet =3,skip=16)

head(Weather_Data) # see what the data looks like 

?mutate
Weather_Data_Density<-mutate(Weather_Data,AirDensity= (`Stn Press (kPa)` *1000)/(287.05*(`Temp (°C)`+273.15))) # Part a

Weather_Data_Density_WindSpeedms<-mutate(Weather_Data_Density,WindSpeedms= `Wind Spd (km/h)`*0.277778) # Part b

Weather_Data_Density_WindSpeedms_Filtered_By_Speed<-filter(Weather_Data_Density_WindSpeedms,Weather_Data_Density_WindSpeedms$WindSpeedms>4 & Weather_Data_Density_WindSpeedms$WindSpeedms<32) #Part c

Weather_Data_Density_WindSpeedms_Filtered_By_Speed_PowerWatt<-mutate(Weather_Data_Density_WindSpeedms_Filtered_By_Speed,PowerWatt=AirDensity*13273.23*0.5*WindSpeedms^3 *0.35) #Part c

Weather_Data_Density_WindSpeedms_Filtered_By_Speed_PowerMegaWatt<-mutate(Weather_Data_Density_WindSpeedms_Filtered_By_Speed_PowerWatt,PowerMegaWatt=PowerWatt/1000000) #Part c 

Weather_Data_Density_WindSpeedms_Filtered_By_Speed_PowerMegaWatt_Nominal<-mutate(Weather_Data_Density_WindSpeedms_Filtered_By_Speed_PowerMegaWatt,PowerMegaWattNominal= if_else(PowerMegaWatt>4,4,PowerMegaWatt)) #Part c

Total_Power_Produced<-sum(Weather_Data_Density_WindSpeedms_Filtered_By_Speed_PowerMegaWatt_Nominal$PowerMegaWattNominal)#Part d
Total_Power_Produced_All_Stations<-Total_Power_Produced*49 #Part d, Final Answer:43077.82

Groupbyday<-group_by(Weather_Data_Density_WindSpeedms_Filtered_By_Speed_PowerMegaWatt_Nominal,Day)
Groupbyday_Summary<- summarize(Groupbyday, sum_PowerMegaWattNominal= sum(PowerMegaWattNominal)*49)

my_graph<-ggplot(Groupbyday_Summary,aes(y=sum_PowerMegaWattNominal,x=Day))+
  geom_line(linetype="solid", color="dark blue", size=1.2) +  geom_point(shape=10, color="red", size=3.5)+
  theme(axis.text.x = element_text(angle = 0, vjust = 0.3)) + 
  scale_x_continuous("Day", 
  labels = as.character(Groupbyday_Summary$Day), breaks = Groupbyday_Summary$Day)+
  
   labs(title = "Generated Power Per Day in January", 
       y = "Generated Power",x="Day")+ theme(plot.title = element_text(hjust = 0.5)) 
  
my_graph

# Part 2 

Groupbyday<-group_by(Weather_Data_Density_WindSpeedms_Filtered_By_Speed_PowerMegaWatt_Nominal,Day)
Groupbyday_Summary<- summarize(Groupbyday, sum_PowerMegaWattNominal= sum(PowerMegaWattNominal))

my_graph<-ggplot(Groupbyday_Summary,aes(y=sum_PowerMegaWattNominal,x=Day))+
  geom_line(linetype="solid", color="Orange", size=1) + 
  theme(axis.text.x = element_text(angle = 0, vjust = 0.3)) + 
  scale_x_continuous("Day", 
                     labels = as.character(Groupbyday_Summary$Day), breaks = Groupbyday_Summary$Day)+
  
  labs(title = "Generated Power Per Day Per Turbine in January", 
       y = "Generated Power",x="Day")+ theme(plot.title = element_text(hjust = 0.5)) 

my_graph

# Average Temperature as a function per day 
Groupbyday<-group_by(Weather_Data_Density_WindSpeedms_Filtered_By_Speed_PowerMegaWatt_Nominal,Day)
Groupbyday_Summary<- summarize(Groupbyday, mean_TempPerDay= mean(`Temp (°C)`))

my_graph<-ggplot(Groupbyday_Summary,aes(y=mean_TempPerDay,x=Day))+
  geom_line(linetype="solid", color="blue", size=1) + 
  theme(axis.text.x = element_text(angle = 0, vjust = 0.3)) + 
  scale_x_continuous("Day", 
                     labels = as.character(Groupbyday_Summary$Day), breaks = Groupbyday_Summary$Day)+
  
  labs(title = "Daily Average Temperature in January", 
       y = "Temperature",x="Day")+ theme(plot.title = element_text(hjust = 0.5)) 

my_graph

# Average Wind Speed as a function per day 
Groupbyday<-group_by(Weather_Data_Density_WindSpeedms_Filtered_By_Speed_PowerMegaWatt_Nominal,Day)
Groupbyday_Summary<- summarize(Groupbyday, mean_WindSpeed= mean(WindSpeedms))

my_graph<-ggplot(Groupbyday_Summary,aes(y=mean_WindSpeed,x=Day))+
  geom_line(linetype="solid", color="Green", size=1) + 
  theme(axis.text.x = element_text(angle = 0, vjust = 0.3)) + 
  scale_x_continuous("Day", 
                     labels = as.character(Groupbyday_Summary$Day), breaks = Groupbyday_Summary$Day)+
  
  labs(title = "Average Daily Windspeed in January", 
       y = "Wind Speed",x="Day")+ theme(plot.title = element_text(hjust = 0.5)) 

my_graph


#Windy VS Non Windy days

Groupbyday<-group_by(Weather_Data_Density_WindSpeedms_Filtered_By_Speed_PowerMegaWatt_Nominal,Day)
Groupbyday_Summary<- summarize(Groupbyday, mean_WindSpeed= mean(WindSpeedms))

count(Groupbyday_Summary$mean_WindSpeed>7 )
sum(Groupbyday_Summary$mean_WindSpeed > 6)
sum(Groupbyday_Summary$mean_WindSpeed <= 6)

Windy_Days=sum(Groupbyday_Summary$mean_WindSpeed > 6)
Non_Windy_Days=sum(Groupbyday_Summary$mean_WindSpeed <= 6)

test=c("Windy_Days","Non_Windy_Day")
hist(c)

43,077.82