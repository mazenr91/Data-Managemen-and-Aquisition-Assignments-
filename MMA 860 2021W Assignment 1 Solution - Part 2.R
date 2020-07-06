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
Groupbyday_Summary<- summarize(Groupbyday, sum_PowerMegaWattNominal= sum(PowerMegaWattNominal))

my_graph<-ggplot(Groupbyday_Summary,aes(y=sum_PowerMegaWattNominal,x=Day))+
  geom_line(linetype="solid", color="dark blue", size=1.2) +  geom_point(shape=10, color="red", size=3.5)+
  theme(axis.text.x = element_text(angle = 0, vjust = 0.3)) + 
  scale_x_continuous("Day", 
  labels = as.character(Groupbyday_Summary$Day), breaks = Groupbyday_Summary$Day)+
  
   labs(title = "Generated Power Per Day in January", 
       y = "Generated Power",x="Day")+ theme(plot.title = element_text(hjust = 0.5)) 
  
my_graph



write.csv(Weather_Data_Density_WindSpeedms_Filtered_By_Speed_PowerMegaWatt_Nominal, "test.csv")

install.packages("xlsx")

write.csv(Weather_Data_Density_WindSpeedms_Filtered_By_Speed_PowerMegaWatt_Nominal, "C:/Users/rifai/OneDrive/Desktop/Mazen/filename.csv")


my_graph2<-ggplot(Weather_Data_Density_WindSpeedms_Filtered_By_Speed_PowerMegaWatt_Nominal,aes(y=PowerMegaWattNominal,x=Day))+
  geom_line(linetype="solid", color="dark blue", size=1.2) +  geom_point(shape=10, color="red", size=3.5)+
  theme(axis.text.x = element_text(angle = 0, vjust = 0.3)) + 
  scale_x_continuous("Day", 
                     labels = as.character(Weather_Data_Density_WindSpeedms_Filtered_By_Speed_PowerMegaWatt_Nominal$Day), breaks = Weather_Data_Density_WindSpeedms_Filtered_By_Speed_PowerMegaWatt_Nominal$Day)+
  
  labs(title = "Generated Power Per Turbine Day in January", 
       y = "Power Generated",x="Day")+ theme(plot.title = element_text(hjust = 0.5)) 

my_graph2






43,077.82