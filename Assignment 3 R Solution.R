
library(mice)
library(readxl)

data <- read_excel("C:\\Users\\rifai\\OneDrive\\Queens University\\MMA\\MMA 860\\Assignment 3\\MMA_860_Assignment_3_Data_v1_0.xlsx",sheet=1)

head(data)

summary(data)

reg1 <- lm(Y ~ X1 + X2 + X3 + X4 + X5, data)

summary(reg1) 


md.pattern(data) #this tells us X2 is missing 5 values, X4 is missing 4 values,X5 is missing 2 values, X3 is missing 1 value, X1 is missing 1 value

imputed_data <- mice(data, m=13, maxit=30, meth='pmm', seed =1)
  
completed_data <- complete(imputed_data, 1)
head(completed_data)
summary(completed_data)

reg2 <- with(imputed_data, lm(Y ~ X1 + X2 + X3 + X4 + X5))
summary(pool(reg2))


# Part 2: Wine Question:

wine <- read_excel("C:\\Users\\rifai\\OneDrive\\Queens University\\MMA\\MMA 860\\Assignment 3\\MMA_860_Assignment_3_Data_v1_0.xlsx",sheet=2)

wine2<-mutate(wine,Country_Italy=(ifelse(wine$Country=="Italy",1,0)))
wine3<-mutate(wine2,Country_France=(ifelse(wine$Country=="France",1,0)))
wine4<-mutate(wine3,Country_US=(ifelse(wine$Country=="US",1,0)))


head(wine4)

summary(wine4)

reg1 <- lm(Rating ~ Price + Alcohol + Residual_Sugar + Sulphates + pH + Country, wine4)


summary(reg1) #R2: 0.8653

reg2 <- lm(Rating ~ Price + Alcohol + Sulphates + Residual_Sugar+ Country_Italy+Country_France+Country_US , wine4)

summary(reg2) #R2: 0.8667

plot(reg2)

density(plot(reg2))

predict(reg2, data.frame(Price = 39.99, Alcohol = 13.9, Sulphates = 0.5,Residual_Sugar = 1.96,Country_Italy=0,Country_France=1,Country_US=0))

predict(reg2,newdata=new.data)

#reg3 <- lm(Rating ~ Price + Alcohol + Sulphates + Country , wine)

#summary(reg3) #R2: 0.8673

#plot(reg3)


