### Data preparation and cleaning


# R libraries
library(tidyverse) # More inclusive than just dplyr
library(dplyr)
library(caret)
library(ggplot2)
library(car)
library(MASS)

# Read in the data set
cars <- read.csv("/Users/jamieliu/Desktop/Cars QM Project/carsData.csv")

# Check unique values for each categorical column
unique(cars$color)
unique(cars$engine_has_gas)
unique(cars$engine_fuel)
unique(cars$engine_type)
unique(cars$body_type)
unique(cars$transmission)
unique(cars$manufacturer_name)
unique(cars$drivetrain)
unique(cars$state)

# Dummy code car transmission type 
cars$transmission <- ifelse(cars$transmission == 'automatic', 1,0)
# Dummy code engine_has_gas 
cars$engine_has_gas <- ifelse(cars$engine_has_gas == 'True', 1,0)
# Dummy code car has_warranty
cars$has_warranty <- ifelse(cars$has_warranty == 'True', 1,0)
# Dummy code car is_exchangeable
cars$is_exchangeable <- ifelse(cars$is_exchangeable == 'True', 1,0)
# Dummy encode car color
for(unique_value in unique(cars$color)){
  cars[paste("Color", unique_value, sep = "_")] <- ifelse(cars$color == unique_value, 1, 0)
}
# Dummy encode engine_fuel
for(i in unique(cars$engine_fuel)){
  cars[paste("EngFuel", i, sep = "_")] <- ifelse(cars$engine_fuel == i, 1, 0)
}
# Dummy encode engine type
for(i in unique(cars$engine_type)){
  cars[paste("EngType", i, sep = "_")] <- ifelse(cars$engine_type == i, 1, 0)
}
#  Dummy encode drivetrain
for(i in unique(cars$drivetrain)){
  cars[paste(i,"drivTrain", sep = "_")] <- ifelse(cars$drivetrain == i, 1, 0)
}
# Dummy code state
for(i in unique(cars$state)){
  cars[paste("state",i, sep = "_")] <- ifelse(cars$state == i, 1, 0)
}
# Subset data
cars <- subset(cars, select = c(1,2,5,8,12,3,6,11,15:19,30,31:57,10))
View(cars)

# Rearrange columns 
carsData <- cars[,c(1,2,7,8,11,10,3,4:6,12:43,9)]
summary(carsData)

# No data in is_exchangeable - remove column
carsData$is_exchangeable <- NULL

# No data in transmission - remove column
carsData$transmission <- NULL

# No data in has_warranty - remove column
carsData$has_warranty <- NULL

# Data in location_region is corrupted - remove column
carsData$location_region <- NULL

# No data in engine_has_gas - remove column
carsData$engine_has_gas <- NULL

# Model names add too much complexity to the data - remove column
carsData$model_name <- NULL

# Renaming primary key
names(carsData)[names(carsData) == 'X'] <- 'Record ID'

# Subset data to only focus on the top 4 most common car manufacturers
unique(carsData$manufacturer_name)
carsData <- carsData %>% filter(manufacturer_name %in% c("Volkswagen","Opel","BMW","Ford")) # 4 most common
unique(carsData$manufacturer_name)

# Dummy Code Manufacturer
for(unique_value in unique(carsData$manufacturer_name)){
  carsData[paste("Name", unique_value, sep = "_")] <- ifelse(carsData$manufacturer_name == unique_value, 1, 0)
  }

carsData$manufacturer_name <- NULL

# Dummy Code Body Type
for(unique_value in unique(carsData$body_type)){
  carsData[paste("Name", unique_value, sep = "_")] <- ifelse(carsData$body_type == unique_value, 1, 0)
}

carsData$body_type <- NULL

# Re-Arrange data set
carsData <- carsData[,c(1,35,2:34,36:50)]

#Approach 1

# Multicollinearity Testing
cor(carsData, use = "complete.obs") # Decent correlation with these variables:
# Odomoter_Value, Year_Produced, Number_of_photos, color_blue, color_red, color_black, color_brown, color_green, engfuel_gasoline, engfuel_diesel, engtype_gasoline, engtype_diesel, all_drivtrain, front_drivTrain, state_new
# Name_Opel, Name_BMW, Name_hatchback, name_universal, name_SUV, name_minibus

ggplot(aes(x=odometer_value,y=price_usd),data=carsData) + geom_point()

# Create regression with correlated variables
initialreg <- lm(price_usd ~ year_produced + number_of_photos + Color_blue + Color_red + Color_black + Color_brown + Color_green + EngFuel_gasoline + EngFuel_diesel
                 + EngType_gasoline + EngType_diesel + all_drivTrain + front_drivTrain + state_new + Name_Opel + Name_BMW + Name_hatchback + Name_universal + Name_suv + Name_minibus, data = carsData)

summary(initialreg)

#Engfuel_gasoline was the only insignificant one, drop and create new model

reg2 <- lm(price_usd ~ year_produced + number_of_photos + Color_blue + Color_red + Color_black + Color_brown + Color_green + EngFuel_diesel
                 + EngType_gasoline + EngType_diesel + all_drivTrain + front_drivTrain + state_new + Name_Opel + Name_BMW + Name_hatchback + Name_universal + Name_suv + Name_minibus, data = carsData)

summary(reg2)

reg3 <- lm(price_usd ~ year_produced + number_of_photos + Color_red + Color_black + Color_brown + Color_green + EngFuel_diesel
           + EngType_gasoline + EngType_diesel + all_drivTrain + state_new + Name_Opel + Name_BMW + Name_hatchback + Name_universal + Name_suv + Name_minibus, data = carsData)

summary(reg3)

reg4 <- lm(price_usd ~ year_produced + number_of_photos + Color_red + Color_black + Color_brown + EngFuel_diesel
           + EngType_gasoline + all_drivTrain + state_new + Name_Opel + Name_BMW + Name_hatchback + Name_universal + Name_suv + Name_minibus, data = carsData)

summary(reg4)

reg5 <- lm(price_usd ~ year_produced + number_of_photos + EngFuel_diesel + EngType_gasoline + all_drivTrain + state_new + Name_Opel + Name_BMW 
           + Name_hatchback + Name_universal + Name_suv + Name_minibus, data = carsData)

summary(reg5)
# Cutting colors very barely affects the R^2

# Test for multicollinearity
vif(reg5)
# Cut Engfuel_diesel, Engtype_gasoline

reg6 <- lm(price_usd ~ year_produced + number_of_photos + EngType_gasoline + all_drivTrain + state_new + Name_Opel + Name_BMW 
                   + Name_hatchback + Name_universal + Name_suv + Name_minibus, data = carsData)

AIC(reg6)
vif(reg6)
summary(reg6)

stepAIC(reg6) # Smallest name_suv, name_hatchback

reg7 <- lm(price_usd ~ year_produced + number_of_photos + EngType_gasoline + all_drivTrain + state_new + Name_Opel + Name_BMW 
          + Name_universal + Name_minibus, data = carsData)

AIC(reg7)
summary(reg7)
stepAIC(reg7) #name universal, state_new

reg8 <- lm(price_usd ~ year_produced + number_of_photos + EngType_gasoline + all_drivTrain + Name_Opel + Name_BMW 
           + Name_minibus, data = carsData)

AIC(reg8)
summary(reg8)
stepAIC(reg8) #name_opel, engtype_gasoline

reg9 <- lm(price_usd ~ year_produced + number_of_photos + all_drivTrain + Name_BMW + Name_minibus, data = carsData)

AIC(reg9)

summary(reg9)

stepAIC(reg9) #number of photos

reg10 <- lm(price_usd ~ year_produced + all_drivTrain + Name_BMW + Name_minibus, data = carsData) # <- Ideal Model

AIC(reg10)

summary(reg10)

stepAIC(reg10)

reg11 <- lm(price_usd ~ year_produced + all_drivTrain + Name_BMW, data = carsData) #.3 R squared drop off

AIC(reg11) # AIC Increase

summary(reg11)

stepAIC(reg11)

reg12 <- lm(price_usd ~ year_produced + all_drivTrain, data = carsData) #.3 R squared drop off

summary(reg12)


# Approach 2
# Create model with all variables (not including Record ID column)
fit <- lm(price_usd~ ., data = carsData[-c(1)])
summary(fit)

# stepAIC to check for unnecessary variables in the first model
stepAIC(fit)

# Run a new model after variables were dropped from the first stepAIC
fit2<-lm(price_usd ~ odometer_value + year_produced + number_of_photos + 
           up_counter + duration_listed + Color_silver + Color_blue + 
           Color_red + Color_black + Color_other + Color_brown + Color_white + 
           Color_green + EngFuel_gasoline + EngFuel_gas + all_drivTrain + 
           front_drivTrain + state_owned + state_emergency + engine_capacity + 
           Name_Opel + Name_Ford + Name_Volkswagen + Name_hatchback + 
           Name_liftback + Name_sedan + Name_minivan + Name_universal + 
           Name_suv + Name_minibus + Name_van, data = carsData)

# Analyze second model
summary(fit2)
#up_counter, color_black, color_white had large p-values - drop

# Create new model without large p-value variables
fit3<-lm(price_usd ~ odometer_value + year_produced + number_of_photos + 
           duration_listed + Color_silver + Color_blue + 
           Color_red + Color_other + Color_brown + 
           Color_green + EngFuel_gasoline + EngFuel_gas + all_drivTrain + 
           front_drivTrain + state_owned + state_emergency + engine_capacity + 
           Name_Opel + Name_Ford + Name_Volkswagen + Name_hatchback + 
           Name_liftback + Name_sedan + Name_minivan + Name_universal + 
           Name_suv + Name_minibus + Name_van, data = carsData)
view(carsData)

# Check for multicollinearity
vif(fit3) #drop state_owned, state_emergency, Name_hatchback, Name_sedan, Name_universal
summary(fit3) #p-values look good

# Create new model without variables with high multicollinearity
fit4<-lm(price_usd ~ odometer_value + year_produced + number_of_photos + 
           duration_listed + Color_silver + Color_blue + 
           Color_red + Color_other + Color_brown + 
           Color_green + EngFuel_gasoline + EngFuel_gas + all_drivTrain + 
           front_drivTrain + engine_capacity + Name_Opel + Name_Ford + Name_Volkswagen + 
           Name_liftback + Name_minivan + Name_suv + Name_minibus + Name_van, data = carsData)

# stepAIC to see if any more insignificant variables drop out
stepAIC(fit4)
summary(fit4)

# Front_drivTrain was dropped in stepAIC of model 4, create new model with output
fit5<-lm(price_usd ~ odometer_value + year_produced + number_of_photos + 
           duration_listed + Color_silver + Color_blue + Color_red + 
           Color_other + Color_brown + Color_green + EngFuel_gasoline + 
           EngFuel_gas + all_drivTrain + engine_capacity + Name_Opel + 
           Name_Ford + Name_Volkswagen + Name_liftback + Name_minivan + 
           Name_suv + Name_minibus + Name_van, data = carsData)

# Check p-values of model 5
summary(fit5)

# Color_other p-value is high, drop and create new model
fit6<-lm(price_usd ~ odometer_value + year_produced + number_of_photos + 
           duration_listed + Color_silver + Color_blue + Color_red + 
           Color_brown + Color_green + EngFuel_gasoline + 
           EngFuel_gas + all_drivTrain + engine_capacity + Name_Opel + 
           Name_Ford + Name_Volkswagen + Name_liftback + Name_minivan + 
           Name_suv + Name_minibus + Name_van, data = carsData)

# Check p-values of model 6
summary(fit6) #p-values look good

# Final stepAIC to check for unnecessary variables
stepAIC(fit6) # no more variables were dropped

# Check residuals
plot(fit6)



# AIC scores of Approach 1
AIC(initialreg) #230833.8
AIC(reg2) #230831.9
AIC(reg3) #230834.1
AIC(reg4) #230835.2
AIC(reg5) #230944.8
AIC(reg6) #230998.8
AIC(reg7) #231113.8
AIC(reg8) #231337.4 - highest R^2 of Approach 1
AIC(reg9) #231618.3
AIC(reg10) #231866
AIC(reg11) #233055.3
AIC(reg12) #234096

# AIC scores of Approach 2
AIC(fit) #229707.3
AIC(fit2) #229699.3 - lowest AIC score, highest R^2 value
AIC(fit3) #229709.3
AIC(fit4) #230141
AIC(fit5) #230139.7
AIC(fit6) #230140.2
