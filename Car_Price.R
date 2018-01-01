#LINEAR REGRESSION ASSIGNMENT - CAR PRICE

# Importing essential libraries
library(stringr)
library(MASS)
library(car)
library(tidyr)
library(ggplot2)

# Importing the CarPrice dataset
carprice <- read.csv("CarPrice_Assignment.csv")
df_carprice <- carprice

# To view the structure and summary of the csv file
str(df_carprice)
summary(df_carprice)x

#CROSS CHECKING DATA CLEANING CHECKLIST
#Is there any summary, incorrect, extra rows? Answer : None
#Is there a need to merge/split/add(if not present)/rename/delete/align columns? Answer : YES
# 1) Variable "CarName" is to be split into "CompanyName" and "ModelName"

df_carprice <- separate(df_carprice, CarName, into = c("CompanyName","ModelName"), 
                        sep = " ", extra = "merge", fill = "right")

# Removing model name as its not to be used in the model.
df_carprice <- df_carprice[,-4]

# Analyse and handle Missing Values(if required)
length(which(is.na(df_carprice))) # No NA values found
sapply(df_carprice, function(x) length(which(x == ""))) # No blank values

#STANDARDISING VALUES
# Checking for outliers in Numeric Variables
quantile(df_carprice$wheelbase, seq(0,1,0.01)) 
quantile(df_carprice$carlength, seq(0,1,0.01))
quantile(df_carprice$carwidth, seq(0,1,0.01))
quantile(df_carprice$carheight, seq(0,1,0.01))
quantile(df_carprice$curbweight, seq(0,1,0.01))
quantile(df_carprice$boreratio, seq(0,1,0.01))
quantile(df_carprice$stroke, seq(0,1,0.01))
quantile(df_carprice$compressionratio, seq(0,1,0.01))
quantile(df_carprice$peakrpm, seq(0,1,0.01))
quantile(df_carprice$citympg, seq(0,1,0.01))
quantile(df_carprice$highwaympg, seq(0,1,0.01))

# There is a jump of 50 units from 98%
quantile(df_carprice$enginesize, seq(0,1,0.01)) # 98%
boxplot(df_carprice$enginesize)
df_carprice$enginesize[which(df_carprice$enginesize > 209)] <- 209

# There is a jump of over 70 units from 99%
quantile(df_carprice$horsepower, seq(0,1,0.01)) #99%
boxplot(df_carprice$horsepower)
df_carprice$horsepower[which(df_carprice$horsepower > 207)] <- 207

#Standardising Text Values
column_factor <- c("CompanyName", "fueltype", "aspiration", "doornumber", "carbody", "drivewheel", "enginelocation",
                   "enginetype", "cylindernumber", "fuelsystem" )
df_carprice[column_factor] <- lapply(df_carprice[column_factor], function(x) factor(toupper(x)))

# INVALID VALUES
# Correcting the data types
summary(df_carprice) # Some factors are having too many levels
# Converting into factor type
df_carprice[c("symboling", "CompanyName")] <- lapply(df_carprice[c("symboling", "CompanyName")], function(x) factor(x))
str(df_carprice)

summary(df_carprice$CompanyName) # multiple variations(incorrect spelling) of the same company name found
levels(df_carprice$CompanyName)[c(10,11)] <- "MAZDA"
levels(df_carprice$CompanyName)[c(16,17)] <- "PORSCHE"
levels(df_carprice$CompanyName)[c(20,21)] <- "TOYOTA"
levels(df_carprice$CompanyName)[c(21,22,24)] <- "VOLKSWAGEN"

# Some factors have too many levels. Hence grouping them based on frequency for level reduction
# to aid regression. Also grouping factor level with too few occurences
# CompanyName
summary(df_carprice$CompanyName)
levels(df_carprice$CompanyName)[c(1,5,8,9,11,16,17)] <- "OTHERS"
levels(df_carprice$CompanyName)[c(2,3,4,5,11,12)] <- "AUDI-BMW-BUICK-DODGE-PLY_SAAB"
levels(df_carprice$CompanyName)[c(3,5,7,8,10,11)] <- "HOND-MITSU-PEUG-SUBARU-VW-VOLV"

# Carbody
summary(df_carprice$carbody)
levels(df_carprice$carbody)[c(1,2)] <- "CONVERTIBLE-HARDTOP"

# EngineType
summary(df_carprice$enginetype)
levels(df_carprice$enginetype)[c(2,7)] <- "DOHCV-ROTOR"
levels(df_carprice$enginetype)[c(1,3)] <- "DOHC-L"
levels(df_carprice$enginetype)[c(4,5)] <- "OHCF-OHCV"

# CylinderNumber
summary(df_carprice$cylindernumber)
levels(df_carprice$cylindernumber)[c(1,5,6,7)] <- "TWO-THREE-EIGHT-TWELVE"

summary(df_carprice$fuelsystem)
levels(df_carprice$fuelsystem)[c(3,5,7,8)] <- "4BBL-MFI-SPDI-SPFI"

#FILTERING DATA
# Checking for duplicate rows
sum(duplicated(df_carprice$car_ID)) # no duplicate rows found.

#UNIVARIATE ANALYSIS
#Creating the ggplot variable 
var_ggplot <- ggplot(df_carprice)

#Plotting univatiate plots for using barcharts
var_ggplot + geom_bar(aes(x = symboling)) #most cars are relatively safe
var_ggplot + geom_bar(aes(x = CompanyName)) + theme(axis.text.x = element_text(angle = 90, hjust = 1)) # Toyota is most popular
var_ggplot + geom_bar(aes(x = carbody)) # Sedan is the most popular type
var_ggplot + geom_bar(aes(x = drivewheel)) # most are FWD
var_ggplot + geom_bar(aes(x = enginetype)) # most are OHC
var_ggplot + geom_bar(aes(x = cylindernumber)) # most have four cylinders
var_ggplot + geom_bar(aes(x = fuelsystem)) # most are MPFI and 2BBL fuel system 

#MULTIVARIATE ANALYSIS
# Comparing prices using scatter plot with factor on the X axis
var_ggplot + geom_point(aes(x = symboling, y = price, colour = carbody), size = 3) # -1 to 1 tend to be expensive
var_ggplot + geom_point(aes(x = symboling, y = price, colour = drivewheel), size = 3) # RWD is the more expensive
var_ggplot + geom_point(aes(x = carbody, y = price, colour = enginelocation), size = 4) # Sedan tends to be expensive
var_ggplot + geom_point(aes(x = enginetype, y = price, colour = carbody), size = 3)
var_ggplot + geom_point(aes(x = fuelsystem, y = price, colour = carbody), size = 3) # MPFI tend to be expensive
var_ggplot + geom_point(aes(x = doornumber, y = price, colour = carbody), size = 3) 
var_ggplot + geom_point(aes(x = cylindernumber, y = price, colour = carbody), size = 3) # four cylinder cars are cheap
var_ggplot + geom_point(aes(x = CompanyName, y = price, colour = carbody), size = 3) +
   theme(axis.text.x = element_text(angle = 90, hjust = 1))

# Comparing prices with coninuous variables on the X axis(Only relationships with patterns have been plotted)
# wheelbase vs price : low linear
var_ggplot + geom_point(aes(x = wheelbase, y = price, colour = carbody, shape = drivewheel), size = 3) +
  geom_smooth(aes(x = wheelbase, y = price)) 

# carlength vs price : moderately linear
var_ggplot + geom_point(aes(x = carlength, y = price, colour = carbody, shape = drivewheel), size = 3) +
  geom_smooth(aes(x = carlength, y = price))

# curbweight vs price : highly linear
var_ggplot + geom_point(aes(x = curbweight, y = price, colour = carbody, shape = drivewheel), size = 3) +
  geom_smooth(aes(x = curbweight, y = price))

# enginesize vs price : moderately linear
var_ggplot + geom_point(aes(x = enginesize, y = price, colour = carbody, shape = drivewheel), size = 3) +
  geom_smooth(aes(x = enginesize, y = price))

# horsepower vs price : highly linear
var_ggplot + geom_point(aes(x = horsepower, y = price, colour = carbody, shape = drivewheel), size = 3) +
  geom_smooth(aes(x = horsepower, y = price))

# highwaympg vs price : negative relationship
var_ggplot + geom_point(aes(x = highwaympg, y = price, colour = carbody, shape = drivewheel), size = 3) +
  geom_smooth(aes(x = highwaympg, y = price))

# CREATING DUMMY VARIABLES
# Two level factor variable : fueltype, aspiration, doornumber, enginelocation
levels(df_carprice$fueltype) <- c(0,1)   # Diesel : 0 | Gas : 1
levels(df_carprice$aspiration) <- c(0,1) # STD : 0    | TURBO : 1
levels(df_carprice$doornumber) <- c(0,1) # FOUR : 0   | TWO  : 1
levels(df_carprice$enginelocation) <- c(0,1) # FRONT : 0 | REAR : 1

# Multi level factor variable : symboling(6), CompanyName(6), carbody(4), drivewheel(3), 
#                               enginetype(4), cylindernumber(4), fuelsystem(5)
dummy_symboling <- data.frame(model.matrix(~symboling, data = df_carprice)) # 5 dummy variables
dummy_CompanyName <- data.frame(model.matrix(~CompanyName, data = df_carprice))
dummy_carbody <- data.frame(model.matrix(~carbody, data = df_carprice))
dummy_drivewheel <- data.frame(model.matrix(~drivewheel, data = df_carprice))
dummy_enginetype <- data.frame(model.matrix(~enginetype, data = df_carprice))
dummy_cylindernumber <- data.frame(model.matrix(~cylindernumber, data = df_carprice))
dummy_fuelsystem <- data.frame(model.matrix(~fuelsystem, data = df_carprice))

# Adding dummy variables along with the other variables to do a sanity check
df_model1 <- cbind(df_carprice[c(1,2)], dummy_symboling[-1], df_carprice[3], dummy_CompanyName[-1],
                   df_carprice[c(4:7)], dummy_carbody[-1], df_carprice[8], dummy_drivewheel[-1],
                   df_carprice[c(9:15)],dummy_enginetype[-1], df_carprice[16], dummy_cylindernumber[-1],
                   df_carprice[c(17,18)], dummy_fuelsystem[-1], df_carprice[c(19:26)])

# Prepating for the model by dropping all the factor variables
to_numeric_columns <- c("fueltype", "aspiration", "doornumber", "enginelocation")
df_model1[to_numeric_columns] <- sapply(df_model1[to_numeric_columns], function(x) as.numeric(as.character(x)))
numeric_indices <- as.numeric(which(sapply(df_model1, is.numeric)))
df_model1 <- df_model1[,numeric_indices]
df_model1 <- df_model1[,-1] # removind CarID as it is only an identifier and has no impact on price

View(data.frame(cor(df_model1))) # Correlation matrix

# MODELLING : LINEAR REGRESSION
# Splitting into test and train data
set.seed(100)
train_indices <- sample(1:nrow(df_model1), 0.7*nrow(df_model1))

train <- df_model1[train_indices,]
test <- df_model1[-train_indices,]

# Building Model with all variables
model_1 <- lm(price~. , data = train)
summary(model_1)

# Using StepAIC to reduce the number of predictors
step <- stepAIC(model_1, direction="both")
step

##################### MODEL APPROACH 1 ########################

# BACKWARD SELECTION 
# Model with predictors given by StepAIC
model_2 <- lm(formula = price ~ symboling0 + symboling1 + CompanyNameHOND.MITSU.PEUG.SUBARU.VW.VOLV + 
                CompanyNameMAZDA + CompanyNameNISSAN + CompanyNameTOYOTA + 
                carbodyHATCHBACK + carbodySEDAN + carbodyWAGON + enginelocation + 
                wheelbase + carlength + carwidth + curbweight + enginetypeDOHCV.ROTOR + 
                enginetypeOHC + cylindernumberFIVE + cylindernumberFOUR + 
                cylindernumberSIX + fuelsystem2BBL + fuelsystem4BBL.MFI.SPDI.SPFI + 
                fuelsystemMPFI + stroke + horsepower, data = train)
summary(model_2)

# Checking the Collinearity between the predictors
sort(vif(model_2), TRUE)
# Dropping the variable Carlength due to high collinearity and low significance
model_3 <- lm(formula = price ~ symboling0 + symboling1 + CompanyNameHOND.MITSU.PEUG.SUBARU.VW.VOLV + 
                CompanyNameMAZDA + CompanyNameNISSAN + CompanyNameTOYOTA + carbodyHATCHBACK + carbodySEDAN + carbodyWAGON + 
                enginelocation + wheelbase + carwidth + curbweight + enginetypeDOHCV.ROTOR +  enginetypeOHC + 
                cylindernumberFIVE + cylindernumberFOUR + cylindernumberSIX + fuelsystem2BBL + fuelsystem4BBL.MFI.SPDI.SPFI +
                fuelsystemMPFI + stroke + horsepower, data = train)
summary(model_3)

# Checking the Collinearity between the predictors
sort(vif(model_3), TRUE)
# Dropping the variable wheelbase due to high collinearity and low significance
model_4 <- lm(formula = price ~ symboling0 + symboling1 + CompanyNameHOND.MITSU.PEUG.SUBARU.VW.VOLV + 
                CompanyNameMAZDA + CompanyNameNISSAN + CompanyNameTOYOTA + carbodyHATCHBACK + carbodySEDAN + carbodyWAGON + 
                enginelocation + carwidth + curbweight + enginetypeDOHCV.ROTOR + enginetypeOHC + cylindernumberFIVE + 
                cylindernumberFOUR + cylindernumberSIX + fuelsystem2BBL + fuelsystem4BBL.MFI.SPDI.SPFI + 
                fuelsystemMPFI + stroke + horsepower, data = train)
summary(model_4)

# Checking the Collinearity between the predictors
sort(vif(model_4), TRUE)
# Dropping the variable carbodySEDAN due to high collinearity and low significance
model_5 <- lm(formula = price ~ symboling0 + symboling1 + CompanyNameHOND.MITSU.PEUG.SUBARU.VW.VOLV + 
                CompanyNameMAZDA + CompanyNameNISSAN + CompanyNameTOYOTA + carbodyHATCHBACK + carbodyWAGON + 
                enginelocation + carwidth + curbweight + enginetypeDOHCV.ROTOR + enginetypeOHC + cylindernumberFIVE + 
                cylindernumberFOUR + cylindernumberSIX + fuelsystem2BBL + fuelsystem4BBL.MFI.SPDI.SPFI + 
                fuelsystemMPFI + stroke + horsepower, data = train)
summary(model_5)

# Checking the Collinearity between the predictors
sort(vif(model_5), TRUE)
# Dropping the variable symboling0 due to low significance
model_6 <- lm(formula = price ~ symboling1 + CompanyNameHOND.MITSU.PEUG.SUBARU.VW.VOLV + 
                CompanyNameMAZDA + CompanyNameNISSAN + CompanyNameTOYOTA + carbodyHATCHBACK + carbodyWAGON + 
                enginelocation + carwidth + curbweight + enginetypeDOHCV.ROTOR + enginetypeOHC + cylindernumberFIVE + 
                cylindernumberFOUR + cylindernumberSIX + fuelsystem2BBL + fuelsystem4BBL.MFI.SPDI.SPFI + 
                fuelsystemMPFI + stroke + horsepower, data = train)
summary(model_6)

# Checking the Collinearity between the predictors
sort(vif(model_6), TRUE)
# Dropping the variable symboling1 due to low significance
model_7 <- lm(formula = price ~ CompanyNameHOND.MITSU.PEUG.SUBARU.VW.VOLV + CompanyNameMAZDA + 
                CompanyNameNISSAN + CompanyNameTOYOTA + carbodyHATCHBACK + carbodyWAGON + enginelocation + 
                carwidth + curbweight + enginetypeDOHCV.ROTOR + enginetypeOHC + cylindernumberFIVE + 
                cylindernumberFOUR + cylindernumberSIX + fuelsystem2BBL + fuelsystem4BBL.MFI.SPDI.SPFI + 
                fuelsystemMPFI + stroke + horsepower, data = train)
summary(model_7)

# Checking the Collinearity between the predictors
sort(vif(model_7), TRUE)

# All variables are showing high significance hence checking correlation
cor(train$curbweight,train$carwidth)
cor(train$curbweight,train$horsepower)

# High correlation between curbweight, carwidth, horsepower, cylinderFOUR. 
# Curbweight only  refers to the weight of the vehicle which is likely to be a combination to the horsepower,
# cylinders, engine and size
# Removing Curbweight since the other terms provide more bussiness value in predicting the factors
model_8 <- lm(formula = price ~ CompanyNameHOND.MITSU.PEUG.SUBARU.VW.VOLV + CompanyNameMAZDA + 
                CompanyNameNISSAN + CompanyNameTOYOTA + carbodyHATCHBACK + carbodyWAGON + enginelocation + 
                carwidth + enginetypeDOHCV.ROTOR + enginetypeOHC + cylindernumberFIVE + 
                cylindernumberFOUR + cylindernumberSIX + fuelsystem2BBL + fuelsystem4BBL.MFI.SPDI.SPFI + 
                fuelsystemMPFI + stroke + horsepower, data = train)
summary(model_8)

# Checking the Collinearity between the predictors
sort(vif(model_8), TRUE)
# Dropping the variable enginetypeOHC due to low significance
model_9 <- lm(formula = price ~ CompanyNameHOND.MITSU.PEUG.SUBARU.VW.VOLV + CompanyNameMAZDA + 
                CompanyNameNISSAN + CompanyNameTOYOTA + carbodyHATCHBACK + carbodyWAGON + enginelocation + 
                carwidth + enginetypeDOHCV.ROTOR + cylindernumberFIVE + 
                cylindernumberFOUR + cylindernumberSIX + fuelsystem2BBL + fuelsystem4BBL.MFI.SPDI.SPFI + 
                fuelsystemMPFI + stroke + horsepower, data = train)
summary(model_9)

# Checking the Collinearity between the predictors
sort(vif(model_9), TRUE)
# Dropping the variable stroke due to low significance
model_10 <- lm(formula = price ~ CompanyNameHOND.MITSU.PEUG.SUBARU.VW.VOLV + CompanyNameMAZDA + 
                CompanyNameNISSAN + CompanyNameTOYOTA + carbodyHATCHBACK + carbodyWAGON + enginelocation + 
                carwidth + enginetypeDOHCV.ROTOR + cylindernumberFIVE + 
                cylindernumberFOUR + cylindernumberSIX + fuelsystem2BBL + fuelsystem4BBL.MFI.SPDI.SPFI + 
                fuelsystemMPFI + horsepower, data = train)
summary(model_10)

# Checking the Collinearity between the predictors
sort(vif(model_10), TRUE)
# Dropping the variable carbodyWAGON due to low significance
model_11 <- lm(formula = price ~ CompanyNameHOND.MITSU.PEUG.SUBARU.VW.VOLV + CompanyNameMAZDA + 
                 CompanyNameNISSAN + CompanyNameTOYOTA + carbodyHATCHBACK +  enginelocation + 
                 carwidth + enginetypeDOHCV.ROTOR + cylindernumberFIVE + 
                 cylindernumberFOUR + cylindernumberSIX + fuelsystem2BBL + fuelsystem4BBL.MFI.SPDI.SPFI + 
                 fuelsystemMPFI + horsepower, data = train)
summary(model_11)

# Checking the Collinearity between the predictors
sort(vif(model_11), TRUE)
# Dropping the variable fuelsystem2BBL due to high collinearity and relatively low significance
model_12 <- lm(formula = price ~ CompanyNameHOND.MITSU.PEUG.SUBARU.VW.VOLV + CompanyNameMAZDA + 
                 CompanyNameNISSAN + CompanyNameTOYOTA + carbodyHATCHBACK +  enginelocation + 
                 carwidth + enginetypeDOHCV.ROTOR + cylindernumberFIVE + 
                 cylindernumberFOUR + cylindernumberSIX + fuelsystem4BBL.MFI.SPDI.SPFI + 
                 fuelsystemMPFI + horsepower, data = train)
summary(model_12)

# Checking the Collinearity between the predictors
sort(vif(model_12), TRUE)
# Dropping the variable fuelsystem4BBL.MFI.SPDI.SPFI due to low significance
model_13 <- lm(formula = price ~ CompanyNameHOND.MITSU.PEUG.SUBARU.VW.VOLV + CompanyNameMAZDA + 
                 CompanyNameNISSAN + CompanyNameTOYOTA + carbodyHATCHBACK +  enginelocation + 
                 carwidth + enginetypeDOHCV.ROTOR + cylindernumberFIVE + 
                 cylindernumberFOUR + cylindernumberSIX + fuelsystemMPFI + horsepower, data = train)
summary(model_13)

# Checking the Collinearity between the predictors
sort(vif(model_13), TRUE)
# Dropping the variable fuelsystemMPFI due to low significance 
model_14 <- lm(formula = price ~ CompanyNameHOND.MITSU.PEUG.SUBARU.VW.VOLV + CompanyNameMAZDA + 
                 CompanyNameNISSAN + CompanyNameTOYOTA + carbodyHATCHBACK +  enginelocation + 
                 carwidth + enginetypeDOHCV.ROTOR + cylindernumberFIVE + 
                 cylindernumberFOUR + cylindernumberSIX + horsepower, data = train)

summary(model_14) # Adjusted R Square is 0.902

sort(vif(model_14), TRUE) # High Multicollinearity and high signicicance
# High Multicolinearity indicates that the the coefficients of the predictors are not accurate even if there is 
# a high R square

# predicting the results in test dataset
Predict_1 <- predict(model_14,test[,-1])
test$test_price <- Predict_1

# Now, we need to test the r square between actual and predicted sales. 
r <- cor(test$price,test$test_price) 
rsquared <- cor(test$price,test$test_price)^2
rsquared # R Square on test data is 0.7753623

######################## MODEL APPROACH 2 ########################

# Forward Selection Method
# Adding available non-dummy variables from the StepAIC output
# Variable : horsepower
model_A1 <- lm(price~ horsepower, data = train)
summary(model_A1)

# Variable : stroke
model_A2 <- lm(price~ horsepower + stroke, data = train)
summary(model_A2) # stroke has no significance hence to be removed.

# Variable : carlength
model_A3 <- lm(price~ horsepower + carlength, data = train)
summary(model_A3) # carlength has high significance.
vif(model_A3) # VIF indicates minimum multicolinearity

# Variable : wheelbase
model_A4 <- lm(price~ horsepower + carlength + wheelbase, data = train)
summary(model_A4) # carlength has low significance, hence dropping carlength
vif(model_A4) # High correlation between wheelbase and carlength

# Variable : carlength dropped
model_A5 <- lm(price~ horsepower + wheelbase, data = train)
summary(model_A5) # carlength has low significance, hence dropping carlength
vif(model_A5) # VIF indicates minimum multicolinearity

# Variable : carwidth
model_A6 <- lm(price~ horsepower + wheelbase  + carwidth, data = train)
summary(model_A6) # wheelbase has low significance, hence dropping 
vif(model_A6) 

# Variable : wheelbase dropped
model_A7 <- lm(price~ horsepower + carwidth, data = train)
summary(model_A7) # carlength has low significance, hence dropping carlength
vif(model_A7) 

# Variable : curbweight
model_A8 <- lm(price~ horsepower + carwidth + curbweight, data = train)
summary(model_A8) # curbweight has high significance
vif(model_A8) # High correlation between carwidth and curbweight, but both are significant

# Adding dummy variables from the stepAIC output
# Variable : symboling0
model_A9 <- lm(price~ horsepower + carwidth + curbweight + symboling0, data = train)
summary(model_A9) # symboling0 has low significance, hence dropping 
vif(model_A9)

# Variable : symboling1
model_A10 <- lm(price~ horsepower + carwidth + curbweight + symboling1, data = train)
summary(model_A10) # symboling1 has low significance, hence dropping 
vif(model_A10)

# Variable : carbodyHATCHBACK
model_A11 <- lm(price~ horsepower + carwidth + curbweight + carbodyHATCHBACK, data = train)
summary(model_A11) # carbodyHATCHBACK has high significance,
vif(model_A11)

# Variable : carbodySEDAN
model_A12 <- lm(price~ horsepower + carwidth + curbweight + carbodyHATCHBACK + carbodySEDAN, data = train)
summary(model_A12) # carbodySEDAN has low significance, hence dropping 
vif(model_A12)

# Variable : carbodyWAGON
model_A13 <- lm(price~ horsepower + carwidth + curbweight + carbodyHATCHBACK + carbodyWAGON, data = train)
summary(model_A13) # carbodyWAGON has high significance, 
vif(model_A13)

# Variable : enginelocation
model_A14 <- lm(price~ horsepower + carwidth + curbweight + carbodyHATCHBACK + carbodyWAGON +
                  enginelocation, data = train)
summary(model_A14) # carbodyWAGON has high significance, 
vif(model_A14)

# Variable : enginetypeDOHCV.ROTOR
model_A15 <- lm(price~ horsepower + carwidth + curbweight + carbodyHATCHBACK + carbodyWAGON +
                  enginelocation + enginetypeDOHCV.ROTOR, data = train)
summary(model_A15) # enginetypeDOHCV.ROTOR has low significance, hence dropping
vif(model_A15)

# Variable : enginetypeOHC
model_A16 <- lm(price~ horsepower + carwidth + curbweight + carbodyHATCHBACK + carbodyWAGON +
                  enginelocation + enginetypeOHC, data = train)
summary(model_A16) # enginetypeOHC has low significance, hence dropping
vif(model_A16)

# Variable : cylindernumberFIVE
model_A17 <- lm(price~ horsepower + carwidth + curbweight + carbodyHATCHBACK + carbodyWAGON +
                  enginelocation + cylindernumberFIVE, data = train)
summary(model_A17) # cylindernumberFIVE has low significance, hence dropping
vif(model_A17)

# Variable : cylindernumberFOUR
model_A18 <- lm(price~ horsepower + carwidth + curbweight + carbodyHATCHBACK + carbodyWAGON +
                  enginelocation + cylindernumberFOUR, data = train)
summary(model_A18) # cylindernumberFOUR has high significance, 
vif(model_A18)

# Variable : cylindernumberSIX
model_A19 <- lm(price~ horsepower + carwidth + curbweight + carbodyHATCHBACK + carbodyWAGON +
                  enginelocation + cylindernumberFOUR + cylindernumberSIX, data = train)
summary(model_A19) # cylindernumberSIX has high significance,
vif(model_A19)

# Variable : fuelsystem2BBL
model_A20 <- lm(price~ horsepower + carwidth + curbweight + carbodyHATCHBACK + carbodyWAGON +
                  enginelocation + cylindernumberFOUR + cylindernumberSIX + fuelsystem2BBL, data = train)
summary(model_A20) # fuelsystem2BBL has low significance, hence dropping
vif(model_A20)

# Variable : fuelsystem4BBL.MFI.SPDI.SPFI
model_A21 <- lm(price~ horsepower + carwidth + curbweight + carbodyHATCHBACK + carbodyWAGON +
                  enginelocation + cylindernumberFOUR + cylindernumberSIX + fuelsystem4BBL.MFI.SPDI.SPFI, 
                data = train)
summary(model_A21) # fuelsystem4BBL.MFI.SPDI.SPFI has high significance,
vif(model_A21) # dropping carwidth due to high collinearity with curbweight but low significance

# Variable : fuelsystemMPFI
model_A22 <- lm(price~ horsepower + curbweight + carbodyHATCHBACK + carbodyWAGON +
                  enginelocation + cylindernumberFOUR + cylindernumberSIX + fuelsystem4BBL.MFI.SPDI.SPFI + 
                  fuelsystemMPFI, data = train)
summary(model_A22) # fuelsystemMPFI has low significance, hence dropping 
vif(model_A22)

# Variable : fuelsystemMPFI dropped
model_A23 <- lm(price~ horsepower + curbweight + carbodyHATCHBACK + carbodyWAGON + 
                  enginelocation + cylindernumberFOUR + cylindernumberSIX + fuelsystem4BBL.MFI.SPDI.SPFI,
                data = train)
summary(model_A23) # fuelsystemMPFI has low significance high correlation with horsepower, hence dropping
vif(model_A23) # the multicolinerity is minimum indicating that the coefficients are accurate

# Adjusted R-squared:  0.8844

# predicting the results in test dataset
Predict_A1 <- predict(model_A23,test[,-1])
test$test_price <- Predict_A1

# Now, we need to test the r square between actual and predicted sales. 
r2 <- cor(test$price,test$test_price)
rsquared2 <- cor(test$price,test$test_price)^2
rsquared2 # R Square on test data is 0.7867738

###################### MODEL APPROACH 3 ##########################

# This is a Forward selection approach to see is Company name has any impact on the Prices.

# Forward Selection Method
# Adding available Vehicle Type and Company to check influence on the price
# Variable : CompanyNameHOND.MITSU.PEUG.SUBARU.VW.VOLV
model_B1 <- lm(price~ CompanyNameHOND.MITSU.PEUG.SUBARU.VW.VOLV, data = train)
summary(model_B1) # Variable has significance although minor

# Variable : CompanyNameMAZDA
model_B2 <- lm(price~ CompanyNameHOND.MITSU.PEUG.SUBARU.VW.VOLV + CompanyNameMAZDA, data = train)
summary(model_B2) # Variable has significance although minor
vif(model_B2)

# Variable : CompanyNameNISSAN
model_B3 <- lm(price~ CompanyNameHOND.MITSU.PEUG.SUBARU.VW.VOLV + CompanyNameMAZDA + CompanyNameNISSAN,
               data = train)
summary(model_B3) # Variable has significance although minor
vif(model_B3)

# Variable : CompanyNameTOYOTA
model_B4 <- lm(price~ CompanyNameHOND.MITSU.PEUG.SUBARU.VW.VOLV + CompanyNameMAZDA + CompanyNameNISSAN +
                 CompanyNameTOYOTA, data = train)
summary(model_B4) # Variable has significance although minor
vif(model_B4) 

# Adjusted R-squared:  0.2524. Too low indicating lack of a linear relationship between company name and price

# Adding Type of Cars
# Variable : carbodyHATCHBACK
model_B5 <- lm(price~ CompanyNameHOND.MITSU.PEUG.SUBARU.VW.VOLV + CompanyNameMAZDA + CompanyNameNISSAN +
                 CompanyNameTOYOTA + carbodyHATCHBACK, data = train)
summary(model_B5) # Variable has significance although minor
vif(model_B5)

# Variable : carbodySEDAN
model_B6 <- lm(price~ CompanyNameHOND.MITSU.PEUG.SUBARU.VW.VOLV + CompanyNameMAZDA + CompanyNameNISSAN +
                 CompanyNameTOYOTA + carbodyHATCHBACK + carbodySEDAN, data = train)
summary(model_B6) # Variable has no significance, hence dropping
vif(model_B6)

# Variable : carbodyWAGON
model_B7 <- lm(price~ CompanyNameHOND.MITSU.PEUG.SUBARU.VW.VOLV + CompanyNameMAZDA + CompanyNameNISSAN +
                 CompanyNameTOYOTA + carbodyHATCHBACK + carbodyWAGON, data = train)
summary(model_B7) # Variable has no significance, hence dropping
vif(model_B7)

# Adjusted R-squared:  0.3058 is too low, however trying prediction

# predicting the results in test dataset
Predict_B1 <- predict(model_B7,test[,-1])
test$test_price <- Predict_B1

# Now, we need to test the r square between actual and predicted sales. 
r3 <- cor(test$price,test$test_price)
rsquared3 <- cor(test$price,test$test_price)^2
rsquared3 # R Square on test data is 0.132488 confirming that the Company name has negligible impact

# Forward Selection Method
# Adding available  Vehicle Type alone to check influence on the price
# Variable : carbodyHATCHBACK
model_C1 <- lm(price~ carbodyHATCHBACK, data = train)
summary(model_C1)

# Variable : carbodyHATCHBACK
model_C2 <- lm(price~ carbodyHATCHBACK + carbodySEDAN, data = train)
summary(model_C2) # carbodySEDAN has no sgnificance

# Variable : carbodyWAGON
model_C3 <- lm(price~ carbodyHATCHBACK + carbodyWAGON, data = train)
summary(model_C3) # carbodyWAGON has no significance 

# Adjusted R-squared:  0.05876 is too low indicating negligible relationship with price

# CONCLUSION : 

# Model Approach 1 : Backward Selection

# Adjusted R-squared:  0.9025
# Multi Colinearity: High(Variables with VIF values between 5 and 8)
# Rsquare on Test data : 0.7753623
# Result : Model rejected

# This approach has high Multicolinearity which indicates that the coefficients for each of 
# the predictors would be inaccurate. The R square on the test data are also less than the 
# adjusted R square by 13%. The goal of this exercise is not the accuracy of predicting the
# price but to "identify the variables and find how well it describes the dependent variable."
# However the presense of multicollinearity would lead to inaccurate interpretation of the 
# response variable. Hence rejecting the model

###############################################################################################

# Model Approach 2: Forward Selection
# Adjusted R-squared:  0.8844
# Multi Colinearity: Low( all VIFs less than 4.5)
# Rsquare on Test data : 0.7867738
# Result : Model Accepted

# This approach has low collinearity which in turn indicates that the coefficients for each of
# the predictors are relatively more accurate. The difference in R square value between training data
# and test data is also minor(indicating that the model is not overfit).Given the goal(mentioned above) 
# of the exercise this model will provide a more accurate interpretation of the response variable.
#
# Price = -8196.76 + 49.28(horsepower) +  8.5711(curbweight) -1225.8239(carbodyHATCHBACK) 
#          -2941.0330(carbodyWAGON) + 13561.0832(enginelocation) -5605.6767(cylindernumberFOUR)
#          -4786.9001(cylindernumberSIX) -2747.1121(fuelsystem4BBL.MFI.SPDI.SPFI)
#
# The Variables carbodyHATCHBACK, carbodyWAGON, enginelocation, cylindernumberFOUR, cylindernumberSIX,
# and fuelsystem4BBL.MFI.SPDI.SPFI take binary values(0 or 1) to finally predict the vehicle price.
# That is for a vehicle for which all these binary values are zero the price depends on the horsepower and 
# weight of the vehicle.


################################################################################################

# Model Approach 3: Forward Selection
# This is done only to see if there is any relationship of significance between the "CompanyName" and the
# price. The adjusted R square(0.2524) is too low indicating lack of a linear relationship between "CompanyName"
# and price car. On addition of the "CarBody" variable the adjusted R value increaded to 0.3058 which is also 
# too low to indicate any kind of linear relationship. The R square value(0.132488) on Test data also confirms this.
