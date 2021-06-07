### Tomiyosi Bola  ######
####  All tree data compilation Feb-01-2021  ######


###Load the packages


library(magrittr)
library(tidyverse)
###Importing the complete data set and name it AlltreeData.

AlltreeData <- read.csv("All_tree_data.csv", header = TRUE, sep = ",")
AlltreeData

##Check for the class of all columns in AlltreeData set

lapply(AlltreeData, class)

###Replace all the "-" in the data set with NA

AlltreeData[AlltreeData == "-"] <- NA
View(AlltreeData)

sapply(AlltreeData, class)

###Convert the following columns to numeric, DBH, HT, PTPH, BTPH

cols.num <- c("DBH", "HT", "PTPH", "BTPH") # First column bind the lsited columns into cols.num
AlltreeData[cols.num] <- sapply(AlltreeData[cols.num], as.numeric) # Use the sapply fucntion to convert the columns to numeric
sapply(AlltreeData, class) #Check the new class for the columns

##Calculate the tree basal area (area/ha) for all the data set, this will be called BA and added to the AlltreeData set

AlltreeData$BA <- (AlltreeData$DBH/100/2)^2*pi ##Calculating tree basal area (area/ha) in m^2
AlltreeData
head(AlltreeData)
## Calculate the total stand basal area (m^2)





### Calculating the basalarea per ha (Baha) in m^2/ha, this represent the basal area of each individual tree per ha based on the plot size.

AlltreeData$Baha <- AlltreeData$BA * AlltreeData$PTPH #The basal area is multiply by the number of plot trees per ha in the AlltreeData set

AlltreeData
head(AlltreeData)


## Calulate the total plot basal area per ha (plot Baha)

PlotBaha <- sum(AlltreeData$Baha, na.rm = TRUE) ## The calculated Baha is summed to get the total plot Baha
PlotBaha


### Creating a diameter distribution for DBH (cm) in the AlltreeData

AlltreeData$DCLASS <- NA ### I created a column for the diameter classes and named it DCLASS and filled it with NAs for now

head(AlltreeData)

summary(AlltreeData$DBH) ##Checking for the minimum and maximum value of the diameter (cm) before creating a suitable diameter class

### I will now create a fucntion for the diameter (cm) class with 10cm class interval. The function will be named DCF (Diameter class function)

DCF <- function(x) I(x>0) + I(x>10) + I(x>20) + I(x>30) + I(x>40) + I(x>50) + I(x>60) + 
  I(x>70) + I(x>80) + I(x>90)+ I(x>100) + I(x>110) + I(x>120) + I(x>130) ##The class interval can be modified 

### The above diameter classes will be represented by numbers 1, 2, 3, ... 14. 

##For instance, a DBH value between 0 and 10 will be represented as 1, between 10 and 20 will be represented by 2 and so on

AlltreeData$DCLASS <- DCF(AlltreeData$DBH) ##Now using the DCF function to create the diameter classes and this will fill the DCLASS column created earlier
head(AlltreeData)

## For clarification: The diameter classes are represented below:

## 0 - 10cm = 1, 10 - 20cm = 2, 20 - 30cm = 3, 20 - 40cm = 4, 40 - 50cm = 5, 50 - 60cm = 6, 60 - 70cm = 7,
## 70 - 80cm = 8, 80 - 90cm = 9, 90 - 100cm = 10, 100 - 110cm = 11, 110 - 120cm = 12, 120 - 130cm = 13


summary(AlltreeData$DCLASS)

## Calculate plot basal area/ha by diameter class

Baha_1stDC <- ifelse(AlltreeData$DCLASS == "1", 1, 0) ## Here i am creating an indicator variables for all the class intervals
Baha_2ndDC <- ifelse(AlltreeData$DCLASS == "2", 1, 0)
Baha_3rdDC <- ifelse(AlltreeData$DCLASS == "3", 1, 0)
Baha_4thDC <- ifelse(AlltreeData$DCLASS == "4", 1, 0)
Baha_5thDC <- ifelse(AlltreeData$DCLASS == "5", 1, 0)
Baha_6thDC <- ifelse(AlltreeData$DCLASS == "6", 1, 0)
Baha_7thDC <- ifelse(AlltreeData$DCLASS == "7", 1, 0)
Baha_8thDC <- ifelse(AlltreeData$DCLASS == "8", 1, 0)
Baha_9thDC <- ifelse(AlltreeData$DCLASS == "9", 1, 0)
Baha_10thDC <- ifelse(AlltreeData$DCLASS == "10", 1, 0)
Baha_11thDC <- ifelse(AlltreeData$DCLASS == "11", 1, 0)
Baha_12thDC <- ifelse(AlltreeData$DCLASS == "12", 1, 0)
Baha_13thDC <- ifelse(AlltreeData$DCLASS == "13", 1, 0)
## Now this i have created the indicator variables for all the class intervals


PlotBaha_1stDC <- AlltreeData$Baha * Baha_1stDC ## Calculate the plot basal area/ha for individual class interval
PlotBaha_2ndDC <- AlltreeData$Baha * Baha_2ndDC
PlotBaha_3rdDC <- AlltreeData$Baha * Baha_3rdDC
PlotBaha_4thDC <- AlltreeData$Baha * Baha_4thDC
PlotBaha_5thDC <- AlltreeData$Baha * Baha_5thDC
PlotBaha_6thDC <- AlltreeData$Baha * Baha_6thDC
PlotBaha_7thDC <- AlltreeData$Baha * Baha_7thDC
PlotBaha_8thDC <- AlltreeData$Baha * Baha_8thDC
PlotBaha_9thDC <- AlltreeData$Baha * Baha_9thDC
PlotBaha_10thDC <- AlltreeData$Baha * Baha_10thDC
PlotBaha_11thDC <- AlltreeData$Baha * Baha_11thDC
PlotBaha_12thDC <- AlltreeData$Baha * Baha_12thDC
PlotBaha_13thDC <- AlltreeData$Baha * Baha_13thDC

sum(AlltreeData$Baha, na.rm = TRUE) ## Summation of the total plot basal area/ha for the AlltreeData
sum(PlotBaha_1stDC, na.rm = TRUE) ## Summation of the total plot basal area/ha for the first class interval, the rest are for the subsequent class intervals
sum(PlotBaha_2ndDC, na.rm = TRUE)
sum(PlotBaha_3rdDC, na.rm = TRUE)
sum(PlotBaha_4thDC, na.rm = TRUE)
sum(PlotBaha_5thDC, na.rm = TRUE)
sum(PlotBaha_6thDC, na.rm = TRUE)
sum(PlotBaha_7thDC, na.rm = TRUE)
sum(PlotBaha_8thDC, na.rm = TRUE)
sum(PlotBaha_9thDC, na.rm = TRUE)
sum(PlotBaha_10thDC, na.rm = TRUE)
sum(PlotBaha_11thDC, na.rm = TRUE)
sum(PlotBaha_12thDC, na.rm = TRUE)
sum(PlotBaha_13thDC, na.rm = TRUE)

### I want to sum the plot basal area/ha for all diameter classes to check if it equals to the total plot basal area/ha for the entire stand.

sum(PlotBaha_1stDC, PlotBaha_2ndDC, PlotBaha_3rdDC, PlotBaha_4thDC, PlotBaha_5thDC, 
    PlotBaha_6thDC, PlotBaha_7thDC, PlotBaha_8thDC, PlotBaha_9thDC, PlotBaha_10thDC, 
    PlotBaha_11thDC, PlotBaha_12thDC, PlotBaha_13thDC, na.rm = TRUE)

sum(AlltreeData$Baha, na.rm = TRUE) ## The total basal area/ha by diameter is equal to the total plot basal area/ha for the entire stand.

### I will now filter the control portion of block 1, 2 and 3 from the AlltreeData. This separates the control from the thinned in the each block

Block1_Control <- AlltreeData %>%  ### For block 1
  filter(BLOCK == 1, TREAT == 'C')
Block1_Control

Block2_Control <- AlltreeData %>%  ### For block 2
  filter(BLOCK == 2, TREAT == 'C')
Block2_Control

Block3_Control <- AlltreeData %>%  ### For block 3
  filter(BLOCK == 3, TREAT == 'C')
Block3_Control

Block1_Control

sum(Block3_Control$BTPH,na.rm = TRUE)

### I want to set up indicator variables (Dummy variable) for live (L) and dead (D) trees for the 3 blocks starting with block_1

Block1_Control$L <- ifelse(Block1_Control$LDSTATUS == "L",1,0)
Block1_Control$D <- ifelse(Block1_Control$LDSTATUS == "D",1,0)

Block1_Control$Baha_L <- Block1_Control$Baha * Block1_Control$L ###Dummy variables for LDSTATUS for Block 1
Block1_Control$Baha_D <- Block1_Control$Baha * Block1_Control$D

sum(Block1_Control$Baha, na.rm = TRUE)  ###Summation of the Baha for live and dead to check if they both equal to total for Block_1
sum(Block1_Control$Baha_L, na.rm = TRUE)
sum(Block1_Control$Baha_D, na.rm = TRUE)
sum(Block1_Control$Baha_L, Block1_Control$Baha_D, na.rm = TRUE)### The two adds up to the total Basal area per ha in block_1

### i will repeat the above for both block 2 and 3 control

##For Block2_Control

Block2_Control$L <- ifelse(Block2_Control$LDSTATUS == "L",1,0)
Block2_Control$D <- ifelse(Block2_Control$LDSTATUS == "D",1,0)

Block2_Control$Baha_L <- Block2_Control$Baha * Block2_Control$L ###Dummy variables for LDSTATUS for Block 2
Block2_Control$Baha_D <- Block2_Control$Baha * Block2_Control$D

sum(Block2_Control$Baha, na.rm = TRUE)  ###Summation of the Baha for live and dead to check if they both equal to total for Block_2
sum(Block2_Control$Baha_L, na.rm = TRUE)
sum(Block2_Control$Baha_D, na.rm = TRUE)
sum(Block2_Control$Baha_L, Block2_Control$Baha_D, na.rm = TRUE)

##For Block3_Control

Block3_Control$L <- ifelse(Block3_Control$LDSTATUS == "L",1,0)
Block3_Control$D <- ifelse(Block3_Control$LDSTATUS == "D",1,0)

Block3_Control$Baha_L <- Block3_Control$Baha * Block3_Control$L ###Dummy variables for LDSTATUS for Block 3
Block3_Control$Baha_D <- Block3_Control$Baha * Block3_Control$D

sum(Block3_Control$Baha, na.rm = TRUE)  ###Summation of the Baha for live and dead to check if they both equal to total for Block_3
sum(Block3_Control$Baha_L, na.rm = TRUE)
sum(Block3_Control$Baha_D, na.rm = TRUE)
sum(Block3_Control$Baha_L, Block3_Control$Baha_D, na.rm = TRUE)




### Plotting diameter distributiin by treatment and control

AlltreeData_Control <- AlltreeData %>%  
  filter(TREAT == 'C')
AlltreeData_Control

cols.num <- c("DBH", "HT", "PTPH", "BTPH") 
AlltreeData_Control[cols.num] <- sapply(AlltreeData_Control[cols.num], as.numeric) 
sapply(AlltreeData_Control, class)

hist(AlltreeData_Control$DBH, freq = FALSE, ann = FALSE, xlim = c(0, 100))
title(main = "Diameter distribution (Control)", xlab = "DBH", ylab = "Density")
lines(density(na.omit(AlltreeData_Control$DBH)), lwd = 2, col ="red")

class(AlltreeData_Control$DBH)

AlltreeData_Treatment <- AlltreeData %>%  
  filter(TREAT == 'T')
AlltreeData_Treatment

cols.num <- c("DBH", "HT", "PTPH", "BTPH") 
AlltreeData_Treatment[cols.num] <- sapply(AlltreeData_Treatment[cols.num], as.numeric) 
sapply(AlltreeData_Treatment, class)

hist(AlltreeData_Treatment$DBH, freq = FALSE, ann = FALSE, xlim = c(0, 100))
title(main = "Diameter distribution (Treatment)", xlab = "DBH", ylab = "Density")
lines(density(na.omit(AlltreeData_Treatment$DBH)), lwd = 2, col ="blue")