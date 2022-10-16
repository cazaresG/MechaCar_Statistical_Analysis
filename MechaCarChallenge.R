
##########################
#Deliverable 1
# Use the library() function to load the dplyr package.
library(tidyverse)
#Import and read in the MechaCar_mpg.csv file as a dataframe.
mechacar_df <- read.csv(file = "data/MechaCar_mpg.csv", check.names=F,stringsAsFactors = F)
#Perform linear regression using the lm() function. In the lm() function, pass in all six variables (i.e., columns), and add the dataframe you created in Step 4 as the data parameter.
mechacar_reg = lm(mpg ~ vehicle_length  + vehicle_weight + spoiler_angle + ground_clearance + AWD,data=mechacar_df)
mechacar_reg
#Using the summary() function, determine the p-value and the r-squared value for the linear regression model.
summary(mechacar_reg)
#Save your MechaCarChallenge.RScript file to your GitHub repository.

##########################
#Deliverable 2
#The Suspension_Coil.csv file is imported and read into a dataframe (5 pt)
SuspCoil_df <- read.csv("data/Suspension_Coil.csv", check.names = F, stringsAsFactors =F )
#An RScript is written to create a total summary dataframe that has the mean, median, variance, and standard deviation of the PSI for all manufacturing lots
SuspCoil_summary <- SuspCoil_df %>% summarize(Mean=mean(PSI), Median=median(PSI), Variance=var(PSI), SD=sd(PSI))
SuspCoil_summary
#Write an RScript that creates a lot_summary dataframe using the group_by() and the summarize() functions to group each manufacturing lot by the mean, median, variance, and standard deviation of the suspension coil’s PSI column
Coil_Lot_summary <- SuspCoil_df %>% group_by(Manufacturing_Lot)%>% summarize(Mean=mean(PSI), Median=median(PSI), Variance=var(PSI), SD=sd(PSI))
Coil_Lot_summary

#########################
#Deliverable 3

#In your MechaCarChallenge.RScript, write an RScript using the t.test() function to determine if the PSI across all manufacturing lots is statistically different from the population mean of 1,500 pounds per square inch.
t.test(SuspCoil_df$PSI,mu=mean(SuspCoil_df$PSI))
# write three more RScripts in your MechaCarChallenge.RScript using the t.test() function and its subset() argument to determine if the PSI for each manufacturing lot is statistically different from the population mean of 1,500 pounds per square inch.
coilSubset1 <- subset(SuspCoil_df, Manufacturing_Lot == "Lot1")
coilSubset2 <- subset(SuspCoil_df, Manufacturing_Lot == "Lot2")
coilSubset3 <- subset(SuspCoil_df, Manufacturing_Lot == "Lot3")
t.test(coilSubset1$PSI,mu=mean(SuspCoil_df$PSI))
t.test(coilSubset2$PSI,mu=mean(SuspCoil_df$PSI))
t.test(coilSubset3$PSI,mu=mean(SuspCoil_df$PSI))
