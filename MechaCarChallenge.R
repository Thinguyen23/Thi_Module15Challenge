library(tidyverse)

## MPG Regression
# Read file
mecha_mpg <- read.csv(file='MechaCar_mpg.csv', check.names=T, stringsAsFactors = F)
#generate multiple linear regression model
lm(mpg ~ vehicle.length + vehicle.weight + spoiler.angle + ground.clearance + AWD,data=mecha_mpg)
# generate summary statictics
summary(lm(mpg ~ vehicle.length + vehicle.weight + spoiler.angle + ground.clearance + AWD,data=mecha_mpg))


## Suspension Coil Summary
# Read file
suspension_coil <- read.csv(file='Suspension_Coil.csv', check.names = F, stringsAsFactors = F)
# Create summary statistics table for suspension coil's PSI
summary_table <- suspension_coil %>% group_by(Manufacturing_Lot) %>% summarise(Mean_PSI=mean(PSI), Median_PSI=median(PSI), Variane=var(PSI), Standard_Deviation=sd(PSI))

##Suspension Coil T-Test
# Create subsets
population1 = subset(suspension_coil, Manufacturing_Lot =="Lot1")
population2 = subset(suspension_coil, Manufacturing_Lot =="Lot2")
population3 = subset(suspension_coil, Manufacturing_Lot =="Lot3")
# Calculate Independent (one Sample) T-Test
t.test(population1[['PSI']], mu=1500)
t.test(population2[['PSI']], mu=1500)
t.test(population3[['PSI']], mu=1500)
