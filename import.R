#Set the working data
setwd("/Users/malia/Downloads/Working")

#Call the foreign package to read the original .dta Stata files
library(foreign)

#Import each original data file. Note, to import the data, I drop all label names in Stata
data.1998 <- read.dta(paste(getwd(),"/Data/Importable Data/importable_GSS1998.dta", sep = ""), convert.factors = TRUE, convert.underscore = TRUE)
data.2000 <- read.dta(paste(getwd(),"/Data/Importable Data/importable_GSS2000.dta", sep = ""), convert.factors = TRUE, convert.underscore = TRUE)
data.2002 <- read.dta(paste(getwd(),"/Data/Importable Data/importable_GSS2002.dta", sep = ""), convert.factors = TRUE, convert.underscore = TRUE)
data.2004 <- read.dta(paste(getwd(),"/Data/Importable Data/importable_GSS2004.dta", sep = ""), convert.factors = TRUE, convert.underscore = TRUE)
data.2006 <- read.dta(paste(getwd(),"/Data/Importable Data/importable_GSS2006.dta", sep = ""), convert.factors = TRUE, convert.underscore = TRUE)
data.2008 <- read.dta(paste(getwd(),"/Data/Importable Data/importable_GSS2008.dta", sep = ""), convert.factors = TRUE, convert.underscore = TRUE)
data.2010 <- read.dta(paste(getwd(),"/Data/Importable Data/importable_GSS2010.dta", sep = ""), convert.factors = TRUE, convert.underscore = TRUE)
data.2012 <- read.dta(paste(getwd(),"/Data/Importable Data/importable_GSS2012.dta", sep = ""), convert.factors = TRUE, convert.underscore = TRUE)

#Call the plyr package to use the rbind.fill command, so that you can use rbind with different numbers of columns
library(plyr)

#Use the rbind.fill command to append each year's dataset into one full dataset, and save the new dataset
import.data <- rbind.fill(data.1998, data.2000, data.2002, data.2004, data.2006, data.2008, data.2010, data.2012)

#Save the final data file as the basic, importable data file before any changes are made
save(import.data, file = "ImportedData")
