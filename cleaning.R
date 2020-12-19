#Set the working data
setwd("/Users/malia/Downloads/Working")

## Load the importable data
load("ImportedData")

## Create a datafile of only relevant variables

clean.data <- data.frame(Income98 = ordered(import.data$rincom98),
				Income06 = ordered(import.data$rincom06),
				SexPart5 = ordered(import.data$sexsex5),
				Sex = ordered(import.data$sex),
				Race = ordered(import.data$race),
				Weekswrk = import.data$weekswrk,
				Hrswrk = import.data$hrs1,
				Degree = ordered(import.data$degree),
				Age = import.data$age,
				EducYrs = import.data$educ,
				Married = ordered(import.data$marital),
				NumChilds = ordered(import.data$childs),
				PlaceSize = ordered(import.data$xnorcsiz),
				Region = ordered(import.data$region),
				Indus80 = import.data$indus80,
				Indus10 = import.data$indus10,
				Occ80 = import.data$occ80,
				Occ10 = import.data$occ10,
				Year = ordered(import.data$year)
				)

n <- nrow(clean.data)


#Create Income Variable, first transform all the NA's into 999999
Inc98 <- ifelse(is.na(clean.data$Income98), 9999, clean.data$Income98)
Inc06 <- ifelse(is.na(clean.data$Income06), 9999, clean.data$Income06)

Income <- rep(NA, n)

Income <- ifelse(Inc98 == 1, 500, Income)
                        Income <-       ifelse(Inc06 == 1, 500, Income)
Income <- ifelse(Inc98 == 2, 2000, 
                              ifelse(Inc06 == 2, 2000, Income))
Income <- ifelse(Inc98 == 3, 3500, 
                              ifelse(Inc06 == 3, 3500, Income)) 
Income <- ifelse(Inc98 == 4, 4500, 
                              ifelse(Inc06 == 4, 4500, Income)) 
Income <- ifelse(Inc98 == 5, 5500, 
                              ifelse(Inc06 == 5, 5500, Income)) 
Income <- ifelse(Inc98 == 6, 6500, 
                              ifelse(Inc06 == 6, 6500, Income)) 
Income <- ifelse(Inc98 == 7, 7500, 
                              ifelse(Inc06 == 7, 7500, Income)) 
Income <- ifelse(Inc98 == 8, 9000, 
                              ifelse(Inc06 == 8, 9000, Income)) 
Income <- ifelse(Inc98 == 9, 11250, 
                              ifelse(Inc06 == 9, 11250, Income)) 
Income <- ifelse(Inc98 == 10, 13750, 
                              ifelse(Inc06 == 10, 13750, Income)) 
Income <- ifelse(Inc98 == 11, 16250, 
                              ifelse(Inc06 == 11, 16250, Income))
Income <- ifelse(Inc98 == 12, 18750, 
                              ifelse(Inc06 == 12, 18750, Income))
Income <- ifelse(Inc98 == 13, 21250, 
                              ifelse(Inc06 == 13, 21250, Income))
Income <- ifelse(Inc98 == 14, 23750, 
                              ifelse(Inc06 == 14, 23750, Income))
Income <- ifelse(Inc98 == 15, 27500, 
                              ifelse(Inc06 == 15, 27500, Income))
Income <- ifelse(Inc98 == 16, 32500, 
                              ifelse(Inc06 == 16, 32500, Income))
Income <- ifelse(Inc98 == 17, 37500, 
                              ifelse(Inc06 == 17, 37500, Income))
Income <- ifelse(Inc98 == 18, 45000, 
                              ifelse(Inc06 == 18, 45000, Income))
Income <- ifelse(Inc98 == 19, 55000, 
                              ifelse(Inc06 == 19, 55000, Income))
Income <- ifelse(Inc98 == 20, 67500, 
                              ifelse(Inc06 == 20, 67500, Income))
Income <- ifelse(Inc98 == 21, 82500, 
                              ifelse(Inc06 == 21, 82500, Income))
Income <- ifelse(Inc98 == 22, 100000, 
                              ifelse(Inc06 == 22, 100000, Income))
Income <- ifelse(Inc98 == 23, 120000, 
                              ifelse(Inc06 == 23, 120000, Income))
Income <- ifelse(Inc06 == 24, 120000, Income)
Income <- ifelse(Inc06 == 25, 120000, Income)

#Create the log of income
LogIncome <- log(Income)				

#Create a dummy variable for sexes
Female <- ifelse(clean.data$Sex == 2, 1, 0)
Male <- ifelse(clean.data$Sex == 1, 1, 0)


#Create a dummy variable for sexual orientation: Queer
Queer <- rep(0, n)
Queer <- ifelse(clean.data$SexPart5 == 1 & Female == 0, 1, Queer)
Queer <- ifelse(clean.data$SexPart5 == 2 & Female == 0, 1, Queer)
Queer <- ifelse(clean.data$SexPart5 == 2 & Female == 1, 1, Queer)
Queer <- ifelse(clean.data$SexPart5 == 3 & Female == 1, 1, Queer)
Queer <- ifelse(is.na(clean.data$SexPart5), NA, Queer)

#Create a dummy variable for race: Nonwhite
Nonwhite <- ifelse(clean.data$Race == 1, 0, 1)

#Dummy code the degree category by creating dummies for degree levels
Lesshs <- ifelse(clean.data$Degree == 0, 1, 0)
Hs <- ifelse(clean.data$Degree == 1, 1, 0)
Assoc <- ifelse(clean.data$Degree == 2, 1, 0)
Bach <- ifelse(clean.data$Degree == 3, 1, 0)
Grad <- ifelse(clean.data$Degree == 4, 1, 0)

#Create a proxy for potential experience and its square, drop negative values (because negative experience levels does not make sense)
Exper <- clean.data$Age - (6 + clean.data$EducYrs)
Exper <- ifelse(Exper < 0, NA, Exper)
Exper_sq <- Exper*Exper

#Create a dummy variable for marital status: Married
Married <- ifelse(clean.data$Married == 1, 1, 0)

#Create a dummy variable for residing in a metropolitan area
Metro <- ifelse(clean.data$PlaceSize == 1, 1, 0)

#Generate regional codes
Northeast <- rep(0, n)
Northeast <- ifelse(clean.data$Region == 1, 1, Northeast)
Northeast <- ifelse(clean.data$Region == 2, 1, Northeast)
Midwest <- rep(0,n)
Midwest <- ifelse(clean.data$Region == 3, 1, Midwest)
Midwest <- ifelse(clean.data$Region == 4, 1, Midwest)
South <- rep(0,n)
South <- ifelse(clean.data$Region == 5, 1, South)
South <- ifelse(clean.data$Region == 6, 1, South)
South <- ifelse(clean.data$Region == 7, 1, South)
West <- rep(0,n)
West <- ifelse(clean.data$Region == 8, 1, West)
West <- ifelse(clean.data$Region == 9, 1, West)

#Generate Industry Codes, first transform all the NA's into 999999, and then create the 
#dummies for industry
Indus80 <- ifelse(is.na(clean.data$Indus80), 999999, clean.data$Indus80)
Indus10 <- ifelse(is.na(clean.data$Indus10), 999999, clean.data$Indus10)

Agri <- rep(0, n)
Agri <- ifelse(Indus10 >= 170 & Indus10 <= 290, 1, Agri)
Agri <- ifelse(Indus80 >= 10 & Indus80 <= 31, 1, Agri)
Agri <- ifelse(Indus10 == 999999 & Indus80 == 999999, NA, Agri)

Mine <- rep(0, n)
Mine <- ifelse(Indus10 >= 370 & Indus10 <= 490, 1, Mine)
Mine <- ifelse(Indus80 >= 40 & Indus80 <= 50, 1, Mine)
Mine <- ifelse(Indus10 == 999999 & Indus80 == 999999, NA, Mine)

Construct <- rep(0, n)
Construct <- ifelse(Indus10 == 770, 1, Construct)
Construct <- ifelse(Indus80 == 060, 1, Construct)
Construct <- ifelse(Indus10 == 999999 & Indus80 == 999999, NA, Construct)

Manu <- rep(0, n)
Manu <- ifelse(Indus10 >= 1070 & Indus10 <= 3990, 1, Manu )
Manu <- ifelse(Indus10 >= 6470 & Indus10 <= 6490, 1, Manu )
Manu <- ifelse(Indus80 >= 100 & Indus80 <= 392, 1, Manu )
Manu <- ifelse(Indus10 == 999999 & Indus80 == 999999, NA, Manu )

Trans <- rep(0, n)
Trans <- ifelse(Indus10 >= 570 & Indus10 <= 690, 1, Trans )
Trans <- ifelse(Indus10 >= 6070 & Indus10 <= 6390, 1, Trans )
Trans <- ifelse(Indus10 >= 6570 & Indus10 <= 6690, 1, Trans )
Trans <- ifelse(Indus80 >= 400 & Indus80 <= 472, 1, Trans )
Trans <- ifelse(Indus10 == 999999 & Indus80 == 999999, NA, Trans )

Trade <- rep(0, n)
Trade <- ifelse(Indus10 >= 4070 & Indus10 <= 5790, 1, Trade )
Trade <- ifelse(Indus10 >= 8680 & Indus10 <= 8690, 1, Trade )
Trade <- ifelse(Indus80 >= 500 & Indus80 <= 691, 1, Trade )
Trade <- ifelse(Indus10 == 999999 & Indus80 == 999999, NA, Trade )

Finance <- rep(0, n)
Finance <- ifelse(Indus10 >= 6870 & Indus10 <= 7190, 1, Finance )
Finance <- ifelse(Indus80 >= 700 & Indus80 <= 712, 1, Finance )
Finance <- ifelse(Indus10 == 999999 & Indus80 == 999999, NA, Finance )

Busi <- rep(0, n)
Busi <- ifelse(Indus10 == 7570, 1, Busi )
Busi <- ifelse(Indus10 >= 7770 & Indus10 <= 7790, 1, Busi )
Busi <- ifelse(Indus10 >= 7580 & Indus10 <= 7780, 1, Busi )
Busi <- ifelse(Indus10 >= 8770 & Indus10 <= 8870, 1, Busi )
Busi <- ifelse(Indus80 >= 721 & Indus80 <= 760, 1, Busi )
Busi <- ifelse(Indus10 == 999999 & Indus80 == 999999, NA, Busi )

Person <- rep(0, n)
Person <- ifelse(Indus10 >= 8660 & Indus10 <= 8670, 1, Person )
Person <- ifelse(Indus10 >= 8880 & Indus10 <= 9090, 1, Person )
Person <- ifelse(Indus80 >= 761 & Indus80 <= 791, 1, Person )
Person <- ifelse(Indus10 == 999999 & Indus80 == 999999, NA, Person )

Enter <- rep(0, n)
Enter <- ifelse(Indus10 >= 8560 & Indus10 <= 8590, 1, Enter )
Enter <- ifelse(Indus80 >= 800 & Indus80 <= 802, 1, Enter )
Enter <- ifelse(Indus10 == 999999 & Indus80 == 999999, NA, Enter )

Prof <- rep(0, n)
Prof <- ifelse(Indus10 >= 6770 & Indus10 <= 6780, 1, Prof )
Prof <- ifelse(Indus10 >= 7270 & Indus10 <= 7490, 1, Prof )
Prof <- ifelse(Indus10 >= 7860 & Indus10 <= 8470, 1, Prof )
Prof <- ifelse(Indus10 >= 9160 & Indus10 <= 9190, 1, Prof )
Prof <- ifelse(Indus80 >= 812 & Indus80 <= 892, 1, Prof )
Prof  <- ifelse(Indus10 == 999999 & Indus80 == 999999, NA, Prof )

Public <- rep(0, n)
Public <- ifelse(Indus10 >= 9370 & Indus10 <= 9590, 1, Public )
Public <- ifelse(Indus10 >= 9670 & Indus10 <= 9870, 1, Public )
Public <- ifelse(Indus80 >= 900 & Indus80 <= 932, 1, Public )
Public <- ifelse(Indus10 == 999999 & Indus80 == 999999, NA, Public )

#Generate Occupation Codes, first transform all the NA's into 999999, and then create the 
#dummies for occupation
Occ80 <- ifelse(is.na(clean.data$Occ80), 999999, clean.data$Occ80)
Occ10 <- ifelse(is.na(clean.data$Occ10), 999999, clean.data$Occ10)

Manprof <- rep(0,n)
Manprof <- ifelse(Occ10 >= 10 & Occ10 <= 3540, 1, Manprof )
Manprof <- ifelse(Occ80 >= 3 & Occ80 <= 235, 1, Manprof )
Manprof <- ifelse(Occ80 >= 473 & Occ80 <= 476, 1, Manprof )
Manprof <- ifelse(Occ10 == 999999 & Occ80 == 999999, NA, Manprof )

Service <- rep(0,n)
Service <- ifelse(Occ10 >= 3600 & Occ10 <= 4650, 1, Service )
Service <- ifelse(Occ80 >= 403 & Occ80 <= 469, 1, Service )
Service <- ifelse(Occ80 == 487, 1, Service )
Service <- ifelse(Occ10 == 999999 & Occ80 == 999999, NA, Service )

Office <- rep(0,n)
Office <- ifelse(Occ10 >= 4700 & Occ10 <= 5940, 1, Office )
Office <- ifelse(Occ80 >= 243 & Occ80 <= 389, 1, Office )
Office <- ifelse(Occ10 == 999999 & Occ80 == 999999, NA, Office )

Natconst <- rep(0,n)
Natconst <- ifelse(Occ10 >= 6000 & Occ10 <= 7630, 1, Natconst )
Natconst <- ifelse(Occ80 >= 477 & Occ80 <= 486, 1, Natconst )
Natconst <- ifelse(Occ80 >= 488 & Occ80 <= 496, 1, Natconst )
Natconst <- ifelse(Occ80 >= 498 & Occ80 <= 616, 1, Natconst )
Natconst <- ifelse(Occ10 == 999999 & Occ80 == 999999, NA, Natconst )

Prodlabor <- rep(0,n)
Prodlabor <- ifelse(Occ10 >= 7700 & Occ10 <= 9750, 1, Prodlabor )
Prodlabor <- ifelse(Occ80 >= 617 & Occ80 <= 889, 1, Prodlabor )
Prodlabor <- ifelse(Occ80 == 497, 1, Prodlabor )
Prodlabor <- ifelse(Occ10 == 999999 & Occ80 == 999999, NA, Prodlabor )


#Create the final dataframe with variables of interest, but dataframe contains NAs

final.data <- data.frame(Income, LogIncome,
				Weekswrk = clean.data$Weekswrk, Hrswrk = clean.data$Hrswrk, 
				Queer = factor(Queer), Female = factor(Female), Nonwhite = factor(Nonwhite),
				Lesshs = factor(Lesshs), Hs = factor(Hs), Assoc = factor(Assoc),
					 Bach = factor(Bach), Grad = factor(Grad),
				Age = clean.data$Age, Exper, Exper_sq,
				Married = factor(Married), Metro = factor(Metro),
				Northeast = factor(Northeast), Midwest = factor(Midwest), South = factor(South), West = factor(West),
				Agri = factor(Agri), Mine = factor(Mine), Construct = factor(Construct), Manu = factor(Manu),
					Trans = factor(Trans), Trade = factor(Trade), Finance = factor(Finance), Busi = factor(Busi),
					Person = factor(Person), Enter = factor(Enter), Prof = factor(Prof), Public = factor(Public),
				Manprof = factor(Manprof), Service = factor(Service), Office = factor(Office), 
					Natconst = factor(Natconst), Prodlabor = factor(Prodlabor)
				)
#Remove NAs from the final dataframe, to create the truly final dataframe

final.data <- na.omit(final.data)

#Save final.data as the final data file to be used for the analysis of the paper
save(final.data, file = "FinalData")


				