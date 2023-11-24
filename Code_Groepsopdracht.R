#-----------------------------------------------------------------------------
# Authors: Nynke van Koningsveld, Suzanne Poelgeest, Isabelle Quartel,
# Olivier van Warmerdam, David nog iets.
#-----------------------------------------------------------------------------

#-----------------------------------------------------------------------------
# Clear the global environment
#-----------------------------------------------------------------------------

remove(list=ls())
cat("\f")

#-----------------------------------------------------------------------------
# Install necessary packages
#-----------------------------------------------------------------------------

# install.packages("ggplot2", dependencies = TRUE)
# install.packages("RColorBrewer", dependencies = TRUE)
# install.packages("psych", dependencies = TRUE)
# install.packages("stargazer", dependencies = TRUE)
# install.packages("Hmisc", dependencies = TRUE)
# install.packages("corrgram", dependencies = TRUE)
# install.packages("corrplot", dependencies = TRUE)
# install.packages("ppcor", dependencies = TRUE)
# install.packages("gmodels", dependencies = TRUE)
# install.packages("dplyr", dependencies = TRUE)
# install.packages("HH", dependencies = TRUE)
# install.packages("ggmap", dependencies = TRUE)

#-----------------------------------------------------------------------------
# Load packages
#-----------------------------------------------------------------------------

library(ggplot2)
library(RColorBrewer)
library(ggmap)
library(ppcor)
library(stargazer)
library(psych)
library(plyr)
library(Hmisc)
library(corrgram)
library(corrplot)
library(gmodels)
library(dplyr)
library(HH)

#-----------------------------------------------------------------------------
# Set directories
#-----------------------------------------------------------------------------

dir <- "C:/Users/nynke/OneDrive/Studie/3. Rotterdam School of Management, Erasmus University/3 Master/02 BM21MIM-P Business Analytics/Groepsopdracht/R Code/"

dirData <- paste0(dir, "Data/")
dirProg <- paste0(dir, "Programs/")
dirRslt <- paste0(dir, "Results/")

#-----------------------------------------------------------------------------
# Read the necessary file
#-----------------------------------------------------------------------------
# Read the data from csv file
dfScooters<- read.csv(file = paste0(dirData, "deelscooters.csv"), 
                   stringsAsFactors = FALSE)

#-----------------------------------------------------------------------------
# Preliminaries
#-----------------------------------------------------------------------------
# Remove 'redundant' columns ( because they will not be used )
dfScooters$EndDate <- NULL
dfScooters$Status <- NULL
dfScooters$StartDate <- NULL
dfScooters$IPAddress <- NULL
dfScooters$Progress<- NULL
dfScooters$Duration..in.seconds. <- NULL
dfScooters$Finished <- NULL
dfScooters$RecordedDate <- NULL
dfScooters$ResponseId <- NULL
dfScooters$RecipientLastName <- NULL
dfScooters$RecipientFirstName <- NULL
dfScooters$RecipientEmail <- NULL
dfScooters$ExternalReference <- NULL
dfScooters$LocationLatitude <- NULL
dfScooters$LocationLongitude <- NULL
dfScooters$DistributionChannel <- NULL
dfScooters$UserLanguage <- NULL
dfScooters$Intro <- NULL

# Delete rows 1 - 3
dfScooters <- dfScooters[-c(1,2,3), ]

# Change column names
colnames(dfScooters)[colnames(dfScooters)=="Vraag.1"]<- "Geslacht"
colnames(dfScooters)[colnames(dfScooters)=="Vraag.3"]<- "Vervoersmiddelen"
colnames(dfScooters)[colnames(dfScooters)=="Q10"]<- "Woonplaats"
colnames(dfScooters)[colnames(dfScooters)=="Vraag.2"]<- "Leeftijd"
colnames(dfScooters)[colnames(dfScooters)=="Q10.1"]<- "Deelscooter_gebruikt?"
colnames(dfScooters)[colnames(dfScooters)=="Q11"]<- "Deelscooter2023"
colnames(dfScooters)[colnames(dfScooters)=="Q12"]<- "Deelscooter2022"

# Switch Variables to factor
dfScooters$fgeslacht <- factor(dfScooters$Geslacht, 
              levels = c("1", "2"), labels = c("Man", "Vrouw"))

dfScooters$fwoonplaats <- factor(dfScooters$Woonplaats, 
                               levels = c("1", "2", "3", "4", "5"), 
                               labels = c("Stedelijk gebied", "Buitenwijk van een stad",
                                          "Grotere plaats", "Kleinere plaats", "Platteland"))

dfScooters$fvervoersmiddelen <- factor(dfScooters$Vervoersmiddelen, 
                               levels = c("1", "2", "3", "4", "5"), 
                               labels = c("Auto", "Scooter/bromfiets",
                                          "Elektrische fiets", "Gewone fiets (niet-elektrisch)", 
                                          "Anders"))

#Switch to numeric values
dfScooters$Q5_1 <- as.numeric(dfScooters$Q5_1)
dfScooters$Q5_2 <- as.numeric(dfScooters$Q5_2)
dfScooters$Q5_3 <- as.numeric(dfScooters$Q5_3)
dfScooters$Q5_4 <- as.numeric(dfScooters$Q5_4)
dfScooters$Q5_5 <- as.numeric(dfScooters$Q5_5)
dfScooters$Q5_6 <- as.numeric(dfScooters$Q5_6)
dfScooters$Q5_7 <- as.numeric(dfScooters$Q5_7)
dfScooters$Q5_8 <- as.numeric(dfScooters$Q5_8)
dfScooters$Q6_1 <- as.numeric(dfScooters$Q6_1)
dfScooters$Q6_2 <- as.numeric(dfScooters$Q6_2)
dfScooters$Q6_3 <- as.numeric(dfScooters$Q6_3)
dfScooters$Q6_4 <- as.numeric(dfScooters$Q6_4)
dfScooters$Q6_5 <- as.numeric(dfScooters$Q6_5)
dfScooters$Q6_6 <- as.numeric(dfScooters$Q6_6)
dfScooters$Q6_7 <- as.numeric(dfScooters$Q6_7)
dfScooters$Q6_8 <- as.numeric(dfScooters$Q6_8)
dfScooters$Q6_9 <- as.numeric(dfScooters$Q6_9)
dfScooters$Leeftijd <- as.numeric(dfScooters$Leeftijd)
dfScooters$Deelscooter2022 <- as.numeric(dfScooters$Deelscooter2022)
dfScooters$Deelscooter2023 <- as.numeric(dfScooters$Deelscooter2023)

# Change NA values to 0
dfScooters$Deelscooter2022 <- replace(dfScooters$Deelscooter2022, is.na(dfScooters$Deelscooter2022), 0)
dfScooters$Deelscooter2023 <- replace(dfScooters$Deelscooter2023, is.na(dfScooters$Deelscooter2023), 0)

#-----------------------------------------------------------------------------
# Univariate analysis
#-----------------------------------------------------------------------------
# Cronbach's Alpha for NEP
library(psych)
psych::alpha(dfScooters[c("Q6_1", "Q6_2", "Q6_3", 
                    "Q6_4", "Q6_5", "Q6_6", "Q6_7", "Q6_8", "Q6_9")],
      keys=c("Q6_1", "Q6_5", "Q6_7", "Q6_9"))

# Cronbach's Alpha for monetary value
psych:: alpha(dfScooters[c("Q5_1", "Q5_2", "Q5_3","Q5_4", 
                           "Q5_5", "Q5_6", "Q5_7", "Q5_8")])

# Add scores to dataframe
dfScooters$NEP <- psych::alpha(dfScooters[c("Q6_1", "Q6_2", "Q6_3", 
                                            "Q6_4", "Q6_5", "Q6_6", "Q6_7", "Q6_8", "Q6_9")],
                               keys=c("Q6_1", "Q6_5", "Q6_7", "Q6_9"),
                       cumulative = FALSE)$scores

dfScooters$geld <- psych:: alpha(dfScooters[c("Q5_1", "Q5_2", "Q5_3","Q5_4", 
                                              "Q5_5", "Q5_6", "Q5_7", "Q5_8")], cumulative = FALSE)$scores

#-----------------------------------------------------------------------------
# Multivariate analysis I: Correlation
#-----------------------------------------------------------------------------
# Overview of the corrleation coefficients of all variables
# TODO: moeten we hier nog een subset maken van alleen de kwantitatieve variabelen?
cor(dfScooters)

# Compare to Spearman's method
# TODO: moeten we hier nog een subset maken van alleen de kwantitatieve variabelen?
cor(dfScooters, method = "Spearman")

# Mean and standard deviations for quantitative variables
# TODO: bij deelscooter2022 en deelscooter2023 lege velden niet meenemen?
mean(dfScooters$Leeftijd)
sd(dfScooters$Leeftijd)
mean(dfScooters$Deelscooter2023)
sd(dfScooters$Deelscooter2023)
mean(dfScooters$NEP)
sd(dfScooters$NEP)
mean(dfScooters$geld)
sd(dfScooters$geld)

# proporties of nominal variables
prop.table(table(dfScooters$fgeslacht))
prop.table(table(dfScooters$fwoonplaats))
           
# Simple linear regression analysis?
# mdlA <- afhankelijkeVariabele ~ variabele1
# rsltA <- lm(mdlA, data = dfScooters)

# mdlB <- afhankelijkeVariabele ~ variabele 1 + variabele2
# rsltB <- lm(mdlB, data = dfScooters)


# Tabel maken met resultaten
stargazer(rsltA,rsltB, type="html", out = paste0(dirRslt, "Schattingsresultaten.doc"))
