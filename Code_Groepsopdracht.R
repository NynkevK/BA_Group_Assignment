#-----------------------------------------------------------------------------
# Authors: Nynke van Koningsveld, Suzanne Poelgeest, Isabelle Quartel,
# Olivier van Warmerdam, David van Rosmalen.
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
# install.packages("car", dependencies = TRUE)
# install.packages("lm.beta", dependencies = TRUE)
# install.packages("rpart", dependencies = TRUE)
# install.packages("rpart.plot", dependencies = TRUE)

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
library(lm.beta)
library(car)
library(rpart)
library(rpart.plot)

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

dfScooters$fDeelscootergebruikt <- factor(dfScooters$`Deelscooter_gebruikt?`,
                                          levels = c("1", "2"),
                                          labels = c("ja", "nee"))

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
# Reliability Analysis of Likert Scales
#-----------------------------------------------------------------------------
# Cronbach's Alpha for NEP
psych::alpha(dfScooters[c("Q6_1", "Q6_2", "Q6_3", 
                          "Q6_4", "Q6_5", "Q6_6", "Q6_7", "Q6_8", "Q6_9")],
             keys=c("Q6_1", "Q6_5", "Q6_7", "Q6_9"))

# Cronbach's Alpha for monetary value
psych:: alpha(dfScooters[c("Q5_1", "Q5_2", "Q5_3","Q5_4", 
                           "Q5_5", "Q5_6", "Q5_7", "Q5_8")])

# Cronbach's Alpha for monetary value als je Q5_5 weg laat
psych:: alpha(dfScooters[c("Q5_1", "Q5_2", "Q5_3","Q5_4", 
                           "Q5_6", "Q5_7", "Q5_8")])

# Add scores to dataframe
dfScooters$NEP <- psych::alpha(dfScooters[c("Q6_1", "Q6_2", "Q6_3", 
                                            "Q6_4", "Q6_5", "Q6_6", "Q6_7", "Q6_8", "Q6_9")],
                               keys=c("Q6_1", "Q6_5", "Q6_7", "Q6_9"),
                               cumulative = FALSE)$scores

dfScooters$Geld <- psych:: alpha(dfScooters[c("Q5_1", "Q5_2", "Q5_3","Q5_4", 
                                              "Q5_5", "Q5_6", "Q5_7", "Q5_8")], cumulative = FALSE)$scores

#-----------------------------------------------------------------------------
# Descriptive Analysis
#-----------------------------------------------------------------------------
# Mean and standard deviations for quantitative variables
mean(dfScooters$Leeftijd)
sd(dfScooters$Leeftijd)
mean(dfScooters$Deelscooter2023)
sd(dfScooters$Deelscooter2023)
mean(dfScooters$Deelscooter2022)
sd(dfScooters$Deelscooter2022)
mean(dfScooters$geld)
sd(dfScooters$geld)
mean(dfScooters$NEP)
sd(dfScooters$NEP)

# absolute values of nominal variables
table(dfScooters$fgeslacht)
table(dfScooters$fwoonplaats)
table(dfScooters$fDeelscootergebruikt)

# proportions of nominal variables
prop.table(table(dfScooters$fgeslacht))
prop.table(table(dfScooters$fwoonplaats))
prop.table(table(dfScooters$fDeelscootergebruikt))

# Frequentie-kruis-tabel
stargazer(table(dfScooters$fgeslacht, dfScooters$fDeelscootergebruikt),
          title="Kruistabel met frequenties",
          align=TRUE, no.space=TRUE,
          type="html",
          out = paste0(dirRslt,"Freq_table_hypothesis_1.doc"))

table(dfScooters$fgeslacht, dfScooters$fDeelscootergebruikt)
table(dfScooters$fgeslacht, dfScooters$Deelscooter2022)
table(dfScooters$fwoonplaats, dfScooters$fDeelscootergebruikt)
table(dfScooters$fwoonplaats, dfScooters$Deelscooter2022)

#-----------------------------------------------------------------------------
# Multivariate analysis with regression
#-----------------------------------------------------------------------------
# Formulate and estimate different models
mdlA <- Deelscooter2022 ~ NEP 
rsltA <- lm(mdlA, data = dfScooters)

mdlB <- Deelscooter2022 ~ fgeslacht
rsltB <- lm(mdlB, data = dfScooters)

mdlC <- Deelscooter2022 ~ Geld
rsltC <- lm(mdlC, data = dfScooters)

mdlD <- Deelscooter2022 ~ Leeftijd
rsltD <- lm(mdlD, data = dfScooters)

mdlE <- Deelscooter2022 ~ fwoonplaats
rsltE <- lm(mdlE, data = dfScooters)

mdlF <- Deelscooter2022 ~ fwoonplaats + Leeftijd
rsltF <- lm(mdlF, data = dfScooters)

mdlG <- Deelscooter2022 ~ NEP + Geld + fgeslacht + fwoonplaats + Leeftijd
rsltG <- lm(mdlG, data = dfScooters)

# Regressionresult of all the models in 1 table
stargazer(rsltA, rsltB, rsltC, rsltD, rsltE, rsltF, rsltG,
          title="Regressieresultaten", 
          column.labels = c("Model A", "Model B", "Model C", "Model D", "Model E",
                            "Model F", "Model G"),
          align=TRUE, no.space=TRUE, intercept.bottom = FALSE,
          type="html",
          out = paste0(dirRslt,"Regressieresultaten groepsopdracht.doc"))

# Standardized beta's for Model G
rsltG.beta <- lm.beta(rsltG) 

# Output the results
stargazer(rsltG.beta,
          title="Gestandaardiseerde regressieresultaten", 
          column.labels = c("Model G"), 
          align=TRUE, no.space=TRUE, intercept.bottom = FALSE,
          type="html",
          out = paste0(dirRslt,"Gestandaardiseerde regressieresultaten groepsopdracht.doc"))

# Multicollineariteit
# VIF results
vif(rsltA) 

# Tol = 1/VIF
1/vif(rsltA)

# Combined results
rsltH <- cbind(VIF = vif(rsltA) ,TOL = 1/vif(rsltA))

# table format
stargazer(rsltH,
          title="tba", 
          align=TRUE, no.space=TRUE, intercept.bottom = FALSE,
          type="text")

#Residuals versus fitted values
yhat    <- fitted(rsltA)        # Fitted values
sdresid <- rstudent(rsltA)      # sdresid
alpha <- 0.01

dfScooters$yhat  <- yhat 
dfScooters$sdresidy <- sdresid

dfTmp <- data.frame(sdresid, yhat)

ggplot(dfTmp, aes(x = yhat, y = sdresid)) +
  geom_point(colour = "black") +
  ylab("Residuen") +
  xlab("Voorspelde waarden") +
  ggtitle("tbd") +  
  geom_hline(yintercept = 0, colour = "grey") +
  geom_hline(yintercept = qt(1 - alpha/2, rsltA$df.residual), 
             colour = "grey", linetype = 2) +
  geom_hline(yintercept = -qt(1 - alpha/2, rsltA$df.residual), 
             colour = "grey", linetype = 2) +
  theme(axis.text.x = element_text(size = rel(1.25)),
        axis.text.y = element_text(size = rel(1.25)))
ggsave(paste0(dirRslt,"Groepsproject.png"),
       width=8, height=6)

#-----------------------------------------------------------------------------
# Classificatie analyse
#-----------------------------------------------------------------------------
mdlG.class <- fDeelscootergebruikt ~ NEP + Geld + Geslacht + Woonplaats + Leeftijd
rsltTreeG <- rpart(mdlG.class, data=dfScooters, 
                   method="class", 
                   parms = list(split = "information"))

rpart.plot(rsltTreeG, extra = 101, digits = 3, box.palette = "blue")

# Age verwijderen uit het model
mdlH.class <- fDeelscootergebruikt ~ NEP + Geld + fgeslacht + fwoonplaats
rsltTreeH <- rpart(mdlH.class, data=dfScooters, 
                   method="class", 
                   parms = list(split = "information"))

rpart.plot(rsltTreeH, extra = 104, digits = 3, box.col=c("white","grey")[rsltTreeH$frame$yval])
png(paste0(dirRslt, "ClassificationTreeGroepsopdracht.png"))
dev.off()
