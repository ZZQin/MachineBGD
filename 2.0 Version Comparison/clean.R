Sys.setenv(LANG = "en")
library(ggplot2)
library(scales)
library(epiR)
library(ggthemes)
library(RColorBrewer)
library(plyr)
library(reshape2)
library(pROC) # contain function roc
library(tidyverse)
library(dplyr)
library(lubridate)
library(readxl)
library(plotROC)
library(flextable)
library(DataExplorer)
library(readxl)
as.numeric.factor <- function(x) {as.numeric(levels(x))[x]}

Master_df <- read.csv(file = "DataWrangling/BGD.csv", header=T)
Master_df$Result.Date <- mdy(Master_df$Radiology.Result.Date)
Master_df$Result.Year <- quarter(Master_df$Result.Date)

Master_df$Xpert2Outcome_num <- as.character(Master_df$GXP.Result)
Master_df$Xpert2Outcome_num[Master_df$GXP.Result %in% "MTB Detected"] <- "1"
Master_df$Xpert2Outcome_num[Master_df$GXP.Result %in% "MTB Not Detected"] <- "0"
Master_df$Xpert2Outcome_num <- as.numeric(Master_df$Xpert2Outcome_num)

Master_df <- Master_df %>%
  filter(Result.Date < as.Date("2017-01-01"))

SymptomData <- read_excel("DataWrangling/ScreeningData.xlsx")

Master_df <- merge(Master_df, SymptomData, by.x = "PID_OMRS", by.y = "OpenMRS Identification Number", all.x = TRUE)

#### Radiologist TB, not TB ####
# Highly TB Suggestive 
Master_df$rad.highly.TB <- "0"
Master_df$rad.highly.TB [Master_df$Radiology.Result == "Highly TB Suggestive" ] <- "1"
Master_df$rad.highly.TB [Master_df$Radiology.Result == "" | Master_df$Radiology.Result == "Image Unclear" ] <- "NA"

# Highly+ possibly
Master_df$rad.TB <- "0"
Master_df$rad.TB [Master_df$Radiology.Result == "Highly TB Suggestive" | Master_df$Radiology.Result =="TB Possible Signs" ] <- "1"
Master_df$rad.TB [Master_df$Radiology.Result == "" | Master_df$Radiology.Result == "Image Unclear"] <- "NA"

# Any abnormality
Master_df$rad.abn <- "0"
Master_df$rad.abn [Master_df$Radiology.Result == "Highly TB Suggestive" | Master_df$Radiology.Result =="TB Possible Signs" | Master_df$Radiology.Result == "Non-TB Abnormality"] <- "1"
Master_df$rad.abn [Master_df$Radiology.Result == "" | Master_df$Radiology.Result == "Image Unclear"] <- "NA"

### Add AI score ------
# Delft CAD4TB6
CAD6_delft <- read.table(file = "DataWrangling/CAD_delft_2018.csv", sep = ",", header = T, fill = T)
colnames(CAD6_delft)[7] <- "TID_Delft"
CAD6_delft <- CAD6_delft[, -2]
CAD6_delft <- CAD6_delft[, c(1, 2, 6, 9, 10)]
names(CAD6_delft)[5] <- "CAD4TBv6"
# 
# Delft CAD4TB7
CAD7_delft <- read.table(file = "DataWrangling/Delft 6.3.0.csv", sep = ",", header = T, fill = T)
names(CAD7_delft)[1] <- "TID_Delft"
names(CAD7_delft)[2] <- "CAD4TBv7"

# qXR V 2 & 3
QA2.BGD <- read.csv(file = "./AI Scores/qXRv2.csv", header=T, sep=",")
QA3.BGD <- read.csv(file = "./AI Scores/qXRv3.csv", header=T, sep=",")



###  Clean MasterDF 
n_occur <- data.frame(table(Master_df$PID_OMRS))
Master_df <- (Master_df[Master_df$PID_OMRS %in% n_occur$Var1[n_occur$Freq < 2], ]) # execute to remove the duplicated PID
n_occur <- data.frame(table(Master_df$TID_OMRS))
MasterDF1TID <- (Master_df[Master_df$TID_OMRS %in% n_occur$Var1[n_occur$Freq == 1], ]) # MasterDF1TID is a subset withOUT duplicated TID

rm(n_occur)
###  Clean Delft
# CAD6_delft <- CAD6_delft[grep("^.{12}$",CAD6_delft$TID_Delft), ]  #TID_Delft is
DeDuDelft_CAD6 <- CAD6_delft[!duplicated(CAD6_delft[c("TID_Delft",  "CAD4TBv6")]), ]
n_occur <- data.frame(table(DeDuDelft_CAD6$TID_Delft))
# But I decided to discard all records with the same TID with different CAD6 (due to inability to trace the true identify of them). 1857 are removed
DelftClean <- DeDuDelft_CAD6[DeDuDelft_CAD6$TID_Delft %in% n_occur$Var1[n_occur$Freq == 1], ] # A df with just unique TID from Delft that don't have different CAD6 score --> 26051
rm(DeDuDelft_CAD6)



###  Merge 
MDF_Delft6 <- merge(MasterDF1TID, CAD6_delft, by.x = "TID_OMRS", by.y = "TID_Delft")
MDF_Delft67 <- merge(MDF_Delft6, CAD7_delft, by.x = "TID_OMRS", by.y = "TID_Delft")

MDF_Delft_qxr2 <- merge(MDF_Delft67, QA2.BGD, by.x = "TID_OMRS", by.y = "TID")
MDF_Delft_qxr23 <- merge(MDF_Delft_qxr2, QA3.BGD, by.x = "TID_OMRS", by.y = "TID")
rm(MDF_Delft)

### Save csv ##
MDF <- MDF_Delft_qxr23
# write.csv(MDF, "2.0 Version Comparison/MDF.csv", row.names = F)
rm(list = ls(all.names = TRUE))



### Clean all ----
MDF <- read.csv("2.0 Version Comparison/MDF.csv")

MDF$Symptoms <- "1"
MDF$Symptoms[MDF$Cough %in% "No" & MDF$Fever %in% "No" & MDF$`Active Breathing Shortness` %in% "No" & MDF$`Weight Loss` %in% "No" & MDF$Haemoptysis %in% "No"] <- "0"

MDF$AgeGroup [MDF$Age<25]<- "[15,25)"
MDF$AgeGroup [MDF$Age>=25 & MDF$Age<60]<- "[25,60)"
MDF$AgeGroup [MDF$Age>=60]<- "[60,108]"

MDF <- MDF[, -c(3,4)]

MDF <- subset(MDF, is.na(MDF$CAD4TBv6)==F)
MDF <- subset(MDF, is.na(MDF$CAD4TBv7)==F)
MDF <- subset(MDF, is.na(MDF$qXRv2)==F)
MDF <- subset(MDF, is.na(MDF$qXRv3)==F)
MDF <- MDF[MDF$CAD4TBv6 !="-1",]
MDF <- MDF[MDF$CAD4TBv7 !="-1",]


Master_df <- read.csv("AI Scores/Master_df.csv")
MDF$Referral <- Master_df$type[match(MDF$PID_OMRS, Master_df$PID)]
MDF_Original <- MDF

#### Referral Source -----------------------
library(readxl)
Referral <- read_excel("DataWrangling/referral_source.xlsx")
Referral <- Referral[, -1]
colnames(Referral)[3] <- "ReferralSource"
Referral$ReferralSource <- tolower(Referral$ReferralSource)

MDF$ReferralSource <- Referral$ReferralSource[match(MDF$TID_OMRS, Referral$TID_OMRS)]

Classification_ZZ <- read_excel("DataWrangling/Classification_ZZ.xlsx")
Classification_ZZ$ReferralUnit <- tolower(Classification_ZZ$ReferralUnit)
MDF$UseCase <- Classification_ZZ$Unit[match(MDF$ReferralSource, Classification_ZZ$ReferralUnit)]
table(MDF$UseCase)

rm(Classification_ZZ, Referral, Master_df)

## conver to the scale of 0-100

MDF$qXRv2 <- MDF$qXRv2*100
MDF$qXRv3 <- MDF$qXRv3*100

#### Save  -----------------------
write.csv(MDF, "2.0 Version Comparison/MDF.csv", row.names = F)