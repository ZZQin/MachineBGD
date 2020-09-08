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

# # Delft 6.0.0
# CAD6_delft <- read.table(file = "DataWrangling/CAD_delft_2018.csv", sep = ",", header = T, fill = T)
# colnames(CAD6_delft)[7] <- "TID_Delft"
# CAD6_delft <- CAD6_delft[, -2]
# CAD6_delft <- CAD6_delft[, c(1, 2, 6, 9, 10)]

# Delft 6.3.0
CAD6_delft <- read.table(file = "DataWrangling/Delft 6.3.0.csv", sep = ",", header = T, fill = T)
names(CAD6_delft)[1] <- "TID_Delft"


QA.BGD <- read.csv(file = "./AI Scores/qXRv3.csv", header=T, sep=",")

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


###  Clean MasterDF 
n_occur <- data.frame(table(Master_df$PID_OMRS))
Master_df <- (Master_df[Master_df$PID_OMRS %in% n_occur$Var1[n_occur$Freq < 2], ]) # execute to remove the duplicated PID
n_occur <- data.frame(table(Master_df$TID_OMRS))
MasterDF1TID <- (Master_df[Master_df$TID_OMRS %in% n_occur$Var1[n_occur$Freq == 1], ]) # MasterDF1TID is a subset withOUT duplicated TID


# ###  Clean Delft 
# # CAD6_delft <- CAD6_delft[grep("^.{12}$",CAD6_delft$TID_Delft), ]  #TID_Delft is 
# DeDuDelft_CAD6 <- CAD6_delft[!duplicated(CAD6_delft[c("TID_Delft",  "CAD4TB6")]), ]
# n_occur <- data.frame(table(DeDuDelft_CAD6$TID_Delft))
# # But I decided to discard all records with the same TID with different CAD6 (due to inability to trace the true identify of them). 1857 are removed
# DelftClean <- DeDuDelft_CAD6[DeDuDelft_CAD6$TID_Delft %in% n_occur$Var1[n_occur$Freq == 1], ] # A df with just unique TID from Delft that don't have different CAD6 score --> 26051
# rm(DeDuDelft_CAD6)



###  Merge 
MDF_Delft <- merge(MasterDF1TID, CAD6_delft, by.x = "TID_OMRS", by.y = "TID_Delft")
# MDF_qxr <- merge(MasterDF1TID, QA.BGD, by.x = "TID_OMRS", by.y = "TID")
MDF_Delft_qxr <- merge(MDF_Delft, QA.BGD, by.x = "TID_OMRS", by.y = "TID")
rm(MDF_Delft)

# MDF <- merge(MasterDF1TID, DelftClean, by.x = "TID_OMRS", by.y = "TID_Delft", all.x = TRUE)
# MDF <- merge(MDF, QA.BGD, by.x = "TID_OMRS", by.y = "TID", all.x = TRUE)
# comment(MDF) <- "excluding the invalid and error results (after repeat) & excluding contract tracing and screening, only adults (>=10)"
rm(ReceivedIndex, Df, BGD8168, DelftClean, n_occur, DeDuDelft_CAD6, CAD6_delft, n_occur, Master_df, QA.BGD, MasterDF1TID)



### add Lunit ###
Lunit <- read_csv("AI Scores/lunit.csv")
# L2 <- read_csv("AI Scores/result.csv")
# Lunit <- rbind(L1, L2)
Lunit <- unique(Lunit)
Lunit <- Lunit[, -c(1, 3:8)]
colnames(Lunit)[2] <- "LunitScore"
Lunit <- Lunit[is.na(Lunit$LunitScore)==F, ]
Lunit$PatientID <- as.factor(Lunit$PatientID)

MDF_Delft_qxr_lunit <- merge(MDF_Delft_qxr, Lunit, by.x = "TID_OMRS", by.y = "PatientID", all.x = T)


### add JF1 ###
JF1 <- read_csv("AI Scores/JF1.csv")
colnames(JF1)[1] <- "TID"
colnames(JF1)[2] <- "JF1"
JF1$TID <- as.factor(JF1$TID)

### add JF2 ###
JF2 <- read_csv("AI Scores/JF2.csv")
colnames(JF2)[1] <- "TID"
colnames(JF2)[2] <- "JF2"
JF2$TID <- gsub(".jpg", "", JF2$TID)
JF2$TID <- as.factor(JF2$TID)

MDF_Delft_qxr_lunit_JF1 <- merge(MDF_Delft_qxr_lunit, JF1, by.x = "TID_OMRS", by.y = "TID")
MDF_Delft_qxr_lunit_JF2 <- merge(MDF_Delft_qxr_lunit_JF1, JF2, by.x = "TID_OMRS", by.y = "TID")

### add IF1 ###
IF1 <- read_csv("AI Scores/IF1.csv")
colnames(IF1)[1] <- "TID"
colnames(IF1)[2] <- "IF1"
IF1$TID <- as.factor(IF1$TID)
MDF_Delft_qxr_lunit_IF1 <- merge(MDF_Delft_qxr_lunit_JF2, IF1, by.x = "TID_OMRS", by.y = "TID")

### add IF2 ###
IF2 <- read_csv("AI Scores/IF2.csv")
colnames(IF2)[1] <- "TID"
colnames(IF2)[2] <- "IF2"
IF2$TID <- gsub(".jpg", "", IF2$TID)
IF2$TID <- as.factor(IF2$TID)

MDF_Delft_qxr_lunit_IF2 <- merge(MDF_Delft_qxr_lunit_IF1, IF2, by.x = "TID_OMRS", by.y = "TID")

### add IF3 ###
IF3 <- read_csv("AI Scores/IF3.csv")
colnames(IF3)[1] <- "TID"
colnames(IF3)[2] <- "IF3"
IF3$TID <- gsub(".jpg", "", IF3$TID)
IF3$TID <- as.factor(IF3$TID)

MDF_Delft_qxr_lunit_IF3 <- merge(MDF_Delft_qxr_lunit_IF2, IF3, by.x = "TID_OMRS", by.y = "TID")

### Save csv ##

MDF <- MDF_Delft_qxr_lunit_IF3
MDF$qXRv3_100 <- MDF$qXRv3*100
MDF$LunitScore_100 <- MDF$LunitScore*100
MDF$JF1_100 <- MDF$JF1*100
MDF$JF2_100 <- MDF$JF2*100
MDF$IF1_100 <- MDF$IF1*100
MDF$IF2_100 <- MDF$IF2*100
MDF$IF3_100 <- MDF$IF3*100


MDF$Symptoms <- "1"
MDF$Symptoms[MDF$Cough %in% "No" & MDF$Fever %in% "No" & MDF$`Active Breathing Shortness` %in% "No" & MDF$`Weight Loss` %in% "No" & MDF$Haemoptysis %in% "No"] <- "0"

rm(MDF_Delft_qxr, CAD6_delft, DelftClean, Lunit, Master_df, SymptomData, QA.BGD, MDF_Delft_qxr_lunit, MasterDF1TID, JF1, JF2, IF1, IF2, n_occur, MDF_Delft_qxr_lunit, MDF_Delft_qxr_lunit_IF1, MDF_Delft_qxr_lunit_IF2, MDF_Delft_qxr_lunit_JF1, MDF_Delft_qxr_lunit_JF2, MDF_Delft_qxr_lunit_IF3, IF3)



## Subset 
# MDF <- subset(MDF, MDF$Age > 15)
MDF <- subset(MDF, is.na(MDF$Xpert2Outcome_num)==F)
MDF <- subset(MDF, is.na(MDF$CAD4TB6)==F)
MDF <- subset(MDF, is.na(MDF$qXRv3)==F)
MDF <- MDF[MDF$CAD4TB6 !="-1",]
MDF$AgeGroup [MDF$Age<25]<- "[15,25)"
MDF$AgeGroup [MDF$Age>=25 & MDF$Age<60]<- "[25,60)"
MDF$AgeGroup [MDF$Age>=60]<- "[60,108]"


MDF <- MDF[, -c(3,4)]
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


#### Save  -----------------------
# write.csv(MDF, "DataWrangling/MDF.csv", row.names = F)
write.csv(MDF, "DataWrangling/MDF.6.3.csv", row.names = F)
rm(Classification_ZZ, Referral, Master_df)



