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
MDF <- read_csv("2.0 Version Comparison/MDF.csv")


sum(is.na(MDF$GXP.Result)==T)
sum(MDF$Age <15) # 110
sum(is.na(MDF$Radiology.Result)==T) #15
MDF <- MDF[is.na(MDF$Radiology.Result)==F, ]
MDF <- MDF[MDF$Age >=15, ]
table(MDF$Radiology.Result) # removing 1 images that were marked  unclear
MDF <- MDF[MDF$Radiology.Result != "", ]
MDF <- MDF[MDF$Radiology.Result != "Image Unclear", ]


# Make it long

MDF_long <- gather(MDF, DeepLearningSystem, CAD4TBv6, CAD4TBv7, qXRv2, qXRv3)

MDF_long$DeepLearningSystem <- as.character(MDF_long$DeepLearningSystem)
MDF_long$Xpert2Outcome_num <- as.factor(MDF_long$Xpert2Outcome_num)

MDF_long$XpertHistory <- ""
MDF_long$XpertHistory[MDF_long$TB.Medication.History %in% "No" & MDF_long$Xpert2Outcome_num %in% "0"] <- "Bac Neg - no TB History"
MDF_long$XpertHistory[MDF_long$TB.Medication.History %in% "Yes" & MDF_long$Xpert2Outcome_num %in% "0"] <- "Bac Neg - with TB History"

MDF_long$XpertHistory[MDF_long$TB.Medication.History %in% "No" & MDF_long$Xpert2Outcome_num %in% "1"] <- "Bac Pos - no TB History"
MDF_long$XpertHistory[MDF_long$TB.Medication.History %in% "Yes" & MDF_long$Xpert2Outcome_num %in% "1"] <- "Bac Pos - with TB History"
