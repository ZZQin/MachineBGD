Sys.setenv(LANG = "en")
knitr::opts_chunk$set(fig.width=8, fig.height=6, 
                      echo=FALSE, warning=FALSE, message=FALSE)
library(ggplot2)
library(scales)
library(epiR)
library(ggthemes)
library(RColorBrewer)
library(plyr)
library(reshape2)
library(pROC)
library(tidyverse)
library(dplyr)
library(lubridate)
library(readxl)
library(plotROC)
library(flextable)
library(DataExplorer)
library("readxl")
as.numeric.factor <- function(x) {as.numeric(levels(x))[x]}
Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

# MDF <- read.csv("DataWrangling/MDF.csv")
MDF <- read.csv("DataWrangling/MDF.6.3.csv")

MDF <- MDF[!MDF$UseCase %in% c("Community screening", "Contacts"), ]
sum(is.na(MDF$GXP.Result)==T)
MDF <- MDF[is.na(MDF$Radiology.Result)==F, ]
MDF <- MDF[MDF$Age >=15, ]


# Make it long
# MDF_long <- gather(MDF, DeepLearningSystem, AbnormalityScore, CAD4TB6, qXRv3_100, IF1_100, IF2_100)
# MDF_long <- gather(MDF, DeepLearningSystem, AbnormalityScore, CAD4TB6, qXRv3_100, LunitScore_100, JF1_100, JF2_100, IF1_100, IF2_100, IF3_100)
MDF_long <- gather(MDF, DeepLearningSystem, AbnormalityScore, CAD4TB6, qXRv3_100, LunitScore_100, JF1_100, IF2_100)

MDF_long$DeepLearningSystem <- as.character(MDF_long$DeepLearningSystem)
MDF_long$DeepLearningSystem[MDF_long$DeepLearningSystem %in% "IF2_100"] <- "InferReadDR"
MDF_long$DeepLearningSystem[MDF_long$DeepLearningSystem %in% "LunitScore_100"] <- "Lunit INSIGHT CXR"
MDF_long$DeepLearningSystem[MDF_long$DeepLearningSystem %in% "CAD4TB6"] <- "CAD4TB"
MDF_long$DeepLearningSystem[MDF_long$DeepLearningSystem %in% "qXRv3_100"] <- "qXR"
MDF_long$DeepLearningSystem[MDF_long$DeepLearningSystem %in% "JF1_100"] <- "JF CXR-1"

MDF_long$DeepLearningSystem[MDF_long$DeepLearningSystem %in% "JF2_100"] <- "JF2"
MDF_long$DeepLearningSystem[MDF_long$DeepLearningSystem %in% "IF1_100"] <- "IF1"
MDF_long$DeepLearningSystem[MDF_long$DeepLearningSystem %in% "IF3_100"] <- "IF3"

MDF_long$Xpert2Outcome_num <- as.factor(MDF_long$Xpert2Outcome_num)

MDF_long$XpertHistory <- ""
MDF_long$XpertHistory[MDF_long$TB.Medication.History %in% "No" & MDF_long$Xpert2Outcome_num %in% "0"] <- "Bac Neg - no TB History"
MDF_long$XpertHistory[MDF_long$TB.Medication.History %in% "Yes" & MDF_long$Xpert2Outcome_num %in% "0"] <- "Bac Neg - with TB History"

MDF_long$XpertHistory[MDF_long$TB.Medication.History %in% "No" & MDF_long$Xpert2Outcome_num %in% "1"] <- "Bac Pos - no TB History"
MDF_long$XpertHistory[MDF_long$TB.Medication.History %in% "Yes" & MDF_long$Xpert2Outcome_num %in% "1"] <- "Bac Pos - with TB History"


ML <- MDF[, c('PID_OMRS', 'Gender', 'Age', 'Cough', 'Fever', 'Active.Breathing.Shortness', 'Weight.Loss', 'Haemoptysis', 'TB.Medication.History', 'qXRv3', 'CAD4TB6', 'JF1', 'IF2','Xpert2Outcome_num')]

# ML_training <- ML[sample(nrow(ML), 18853), ]
# ML_testing <- subset(ML, !(ML$PID_OMRS %in% ML_training$PID_OMRS))

# write.csv(ML, "DataWrangling/ML.csv", row.names = F)
# write.csv(ML_training, "DataWrangling/ML_training.csv", row.names = F)
# write.csv(ML_testing, "DataWrangling/ML_testing.csv", row.names = F)
