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

MDF <- read.csv("DataWrangling/MDF.csv")

# Make it long
# MDF_long <- gather(MDF, DeepLearningSystem, AbnormalityScore, CAD4TB6, qXRv2_100, IF1_100, IF2_100)
# MDF_long <- gather(MDF, DeepLearningSystem, AbnormalityScore, CAD4TB6, qXRv2_100, LunitScore_100, JF1_100, JF2_100, IF1_100, IF2_100, IF3_100)
MDF_long <- gather(MDF, DeepLearningSystem, AbnormalityScore, CAD4TB6, qXRv2_100, LunitScore_100, JF1_100, IF2_100)

MDF_long$DeepLearningSystem[MDF_long$DeepLearningSystem %in% "CAD4TB6"] <- "CAD4TB"
MDF_long$DeepLearningSystem[MDF_long$DeepLearningSystem %in% "qXRv2_100"] <- "qXR"
MDF_long$DeepLearningSystem[MDF_long$DeepLearningSystem %in% "LunitScore_100"] <- "Lunit"
MDF_long$DeepLearningSystem[MDF_long$DeepLearningSystem %in% "JF1_100"] <- "JF CXR-1"
MDF_long$DeepLearningSystem[MDF_long$DeepLearningSystem %in% "JF2_100"] <- "JF2"
MDF_long$DeepLearningSystem[MDF_long$DeepLearningSystem %in% "IF1_100"] <- "Infervision_TB 1 (product name??)"
MDF_long$DeepLearningSystem[MDF_long$DeepLearningSystem %in% "IF2_100"] <- "Infervision"
MDF_long$DeepLearningSystem[MDF_long$DeepLearningSystem %in% "IF3_100"] <- "Infervision_TB 3 (product name??)"

MDF_long$Xpert2Outcome_num <- as.factor(MDF_long$Xpert2Outcome_num)


