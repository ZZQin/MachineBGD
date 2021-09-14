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
Master_df <- Master_df %>%  filter(Age < 15)

Master_df$Result.Date <- mdy(Master_df$Radiology.Result.Date)
Master_df$Result.Year <- year(Master_df$Result.Date)

#### Recode ------
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


Master_df$Xpert2Outcome_num <- as.character(Master_df$GXP.Result)
Master_df$Xpert2Outcome_num[Master_df$GXP.Result %in% "MTB Detected"] <- "1"
Master_df$Xpert2Outcome_num[Master_df$GXP.Result %in% "MTB Not Detected"] <- "0"
Master_df$Xpert2Outcome_num <- as.numeric(Master_df$Xpert2Outcome_num)

table(Master_df$Result.Year)
 
SymptomData <- read_excel("DataWrangling/ScreeningData.xlsx")


Master_df <- merge(Master_df, SymptomData, by.x = "PID_OMRS", by.y = "OpenMRS Identification Number", all.x = TRUE)

summary(Master_df)
