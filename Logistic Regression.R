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

MDF <- read.csv("DataWrangling/MDF.6.3.csv")



library(caTools)
set.seed(88)
split <- sample.split(MDF$Xpert2Outcome_num, SplitRatio = 0.75)
#get training and test data
MDF_train <- subset(MDF, split == TRUE)
MDF_test <- subset(MDF, split == FALSE)
NC.reg <- MDF


model.null = glm(Xpert2Outcome_num ~ 1, 
                 data=NC.reg,
                 family = binomial(link="logit")
)

model.full <- glm (Xpert2Outcome_num ~ AgeGroup + Gender + Cough + Fever + Weight.Loss + Haemoptysis + Active.Breathing.Shortness + TB.Medication.History + qXRv3, 
                   data = NC.reg, 
                   family = binomial(link="logit"))


step(model.null,
     scope = list(upper=model.full),
     direction="both",
     test="Chisq",
     data=NC.reg)

final.model <- logistic.regression.or.ci(glm(formula = Xpert2Outcome_num ~ highLunit + Age + Cough + Sex + WeightLoss + Fever, family = binomial(link = "logit"), data = NC.reg))

adjOR <- final.model$OR.ci
adjOR <- round(cbind(final.model$OR, adjOR), 2)
adjOR
