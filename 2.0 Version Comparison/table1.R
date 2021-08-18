source("2.0 Version Comparison/Global.R")

# MDF$qXRv3 <- MDF$qXRv3*100
# MDF$LunitScore <- MDF$LunitScore*100
# MDF$IF1 <- MDF$IF*100
# MDF$JF1 <- MDF$JF1*100

library(tableone)
# MDF <- MDF[MDF$Xpert2Outcome_num %in% "1", ]

# Define numeric variables
listVar <- c("Age", "AgeGroup", "Gender", "Cough", "Fever", "Active.Breathing.Shortness", "Weight.Loss","Haemoptysis", "Symptoms", "TB.Medication.History", "Xpert2Outcome_num", "MTB.Burden", "RIF.Result", "UseCase", "Radiology.Result", "CAD4TB6", "qXRv3", "LunitScore", "JF1", "IF1", "IF2")

#Define categorical variables
catVars <- c( "AgeGroup", "Gender", "Cough", "Fever", "Active.Breathing.Shortness", "Weight.Loss","Haemoptysis", "Symptoms", "TB.Medication.History", "Xpert2Outcome_num", "MTB.Burden", "RIF.Result", "UseCase", "Radiology.Result")


table1 <- CreateTableOne(vars = listVar, strata=c("Xpert2Outcome_num"), data = MDF, factorVars = catVars)
tableType <- CreateTableOne(vars = listVar, strata=c("Radiology.Result"), data = MDF, factorVars = catVars)
table1.all <- CreateTableOne(vars = listVar, data = MDF, factorVars = catVars)
tablethistory <- CreateTableOne(vars = listVar, strata=c("TB.Medication.History"), data = MDF, factorVars = catVars)
# 
# 
# #### p-value ---------
# summary(table1$CatTable)
# summary(table1$ContTable)
# 
# 
# summary(tableType$CatTable)
# summary(tableType$ContTable)
# 
# summary(tablethistory$CatTable)
# summary(tablethistory$ContTable)



#### ---------


table1 <- print(table1, nonnormal = c("Age", "CAD4TB6", "qXRv3", "LunitScore", "JF1", "IF1", "IF2"), cramVars = "Gender", catDigits = 1, contDigits = 1, noSpaces = TRUE)
tabletype <- print(tableType, nonnormal = c("Age", "CAD4TB6", "qXRv3", "LunitScore", "JF1", "IF1", "IF2"), cramVars = "Gender", catDigits = 1, contDigits = 1, noSpaces = TRUE)
table1all <- print(table1.all, nonnormal = c("Age","CAD4TB6", "qXRv3", "LunitScore", "JF1", "IF1", "IF2"), cramVars = "Gender", catDigits = 1, contDigits = 1, noSpaces = TRUE)
tablethistory <- print(tablethistory, nonnormal = c("Age","CAD4TB6", "qXRv3", "LunitScore", "JF1", "IF1", "IF2"), cramVars = "Gender", catDigits = 1, contDigits = 1, noSpaces = TRUE)

table1DF <- data.frame(columnNameILike = row.names(table1), table1)
table1allDF <- data.frame(columnNameILike = row.names(table1all), table1all)
tablethistory <- data.frame(columnNameILike = row.names(tablethistory), tablethistory)
tabletypeDF <- data.frame(columnNameILike = row.names(tabletype), tabletype)

T1 <- cbind(table1allDF, table1DF, tablethistory, tabletypeDF)


T1 <- T1[, c(1, 2, 5,4,6,10,9,11,14,16,15,17,18)]

# T1 <- T1[, c(1, 2, 5,4,6,9,7,10)]

colnames(T1) <- c("", "Overall", "Xpert Positive", "Xpert Negative", "p test", "TB History", "New Case", "p test", "Abnormal - Highly suggestive of TB", "Abnormal - Possibly TB", "Abnormal - not TB", "Normal", "p test")

 
# TEMP <- scan(text = "n,Age (median [IQR]),Gender = F/M (%),Cough = Yes (%),Fever = Yes (%),Short of breath = Yes (%),Weight Loss = Yes (%),Haemoptysis = Yes (%),Any symptom(s) = Yes (%),TB History = Yes (%),Xpert positive (%),MTB Burden (%), High,   Low,   Medium,   Very Low,RIF Result (%), Detected,   Indeterminate,   Not Detected, Use Case (%),   Private&Public Referral,   Public DOTS Retesting,   Walk-in, Radiologist = Highly suggestive of TB (%),Radiologist = Possibly TB (%),Radiologist = Any abnormality (%),CAD4TB (median [IQR]),qXR (median [IQR]),Lunit INSIGHT CXR (median [IQR]),JF CXR-1 (median [IQR]), IF1 (median [IQR]), InferReadDR (median [IQR])", sep = ",", what = "")
# 
# T1$N <- TEMP
# T1 <- T1[, c(10, 2,3, 5:8)]
# rm(TEMP)
write.csv(T1, "2.0 Version Comparison//Table 1.csv", row.names = FALSE)


# knitr::kable(T1, row.names = FALSE, caption = "Table 1 Demographic and Clinical Characteristics of People Screened with Chest X-ray and Xpert MTB/RIF")


rm(table1.all, table1all, table1allDF, table1, table1DF, tabletype, tabletypeDF, tableType, listVar, catVars)

