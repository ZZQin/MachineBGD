source("DataWrangling/GlobalOption.R")

# MDF <- subset(MDF, MDF$Age > 15)
library(tableone)

# Define numeric variables
listVar <- c("Age", "Gender", "Cough", "Fever", "Active.Breathing.Shortness", "Weight.Loss","Haemoptysis", "Symptoms", "TB.Medication.History", "Xpert2Outcome_num", "MTB.Burden", "RIF.Result", "Referral", "rad.highly.TB", "rad.TB", "rad.abn", "CAD4TB6", "qXRv2", "LunitScore", "JF1", "IF1", "IF2")

#Define categorical variables
catVars <- c( "Gender", "Cough", "Fever", "Active.Breathing.Shortness", "Weight.Loss","Haemoptysis", "Symptoms", "TB.Medication.History", "Xpert2Outcome_num", "MTB.Burden", "RIF.Result", "Referral", "rad.highly.TB", "rad.TB", "rad.abn")


table1 <- CreateTableOne(vars = listVar, strata=c("Xpert2Outcome_num"), data = MDF, factorVars = catVars)
tableType <- CreateTableOne(vars = listVar, strata=c("Referral"), data = MDF, factorVars = catVars)

table1.all <- CreateTableOne(vars = listVar, data = MDF, factorVars = catVars)


table1 <- print(table1, nonnormal = c("Age", "CAD4TB6", "qXRv2", "LunitScore", "JF1", "IF1", "IF2"), cramVars = "Gender", catDigits = 1, contDigits = 1, noSpaces = TRUE)
tabletype <- print(tableType, nonnormal = c("Age", "CAD4TB6", "qXRv2", "LunitScore", "JF1", "IF1", "IF2"), cramVars = "Gender", catDigits = 1, contDigits = 1, noSpaces = TRUE)
table1all <- print(table1.all, nonnormal = c("Age","CAD4TB6", "qXRv2", "LunitScore", "JF1", "IF1", "IF2"), cramVars = "Gender", catDigits = 1, contDigits = 1, noSpaces = TRUE)

table1DF <- data.frame(columnNameILike = row.names(table1), table1)
table1allDF <- data.frame(columnNameILike = row.names(table1all), table1all)
tabletypeDF <- data.frame(columnNameILike = row.names(tabletype), tabletype)
T1 <- cbind(table1DF, table1allDF, tabletypeDF)
# T1 <- T1[, c(1, 3, 2, 4, 7)]
# colnames(T1) <- c("", "Xpert Positive", "Xpert Negative", "p test", "Overall")

T1 <- T1[, c(1, 3, 2, 4, 7, 9:12)]
colnames(T1) <- c("", "Xpert Positive", "Xpert Negative", "p test", "Overall", "Private", "Public", "WalkIn")


TEMP <- scan(text = "n,Age (median [IQR]),Gender = F/M (%),Cough = Yes (%),Fever = Yes (%),Short of breath = Yes (%),Weight Loss = Yes (%),Haemoptysis = Yes (%),Any symptom(s) = Yes (%),TB History = Yes (%),Xpert positive (%),MTB Burden (%),   ,   High,   Low,   Medium,   Very Low,RIF Result (%),   ,   Detected,   Indeterminate,   Not Detected,Referral Type(%),   Private Provider,   Public DOTS Facilities,   Walk-in,Radiologist = Highly suggestive of TB (%),Radiologist = Possibly TB (%),Radiologist = Any abnormality (%),CAD4TB (median [IQR]),qXR (median [IQR]),Lunit (median [IQR]),JF1 (median [IQR]), IF1, IF2", sep = ",", what = "")

T1$N <- TEMP
T1 <- T1[, c(10, 2,3, 5:8)]
rm(TEMP)
write.csv(T1, "Results/Table 1.csv", row.names = FALSE)


knitr::kable(T1, row.names = FALSE, caption = "Table 1 Demographic and Clinical Characteristics of People Screened with Chest X-ray and Xpert MTB/RIF")


rm(table1.all, table1all, table1allDF, table1, table1DF, tabletype, tabletypeDF, tableType, listVar, catVars)