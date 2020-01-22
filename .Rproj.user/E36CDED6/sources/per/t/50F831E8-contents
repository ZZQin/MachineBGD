source("DataCleaning.R")
library("VIM")
library("gridExtra")
source("DataCleaning.R")


mnar_data <- MDF %>%
  select(TID_OMRS, CAD4TB6, qXRv2, Xpert2Outcome_num, Radiology.Result, Gender, Age, MTB.Burden, Result.Year)



tiff("Figure-1 Missing_CAD4TB_Data.tif", width = 18, height = 10, units = "in", res = 100)
par(mfrow = c(2,3))
spineMiss(mnar_data[, c("Xpert2Outcome_num", "CAD4TB6")])
spineMiss(mnar_data[, c("Radiology.Result", "CAD4TB6")])
spineMiss(mnar_data[, c("Age", "CAD4TB6")])
spineMiss(mnar_data[, c("Gender", "CAD4TB6")])
spineMiss(mnar_data[, c("MTB.Burden", "CAD4TB6")])
spineMiss(mnar_data[, c("Result.Year", "CAD4TB6")])
dev.off()

tiff("Figure-2 Missing_qXR_Data.tif", width = 18, height = 10, units = "in", res = 100)
par(mfrow = c(2,3))
spineMiss(mnar_data[, c("Xpert2Outcome_num", "qXRv2")])
spineMiss(mnar_data[, c("Radiology.Result", "qXRv2")])
spineMiss(mnar_data[, c("Age", "qXRv2")])
spineMiss(mnar_data[, c("Gender", "qXRv2")])
spineMiss(mnar_data[, c("MTB.Burden", "qXRv2")])
spineMiss(mnar_data[, c("Result.Year", "qXRv2")])
dev.off()

tiff("Figure-3 Age.tif", width = 18, height = 12, units = "in", res = 100)
par(mfrow = c(2,1))
spineMiss(mnar_data[, c("Age", "CAD4TB6")])
spineMiss(mnar_data[, c("Age", "qXRv2")])
dev.off()

