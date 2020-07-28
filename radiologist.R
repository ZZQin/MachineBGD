source("DataWrangling/GlobalOption.R")


HumanReader <- function (dataset){ 
  ### Highly suggestive of TB -------------------------------
  TP <- sum(dataset$rad.highly.TB %in% "1" & dataset$Xpert2Outcome_num==1)
  FP <- sum(dataset$rad.highly.TB %in% 1 & dataset$Xpert2Outcome_num==0)
  TN <- sum(dataset$rad.highly.TB %in% 0 & dataset$Xpert2Outcome_num==0)
  FN <- sum(dataset$rad.highly.TB %in% 0 & dataset$Xpert2Outcome_num==1)
  
  
  dat <- as.table(matrix(c(TP, FP, FN, TN), nrow=2, byrow=TRUE))
  colnames(dat) <- c("Xpert+","Xpert-")
  rownames(dat) <- c("CXR+","CXR-")
  rval <- epi.tests(dat, conf.level = 0.95)
  rval
  Highly <- rval$elements[c(59, 55)]
  rm(TP, FP, TN, FN)
  
  
  ###  High+possible ----------------------------
  TP <- sum(dataset$rad.TB %in% "1" & dataset$Xpert2Outcome_num==1)
  FP <- sum(dataset$rad.TB %in% 1 & dataset$Xpert2Outcome_num==0)
  TN <- sum(dataset$rad.TB %in% 0 & dataset$Xpert2Outcome_num==0)
  FN <- sum(dataset$rad.TB %in% 0 & dataset$Xpert2Outcome_num==1)
  
  
  dat <- as.table(matrix(c(TP, FP, FN, TN), nrow=2, byrow=TRUE))
  colnames(dat) <- c("Xpert+","Xpert-")
  rownames(dat) <- c("CXR+","CXR-")
  rval <- epi.tests(dat, conf.level = 0.95)
  rval
  Probably <- rval$elements[c(59, 55)]
  rm(TP, FP, TN, FN)
  
  ### Any abnormalities ------------------------------
  TP <- sum(dataset$rad.abn %in% "1" & dataset$Xpert2Outcome_num==1)
  FP <- sum(dataset$rad.abn %in% 1 & dataset$Xpert2Outcome_num==0)
  TN <- sum(dataset$rad.abn %in% 0 & dataset$Xpert2Outcome_num==0)
  FN <- sum(dataset$rad.abn %in% 0 & dataset$Xpert2Outcome_num==1)
  
  
  dat <- as.table(matrix(c(TP, FP, FN, TN), nrow=2, byrow=TRUE))
  colnames(dat) <- c("Xpert+","Xpert-")
  rownames(dat) <- c("CXR+","CXR-")
  rval <- epi.tests(dat, conf.level = 0.95)
  rval
  Any <- rval$elements[c(59, 55)]
  rm(TP, FP, TN, FN)
  
  AccuracyTable <- data.frame(matrix(unlist(c(Highly, Probably, Any)), nrow = 3, byrow = T))
  colnames(AccuracyTable) <- c("Sens", "Sens_L", "Sens_H", "Spec", "Spec_L", "Spec_H")
  AccuracyTable$X <- 1-AccuracyTable$Spec
  AccuracyTable$AccuracyTableCategory <- c("Highly Suggestive", "Probably TB", "Any Abnormality")
  AccuracyTable$Referral <- deparse(substitute(dataset))

  rm(Highly, Probably, Any, rval)
  return(AccuracyTable)
  
}


allReferral <- HumanReader(MDF)
Referral <- HumanReader(subset(MDF, MDF$UseCase %in% "PrivatePublicReferral"))
DOTS <- HumanReader(subset(MDF, MDF$UseCase %in% "PublicDOTSRetesting"))
WalkIn <- HumanReader(subset(MDF, MDF$UseCase %in% "WalkIn"))
Missing <- HumanReader(subset(MDF, is.na(MDF$UseCase)==T))

Radiologist <- rbind(allReferral, Referral, DOTS, WalkIn, Missing)
rm(allReferral, Referral, DOTS, WalkIn, Missing)
Radiologist$Sensitivity <- paste0(percent(Radiologist$Sens), ", (", percent(Radiologist$Sens_L, suffix = ""), "-", percent(Radiologist$Sens_H), ")")
Radiologist$Specificity <- paste0(percent(Radiologist$Spec), ", (", percent(Radiologist$Spec_L, suffix = ""), "-", percent(Radiologist$Spec_H), ")")



