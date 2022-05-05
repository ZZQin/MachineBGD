source("2.0 Version Comparison/Global.R")

HumanReader <- function (dataset){ 
 
  ### TB abnormalities ------------------------------
  TP <- sum(dataset$rad.TB %in% "1" & dataset$Xpert2Outcome_num==1)
  FP <- sum(dataset$rad.TB %in% 1 & dataset$Xpert2Outcome_num==0)
  TN <- sum(dataset$rad.TB %in% 0 & dataset$Xpert2Outcome_num==0)
  FN <- sum(dataset$rad.TB %in% 0 & dataset$Xpert2Outcome_num==1)
  
  
  dat <- as.table(matrix(c(TP, FP, FN, TN), nrow=2, byrow=TRUE))
  rval <- epi.tests(dat, conf.level = 0.95)
  rval
  TB.df <- rval$detail[c(3,4)]
  rm(TP, FP, TN, FN)
  
  
  AccuracyTable <- data.frame()
  AccuracyTable <- data.frame(matrix(unlist(TB.df), nrow = 1, byrow = T))
  colnames(AccuracyTable) <- c("Sens", "Sens_L", "Sens_H", "Spec", "Spec_L", "Spec_H")
  AccuracyTable$X <- 1-AccuracyTable$Spec
  AccuracyTable$Subgroup <- deparse(substitute(dataset))

  return(AccuracyTable)
  }


allReferral <- HumanReader(MDF)

PrivateReferral <- HumanReader(subset(MDF, MDF$UseCase %in% "PrivateReferral"))
PublicReferral <- HumanReader(subset(MDF, MDF$UseCase %in% "PublicReferral"))
DOTS <- HumanReader(subset(MDF, MDF$UseCase %in% "PublicDOTSRetesting"))
WalkIn <- HumanReader(subset(MDF, MDF$UseCase %in% "WalkIn"))
Community <- HumanReader(subset(MDF, MDF$UseCase %in% "Community screening"))
Contacts <- HumanReader(subset(MDF, MDF$UseCase %in% "Contacts"))
Missing <- HumanReader(subset(MDF, is.na(MDF$UseCase)==T))


Young <- HumanReader(Young)
Middle <- HumanReader(Middle)
Senior <- HumanReader(Senior)
New <- HumanReader(New)
Old <- HumanReader(Old)
Female <- HumanReader(Female)
Male <- HumanReader(Male)


Radiologist <- rbind(allReferral, Young, Middle, Senior, New, Old, Female, Male , PrivateReferral,PublicReferral, DOTS, WalkIn, Community,Contacts, Missing)

rm(allReferral, Young, Middle, Senior, New, Old, Female, Male , PrivateReferral,PublicReferral, DOTS, WalkIn, Community,Contacts, Missing)

Radiologist$Sensitivity <- paste0(percent(Radiologist$Sens), ", (", percent(Radiologist$Sens_L, suffix = ""), "-", percent(Radiologist$Sens_H), ")")
Radiologist$Specificity <- paste0(percent(Radiologist$Spec), ", (", percent(Radiologist$Spec_L, suffix = ""), "-", percent(Radiologist$Spec_H), ")")



write.csv(Radiologist, "2.0 Version Comparison/Radiologist.csv", row.names = FALSE)

# ### PPV ---------
# TP <- sum(MDF$rad.highly.TB %in% "1" & MDF$Xpert2Outcome_num==1)
# FP <- sum(MDF$rad.highly.TB %in% 1 & MDF$Xpert2Outcome_num==0)
# TN <- sum(MDF$rad.highly.TB %in% 0 & MDF$Xpert2Outcome_num==0)
# FN <- sum(MDF$rad.highly.TB %in% 0 & MDF$Xpert2Outcome_num==1)
# 
# 
# dat <- as.table(matrix(c(TP, FP, FN, TN), nrow=2, byrow=TRUE))
# colnames(dat) <- c("Xpert+","Xpert-")
# rownames(dat) <- c("CXR+","CXR-")
# rval <- epi.tests(dat, conf.level = 0.95)
# rval
# Highly <- rval$elements[c(59, 55)]
# rm(TP, FP, TN, FN)
# 
# 
# ###  High+possible ----------------------------
# TP <- sum(MDF$rad.TB %in% "1" & MDF$Xpert2Outcome_num==1)
# FP <- sum(MDF$rad.TB %in% 1 & MDF$Xpert2Outcome_num==0)
# TN <- sum(MDF$rad.TB %in% 0 & MDF$Xpert2Outcome_num==0)
# FN <- sum(MDF$rad.TB %in% 0 & MDF$Xpert2Outcome_num==1)
# 
# 
# dat <- as.table(matrix(c(TP, FP, FN, TN), nrow=2, byrow=TRUE))
# colnames(dat) <- c("Xpert+","Xpert-")
# rownames(dat) <- c("CXR+","CXR-")
# rval <- epi.tests(dat, conf.level = 0.95)
# rval
# Probably <- rval$elements[c(59, 55)]
# rm(TP, FP, TN, FN)
# 

### Any abnormalities ------------------------------
TP <- sum(MDF$rad.abn %in% "1" & MDF$Xpert2Outcome_num==1)
FP <- sum(MDF$rad.abn %in% 1 & MDF$Xpert2Outcome_num==0)
TN <- sum(MDF$rad.abn %in% 0 & MDF$Xpert2Outcome_num==0)
FN <- sum(MDF$rad.abn %in% 0 & MDF$Xpert2Outcome_num==1)


dat <- as.table(matrix(c(TP, FP, FN, TN), nrow=2, byrow=TRUE))
colnames(dat) <- c("Xpert+","Xpert-")
rownames(dat) <- c("CXR+","CXR-")
rval <- epi.tests(dat, conf.level = 0.95)
rval

rval$elements[c(59, 55, 51, 47)]

Any <- rval$elements[c(59, 55, 51, 47)]

rm(TP, FP, TN, FN, Any, rval)



