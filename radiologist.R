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
Private <- HumanReader(subset(MDF, MDF$Referral %in% "Private Provider"))
Public <- HumanReader(subset(MDF, MDF$Referral %in% "Public DOTS Facilities"))
WalkIn <- HumanReader(subset(MDF, MDF$Referral %in% "Walk-in"))
Missing <- HumanReader(subset(MDF, is.na(MDF$Referral)==T))

Radiologist <- rbind(allReferral, Private, Public, WalkIn, Missing)
rm(allReferral, Private, Public, WalkIn, Missing)
Radiologist$Sensitivity <- paste0(percent(Radiologist$Sens), ", (", percent(Radiologist$Sens_L), " - ", percent(Radiologist$Sens_H), ")")
Radiologist$Specificity <- paste0(percent(Radiologist$Spec), ", (", percent(Radiologist$Spec_L), " - ", percent(Radiologist$Spec_H), ")")


########### Human vs AI ####################
SubgroupPlot <- read.csv("Chapter/Subgroup Table/SubgroupTable.csv")
SubgroupPlot <- SubgroupPlot[, -1]

CAD_Xpert_plot <- read.csv("Results/CAD_Xpert Cutoffs TABLE 4 digits.csv")
CAD_Xpert_plot <- CAD_Xpert_plot[, -1]
CAD_Xpert_plot <- CAD_Xpert_plot[!CAD_Xpert_plot$DeepLearningSystem %in% c("IF1", "JF2", "IF3"), ]

# knitr::kable(Radiologist[(Radiologist$Referral %in% "MDF"), c(8, 10, 11)])
Human <- Radiologist[(Radiologist$Referral %in% "MDF"), c(8, 10, 11, 4)]
Human <- Human[c(3,2,1), ]
Human <- rbind(Human, Human, Human, Human, Human)

AI <- CAD_Xpert_plot[CAD_Xpert_plot$Comment !="" , c(2, 3, 9, 18, 19, 23)]
AI$Subject <- paste(AI$DeepLearningSystem, AI$Comment)
require(data.table)
AI <- as.data.table(AI)
# # knitr::kable(AI[AI[, .I[which.max(Spec)], by=Subject]$V1])
AI <- AI[AI[, .I[which.max(Spec)], by=Subject]$V1]
# Radiologist[grep("Probably", Radiologist$AccuracyTableCategory), c(8:11)]
humanAI <- cbind(Human, AI[, c(1,2,4, 5, 3)])
humanAI$Diff <- percent(humanAI[, 9]- humanAI[, 4])
humanAI <- humanAI[, c(1,2,3,5:8, 10, 9,4)]
colnames(humanAI) <- c("human benchmark", "sensitivity", "specificy", "DL System", "AIScore", "sensitivity", "specificy", "Diff specificity", "specAI", "specH")


### McNemar test specificity 
healthy <- sum(MDF$Xpert2Outcome_num %in% "0")

# library(readr)
# humanAI <- read_csv("Results/humanAI.csv", 
#                     col_types = cols(`Diff.specificity` = col_number()))
humanAI$CI <- ""

for (i in 1:15){
  test <- prop.test(x=c(humanAI$specAI[i]*healthy, healthy*humanAI$specH[i]), n=c(healthy, healthy))
  humanAI$CI[i] <- paste0(percent(test$conf.int[1]), " - ", percent(test$conf.int[2]))
  # return(humanAI)
}
# humanAI$CI
write.csv(humanAI, "Results/humanAI.csv", row.names = F)
rm(AI, Human)

