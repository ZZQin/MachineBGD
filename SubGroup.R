## All

library(precrec)
library(ggplot2)

ROCPRC <- function(dataset, dataset_long){
  attr <- attributes(evalmod(scores = dataset$CAD4TB6, labels = dataset$Xpert2Outcome_num))
  aucT1 <- c("CAD4TB ", round(attr$auc[4],3))
  
  attr <- attributes(evalmod(scores = dataset$qXRv2, labels = dataset$Xpert2Outcome_num))
  aucT2 <- c("qXR ", round(attr$auc[4],3))
  
  attr <- attributes(evalmod(scores = dataset$LunitScore, labels = dataset$Xpert2Outcome_num))
  aucT3 <- c("Lunit  ", round(attr$auc[4],3))
  
  attr <- attributes(evalmod(scores = dataset$JF1, labels = dataset$Xpert2Outcome_num))
  aucT4 <- c("JF1 ", round(attr$auc[4],3))
  
  attr <- attributes(evalmod(scores = dataset$IF2, labels = dataset$Xpert2Outcome_num))
  aucT5 <- c("Infervision ", round(attr$auc[4],3))
  
  # return(mmcurves)
  # return(autoplot(mmcurves))
  aucT <- c(aucT1, aucT2, aucT3, aucT4, aucT5)
  return(aucT)
}

l <- ROCPRC(MDF, MDF_long)
all <- data.frame(matrix(unlist(l), nrow=5, byrow=T))
all$subgroup <- "all"


## New only
l <- ROCPRC(New, New_long)
New <- data.frame(matrix(unlist(l), nrow=5, byrow=T))
New$subgroup <- "New"

## Old only
l <- ROCPRC(Old, Old_long)
Old <- data.frame(matrix(unlist(l), nrow=5, byrow=T))
Old$subgroup <- "Old"

## Young
l <- ROCPRC(Young, Young_long)
Young <- data.frame(matrix(unlist(l), nrow=5, byrow=T))
Young$subgroup <- "Young"

## Middle
l <- ROCPRC(Middle, Middle_long)
Middle <- data.frame(matrix(unlist(l), nrow=5, byrow=T))
Middle$subgroup <- "Middle"

## Senior
l <- ROCPRC(Senior, Senior_long)W
Senior <- data.frame(matrix(unlist(l), nrow=5, byrow=T))
Senior$subgroup <- "Senior"

## Private only
l <- ROCPRC(Private, Private_long)
Private <- data.frame(matrix(unlist(l), nrow=5, byrow=T))
Private$subgroup <- "Private"

## Public only
l <- ROCPRC(Public, Public_long)
Public <- data.frame(matrix(unlist(l), nrow=5, byrow=T))
Public$subgroup <- "Public"

## Walk-in only
l <- ROCPRC(WalkIn, WalkIn_long)
WalkIn <- data.frame(matrix(unlist(l), nrow=5, byrow=T))
WalkIn$subgroup <- "WalkIn"

## Missing 
l <- ROCPRC(Missing, Missing_long)
Missing <- data.frame(matrix(unlist(l), nrow=5, byrow=T))
Missing$subgroup <- "Missing"

aucTable <- rbind(all, New, Old, Young, Middle, Senior, Private, Public, WalkIn, Missing)
colnames(aucTable) <- c("DL System", "AUC", "PCAUC", "Subgroup")
write.csv(aucTable, "Results/aucTable.csv")
rm(list=(ls()))
