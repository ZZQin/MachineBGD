rm(list = ls()) 
source("2.0 Version Comparison/Global.R")
source("2.0 Version Comparison/radiologist.R")


New <- MDF[MDF$TB.Medication.History %in% "No",]
Old <- MDF[MDF$TB.Medication.History %in% "Yes",]

Female <- MDF[MDF$Gender %in% "F", ]
Male <- MDF[MDF$Gender %in% "M", ]

Young <- MDF[MDF$AgeGroup %in% "[15,25)",]
Middle <- MDF[MDF$AgeGroup %in% "[25,60)",]
Senior <- MDF[MDF$AgeGroup %in% "[60,108]",]

PrivateReferral <-subset(MDF, MDF$UseCase %in% "PrivateReferral")
PublicReferral <-subset(MDF, MDF$UseCase %in% "PublicReferral")
DOTS <-subset(MDF, MDF$UseCase %in% "PublicDOTSRetesting")
WalkIn <-subset(MDF, MDF$UseCase %in% "WalkIn")
Community <-subset(MDF, MDF$UseCase %in% "Community screening")
Contacts <-subset(MDF, MDF$UseCase %in% "Contacts")


### AUC with CI --- 
library(pROC)
ROC <- function(dataset){
  roc_CAD6 <- ci.auc(Xpert2Outcome_num ~ CAD4TBv6, dataset)
  roc_qure2 <- ci.auc(Xpert2Outcome_num ~ qXRv2, dataset)
  roc_CAD7 <- ci.auc(Xpert2Outcome_num ~ CAD4TBv7, dataset)
  roc_qure3 <- ci.auc(Xpert2Outcome_num ~ qXRv3, dataset)
  
  
  aucT <- as.data.frame(matrix(c(roc_CAD6[2], roc_CAD6[1], roc_CAD6[3], roc_qure2[2], roc_qure2[1], roc_qure2[3], roc_CAD7[2], roc_CAD7[1], roc_CAD7[3], roc_qure3[2], roc_qure3[1], roc_qure3[3]),  byrow=TRUE, ncol =  3))
  names(aucT) <- c("AUC", "AUCL", "AUCH")
  aucT$AI.Algorithm <- c("CAD4TB v6", "qXR v2", "CAD4TB v7", "qXR v3")
  
  
  return(aucT)
}



## New only
New <- ROC(New)
New$subgroup <- "New cases"

## Old only
Old <- ROC(Old)
Old$subgroup <- "Previously treated cases"

## Young
Young <- ROC(Young)
Young$subgroup <- "Young age"

## Middle
Middle <- ROC(Middle)
Middle$subgroup <- "Middle age"

## Senior
Senior <- ROC(Senior)
Senior$subgroup <- "Old age"

## PrivateReferral only
PrivateReferral <- ROC(PrivateReferral)
PrivateReferral$subgroup <- "PrivateReferral"

## PublicReferral only
PublicReferral <- ROC(PublicReferral)
PublicReferral$subgroup <- "PublicReferral"

## DOTS only
DOTS <- ROC(DOTS)
DOTS$subgroup <- "DOTS retested"


## WalkIn only
WalkIn <- ROC(WalkIn)
WalkIn$subgroup <- "WalkIn"


## Community only
Community <- ROC(Community)
Community$subgroup <- "Community"


## Contacts only
Contacts <- ROC(Contacts)
Contacts$subgroup <- "Contacts"


## Female only
Female <- ROC(Female)
Female$subgroup <- "Female"


## Male only
Male <- ROC(Male)
Male$subgroup <- "Male"



ROCaucTable <- rbind(New, Old, Young, Middle, Senior, PrivateReferral, PublicReferral, DOTS, WalkIn,Community, Contacts, Female, Male)
View(ROCaucTable)
write.csv(ROCaucTable, "2.0 Version Comparison/SubgroupROC.csv", row.names = F)



### Bar chart with error bar ----
rm(list = ls()) 
ROCaucTable <- read.csv("2.0 Version Comparison/SubgroupROC.csv")
ROCaucTable$AUC <- round(ROCaucTable$AUC, 3)

ROCaucTable$subgroup <- factor(ROCaucTable$subgroup, levels = c("All","Young age", "Middle age", "Old age", "PrivateReferral", "PublicReferral", "DOTS retested", "WalkIn", "Community", "Contacts", "New cases", "Previously treated cases", "Female", "Male"))

ROCaucTable$DL <- ""
ROCaucTable$DL[ROCaucTable$AI.Algorithm %in% "CAD4TB v6" |ROCaucTable$AI.Algorithm %in% "CAD4TB v7" ] <- "CAD4TB"
ROCaucTable$DL[ROCaucTable$AI.Algorithm %in% "qXR v2" |ROCaucTable$AI.Algorithm %in% "qXR v3" ] <- "qXR"
ROCaucTable$DL <- as.factor(ROCaucTable$DL)



age <-ggplot(ROCaucTable[ROCaucTable$subgroup %in% c("Young age", "Middle age", "Old age"), ], aes(x=subgroup, y=AUC, fill=AI.Algorithm)) +  geom_bar(stat="identity", width=0.6, position=position_dodge()) + geom_errorbar(aes(ymin=AUCL, ymax=AUCH), width=.2, position=position_dodge(0.7)) + geom_text(aes(label=AUC), position=position_dodge(width=0.9), vjust=0.05, hjust = 1.5, angle=90) + facet_wrap(~DL) + theme_minimal()+ theme(legend.position = "top")+  ylim(0.7, 0.95) + theme(panel.grid.minor = element_line(size=0.7)) + scale_y_continuous(minor_breaks = seq(0.7 , 0.95, 0.05), breaks = seq(0.7, 0.95, 0.05)) 
age

referral<-ggplot(ROCaucTable[ROCaucTable$subgroup %in% c("PrivateReferral", "PublicReferral", "DOTS retested", "WalkIn", "Community", "Contacts"), ], aes(x=subgroup, y=AUC, fill=AI.Algorithm)) + geom_bar(stat="identity", width=0.7, position=position_dodge()) + facet_wrap(~DL) +theme_minimal() + geom_text(aes(label=AUC), position=position_dodge(width=0.9), vjust=0.05, hjust = 1.5, angle=90)+ theme(legend.position = "top")+  ylim(0.7, 0.95) + theme(panel.grid.minor = element_line(size=0.5)) + geom_errorbar(aes(ymin=AUCL, ymax=AUCH), width=.2, position=position_dodge(0.6)) + theme(axis.text.x = element_text(angle=45)) + scale_y_continuous(minor_breaks = seq(0.7 , 0.95, 0.05), breaks = seq(0.7, 0.95, 0.05)) 
referral

history<-ggplot(ROCaucTable[ROCaucTable$subgroup %in% c("New cases", "Previously treated cases"), ], aes(x=subgroup, y=AUC, fill=AI.Algorithm)) + geom_bar(stat="identity", width=0.5, position=position_dodge()) + facet_wrap(~DL) +theme_minimal() + theme(legend.position = "top") +  ylim(0, 0.95) + geom_text(aes(label=AUC), position=position_dodge(width=0.9), vjust=0.05, hjust = 1.5, angle=90)+ theme(panel.grid.minor = element_line(size=0.7)) + scale_y_continuous(minor_breaks = seq(0 , 0.95, 0.05), breaks = seq(0.7, 0.95, 0.05)) + geom_errorbar(aes(ymin=AUCL, ymax=AUCH), width=.2, position=position_dodge(0.6))

gender<-ggplot(ROCaucTable[ROCaucTable$subgroup %in% c("Female", "Male"), ], aes(x=subgroup, y=AUC, fill=AI.Algorithm)) + geom_bar(stat="identity", width=0.5, position=position_dodge()) + facet_wrap(~DL) +theme_minimal() + theme(legend.position = "top") +  ylim(0, 0.95) + geom_text(aes(label=AUC), position=position_dodge(width=0.9), vjust=0.05, hjust = 1.5, angle=90)+ theme(panel.grid.minor = element_line(size=0.5)) + scale_y_continuous(minor_breaks = seq(0 , 0.95, 0.05), breaks = seq(0.7, 0.95, 0.05)) + geom_errorbar(aes(ymin=AUCL, ymax=AUCH), width=.2, position=position_dodge(0.6))



tiff("2.0 Version Comparison/Subgroup.tif", width = 12, height = 18, units = "in", res = 250)
require(gridExtra)
grid.arrange(age, referral, history, gender, ncol=2)
dev.off()