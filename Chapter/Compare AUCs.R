source("radiologist.R")
# CAD_Xpert_plot <- read.csv("Results/CAD_Xpert Cutoffs TABLE_CAD6.3.csv")
# CAD_Xpert_plot <- subset(CAD_Xpert_plot, CAD_Xpert_plot$Site %in% "BGD")
# Radiologist <- Radiologist[Radiologist$Referral %in% "MDF", ]

# Creating the subgroups
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

# CAD_Xpert_plot <- CAD_Xpert_plot[!CAD_Xpert_plot$DeepLearningSystem %in% c("IF1", "JF2", "IF3", "InferReadDR", "JF CXR-1", "Lunit INSIGHT CXR", "qXR"), ]

#### Calculate the CIs of AUC of ROC --------
# roc_CAD6 <- ci.auc(Xpert2Outcome_num ~ CAD4TB6, MDF)
# roc_qure <- ci.auc(Xpert2Outcome_num ~ qXRv3_100, MDF)
# roc_Lunit <- ci.auc(Xpert2Outcome_num ~ LunitScore_100, MDF)
# roc_JF1 <- ci.auc(Xpert2Outcome_num ~ JF1_100, MDF)
# roc_IF2 <- ci.auc(Xpert2Outcome_num ~ IF2_100, MDF)





##### Subgroup CI  -----

### 1. PRC: Bootstrapping PRC AUC CI calculation ------
# 1. Resample scores r1 times
r1 <- 10
scores <-  MDF$CAD4TB6
labels <-  MDF$Xpert2Outcome_num
resampled_scores <- replicate(r1, sample(scores, replace=TRUE))
# Calculate curves (single model with multiple datasets)
smdat1 <- mmdata(resampled_scores, labels, modnames=rep("m1", r1), dsids=1:r1)
smcurves1 <- evalmod(smdat1)
auc_ci(smcurves1)

# 2 Resample labels r2 times
scores <-  MDF$CAD4TB6
labels <-  MDF$Xpert2Outcome_num
# Create bootstrapped labels
r2 <- 20
resampled_labels <- replicate(r2, sample(labels, replace=TRUE))

# Calculate curves (single model with multiple datasets)
smdat2 <- mmdata(replicate(r2, scores), resampled_labels, modnames=rep("m1", r2), dsids=1:r2)
smcurves2 <- evalmod(smdat2)
auc_ci(smcurves2) 


### Bootstrapping ROC AUC CI calculation --- 
####2. ROC: pROC --------


## 2.1 Created the roc of individual CAD product
## Change the CAD software: CAD4TB6 qXRv3_100 LunitScore_100 JF1_100 IF2_100

## 2.2 different subgroup
# Age group
roc_young <- roc(Xpert2Outcome_num ~ IF2_100, Young)
roc_middle <- roc(Xpert2Outcome_num ~ IF2_100, Middle)
roc_senior <- roc(Xpert2Outcome_num ~ IF2_100, Senior)

# Patient source
roc_private <- roc(Xpert2Outcome_num ~ IF2_100, PrivateReferral)
roc_public <- roc(Xpert2Outcome_num ~ IF2_100, PublicReferral)
roc_dots <- roc(Xpert2Outcome_num ~ IF2_100, DOTS)
roc_walkin <- roc(Xpert2Outcome_num ~ IF2_100, WalkIn)
roc_community <- roc(Xpert2Outcome_num ~ IF2_100, Community)
roc_contacts <- roc(Xpert2Outcome_num ~ IF2_100, Contacts)


# TB History
roc_new <- roc(Xpert2Outcome_num ~ IF2_100, New)
roc_old <- roc(Xpert2Outcome_num ~ IF2_100, Old)

# Gender
roc_female <- roc(Xpert2Outcome_num ~ IF2_100, Female)
roc_male <- roc(Xpert2Outcome_num ~ IF2_100, Male)


## 2.3. calculate the p-value
# Age group
roc.test(roc_young, roc_middle)$p.value
roc.test(roc_young, roc_senior)$p.value
roc.test(roc_middle, roc_senior)$p.value
# Patient source
roc.test(roc_private, roc_public)$p.value
roc.test(roc_private, roc_dots)$p.value
roc.test(roc_private, roc_walkin)$p.value
roc.test(roc_private, roc_community)$p.value
roc.test(roc_private, roc_contacts)$p.value

roc.test(roc_public, roc_dots)$p.value
roc.test(roc_public, roc_walkin)$p.value
roc.test(roc_public, roc_community)$p.value
roc.test(roc_public, roc_contacts)$p.value

roc.test(roc_dots, roc_walkin)$p.value
roc.test(roc_dots, roc_community)$p.value
roc.test(roc_dots, roc_contacts)$p.value

roc.test(roc_walkin, roc_community)$p.value
roc.test(roc_walkin, roc_contacts)$p.value

roc.test(roc_community, roc_contacts)$p.value

# TB History
roc.test(roc_new, roc_old)$p.value
# Gender
roc.test(roc_female, roc_male)$p.value











