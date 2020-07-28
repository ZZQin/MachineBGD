# Specific sampling methods - 1.5 non-TB cases per TB case

Bp <- MDF[MDF$GXP.Result %in% "MTB Detected", ]
Bn <- MDF[MDF$GXP.Result %in% "MTB Not Detected", ]

BnS <- Bn[Bn$TID_OMRS %in% sample(Bn$TID_OMRS, 5450, replace = F), ]

Resampled <- rbind(Bp, BnS)
rm(Bp, Bn, BnS)

# Summary of the resampled dataset
summary(Resampled)
names(Resampled)



#### Qure.AI in the resampled dataset
paste("Qure.AI in the resampled dataset", "AUC=", round(ci.auc(Xpert2Outcome_num ~ qXRv3, Resampled), 3))

#### QureAI in the original bangladesh dataset ########
paste("QureAI in the original bangladesh dataset", "AUC=", round(ci.auc(Xpert2Outcome_num ~ qXRv3, MDF), 3))

#### CAD4TB in the resampled dataset ####
paste("CAD4TB in the resampled dataset","AUC=", round(ci.auc(Xpert2Outcome_num ~ CAD4TB6, Resampled), 3))

#### CAD4TB in the original bangladesh dataset ########
paste("CAD4TB in the original bangladesh dataset","AUC=", round(ci.auc(Xpert2Outcome_num ~ CAD4TB6, MDF), 3))

#### Lunit in the resampled dataset ####
paste("Lunit in the resampled dataset","AUC=", round(ci.auc(Xpert2Outcome_num ~ LunitScore, Resampled), 3))

#### Lunit in the original bangladesh dataset ########
paste("Lunit in the original bangladesh dataset", "AUC=", round(ci.auc(Xpert2Outcome_num ~ LunitScore, MDF), 3))

