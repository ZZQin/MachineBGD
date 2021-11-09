# source("2.0 Version Comparison/radiologist.R")
Radiologist <- Radiologist[Radiologist$Referral %in% "MDF", ]

roc_CAD6 <- roc(Xpert2Outcome_num ~ CAD4TBv6, MDF)
roc_qure2 <- roc(Xpert2Outcome_num ~ qXRv2, MDF)
roc_CAD7 <- roc(Xpert2Outcome_num ~ CAD4TBv7, MDF)
roc_qure3 <- roc(Xpert2Outcome_num ~ qXRv3, MDF)

roc.test(roc_CAD6, roc_CAD7)
roc.test(roc_qure2, roc_qure3)

roc_CAD6 <- ci.auc(Xpert2Outcome_num ~ CAD4TBv6, MDF)
roc_qure2 <- ci.auc(Xpert2Outcome_num ~ qXRv2, MDF)
roc_CAD7 <- ci.auc(Xpert2Outcome_num ~ CAD4TBv7, MDF)
roc_qure3 <- ci.auc(Xpert2Outcome_num ~ qXRv3, MDF)




## qXR ----
CAD_Xpert_plot <- read.csv("2.0 Version Comparison/Cutoffs TABLE.csv")
CAD_Xpert_plot <- CAD_Xpert_plot[CAD_Xpert_plot$DeepLearningSystem %in% c("qXRv2", "qXRv3"), ]
base <- ggplot(CAD_Xpert_plot, aes(X, Sens)) + geom_path(aes(color = DeepLearningSystem)) + 
  geom_ribbon(aes(x = X, ymin = Sens_L, ymax = Sens_H, color = DeepLearningSystem, fill = DeepLearningSystem), alpha= 0.2) 


ggROC_qure <- base + theme_light() + geom_abline(slope=1, intercept = 0, linetype = "dashed", alpha=0.7, color = "grey") + coord_equal() + annotate("text", x = .75, y = .15, label = paste("qXRv2: ", round(roc_qure2[2],4), "\n"," (95% CI:", round(roc_qure2[1],4), "-", round(roc_qure2[3],4), ")", "\n",
                                                   "qXRv3: ", round(roc_qure3[2],4), "\n", " (95% CI:", round(roc_qure3[1],4), "-", round(roc_qure3[3],4), ")", 
                                                   sep = ""), size = 3) + 
labs(x = "1-Specificity", y= "Sensitivity")+theme(legend.position = "bottom")

## CAD4TB ----
CAD_Xpert_plot <- read.csv("2.0 Version Comparison/Cutoffs TABLE.csv")
CAD_Xpert_plot <- CAD_Xpert_plot[CAD_Xpert_plot$DeepLearningSystem %in% c("CAD4TBv6", "CAD4TBv7"), ]

base <- ggplot(CAD_Xpert_plot, aes(X, Sens)) + geom_path(aes(color = DeepLearningSystem)) + 
  geom_ribbon(aes(x = X, ymin = Sens_L, ymax = Sens_H, color = DeepLearningSystem, fill = DeepLearningSystem), alpha= 0.2) 


ggROC_CAD <- base + theme_light() + geom_abline(slope=1, intercept = 0, linetype = "dashed", alpha=0.7, color = "grey") + coord_equal() + annotate("text", x = .75, y = .15, label = paste("CAD4TBv6: ", round(roc_CAD6[2],4), "\n", " (95% CI:", round(roc_CAD6[1],4), "-", round(roc_CAD6[3],4), ")", "\n",
                       "CAD4TBv7: ", round(roc_CAD7[2],4), "\n", " (95% CI:", round(roc_CAD7[1],4), "-", round(roc_CAD7[3],4), ")", sep = ""), size = 3) +
labs(x = "1-Specificity", y= "Sensitivity")+theme(legend.position = "bottom")



tiff("2.0 Version Comparison/Figure-2 ROCs.tif", width = 8, height = 6, units = "in", res = 100)
require(gridExtra)
grid.arrange(ggROC_CAD, ggROC_qure, nrow=1)
dev.off()





AUC <- paste("qXRv2: ", round(roc_qure2[2],4), " (95% CI:", round(roc_qure2[1],4), "-", round(roc_qure2[3],4), ")", ", ", 
             
             "qXRv3: ", round(roc_qure3[2],4), " (95% CI:", round(roc_qure3[1],4), "-", round(roc_qure3[3],4), ")", ", ",
             
             "CAD4TBv6: ", round(roc_CAD6[2],4), " (95% CI:", round(roc_CAD6[1],4), "-", round(roc_CAD6[3],4), ")", ", ", 
             
             "CAD4TBv7: ", round(roc_CAD7[2],4), " (95% CI:", round(roc_CAD7[1],4), "-", round(roc_CAD7[3],4), ")",sep = "")

