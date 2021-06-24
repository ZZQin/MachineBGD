source("radiologist.R")
CAD_Xpert_plot <- read.csv("Results/CAD_Xpert Cutoffs TABLE_CAD6.3.csv")

# MDF <- read.csv("DataWrangling/MDF.6.3.csv")
# MDF <- read.csv(file = "C:/Users/zhizh/OneDrive - Stop TB Partnership/UNOPS/10 Paper Writing/CAR software/03 Nepal_Cameroon/CAR -- 03 Nepal_Cameroon/Nepal_Cameroon.csv")

CAD_Xpert_plot <- subset(CAD_Xpert_plot, CAD_Xpert_plot$Site %in% "BGD")

Radiologist <- Radiologist[Radiologist$Referral %in% "MDF", ]

# CAD_Xpert_plot <- CAD_Xpert_plot[!CAD_Xpert_plot$DeepLearningSystem %in% c("IF1", "JF2", "IF3"), ]

CAD_Xpert_plot <- CAD_Xpert_plot[!CAD_Xpert_plot$DeepLearningSystem %in% c("IF1", "JF2", "IF3", "InferReadDR", "JF CXR-1", "Lunit INSIGHT CXR", "qXR"), ]

roc_CAD6 <- ci.auc(Xpert2Outcome_num ~ CAD4TB6, MDF)
roc_qure <- ci.auc(Xpert2Outcome_num ~ qXRv3_100, MDF)
roc_Lunit <- ci.auc(Xpert2Outcome_num ~ LunitScore_100, MDF)
roc_JF1 <- ci.auc(Xpert2Outcome_num ~ JF1_100, MDF)
roc_IF2 <- ci.auc(Xpert2Outcome_num ~ IF2_100, MDF)

roc_CAD6 <- roc(Xpert2Outcome_num ~ CAD4TB6, MDF)
roc_qure <- roc(Xpert2Outcome_num ~ qXRv3_100, MDF)
roc_Lunit <- roc(Xpert2Outcome_num ~ LunitScore_100, MDF)
roc_JF1 <- roc(Xpert2Outcome_num ~ JF1_100, MDF)
roc_IF2 <- roc(Xpert2Outcome_num ~ IF2_100, MDF)

roc.test(roc_CAD6, roc_qure)
roc.test(roc_CAD6, roc_Lunit)
roc.test(roc_CAD6, roc_JF1)
roc.test(roc_CAD6, roc_IF2)
roc.test(roc_qure, roc_Lunit)

roc.test(roc_qure, roc_JF1)
roc.test(roc_qure, roc_IF2)
roc.test(roc_Lunit, roc_JF1)
roc.test(roc_Lunit, roc_IF2)
roc.test(roc_JF1, roc_IF2)

### PRC 

library("PRROC")

pr.curve(scores.class0 = MDF[MDF$Xpert2Outcome_num %in% "0", 29], scores.class1 = MDF[MDF$Xpert2Outcome_num %in% "1", 29])
  


# base <- ggplot(CAD_Xpert_plot, aes(X, Sens, color = DeepLearningSystem)) + geom_path()+ geom_ribbon(aes(x = X, ymin = Sens_L, ymax = Sens_H), fill = "red", alpha= 0.2)

base <- ggplot(CAD_Xpert_plot, aes(X, Sens)) + geom_path(aes(color = DeepLearningSystem)) + 
  geom_ribbon(aes(x = X, ymin = Sens_L, ymax = Sens_H, color = DeepLearningSystem, fill = DeepLearningSystem), alpha= 0.2) 


# ggROC <- base + theme_light() + geom_abline(slope=1, intercept = 0, linetype = "dashed", alpha=0.7, color = "grey") + coord_equal()+ annotate("text", x = .75, y = .15, label = paste("Lunit INSIGHT CXR: ", round(roc_Lunit[2],2), " (95% CI:", round(roc_Lunit[1],2), "-", round(roc_Lunit[3],2), ")", "\n", "qXR: ", round(roc_qure[2],2), " (95% CI:", round(roc_qure[1],2), "-", round(roc_qure[3],2), ")", "\n", "InferReadDR: ", round(roc_IF2[2],2), " (95% CI:", round(roc_IF2[1],2), "-", round(roc_IF2[3],2), ")", "\n", "JF CXR-1: ", round(roc_JF1[2],2), " (95% CI:", round(roc_JF1[1],2), "-", round(roc_JF1[3],2), ")", "\n",  "CAD4TB: ", round(roc_CAD6[2],2), " (95% CI:", round(roc_CAD6[1],2), "-", round(roc_CAD6[3],2), ")", sep = ""), size = 3) +
# labs(x = "1-Specificity", y= "Sensitivity", subtitle = paste0("Figure X: The ROC curves of  CAD4TB(v6), Infervision(v2), JF CXR-1, Lunit(4.9.0) and qXR(v2) using Xpert results as the reference (n=", length(MDF$PID_OMRS), ")")) 


### GDG for the top 3 only ****
# ggROC <- base + theme_light() + geom_abline(slope=1, intercept = 0, linetype = "dashed", alpha=0.7, color = "grey") + coord_equal()+ annotate("text", x = .75, y = .15, label = paste("Lunit INSIGHT CXR: ", round(roc_Lunit[2],2), " (95% CI:", round(roc_Lunit[1],2), "-", round(roc_Lunit[3],2), ")", "\n", "qXR: ", round(roc_qure[2],2), " (95% CI:", round(roc_qure[1],2), "-", round(roc_qure[3],2), ")", "\n",  "CAD4TB: ", round(roc_CAD6[2],2), " (95% CI:", round(roc_CAD6[1],2), "-", round(roc_CAD6[3],2), ")", sep = ""), size = 3) +
  # labs(x = "1-Specificity", y= "Sensitivity", subtitle = paste0("Figure X: The ROC curves of  CAD4TB, Lunit and qXR using Xpert results as the reference (n=", length(MDF$PID_OMRS), ")")) 


### cad4tb 6.3 only ****
ggROC <- base + theme_light() + geom_abline(slope=1, intercept = 0, linetype = "dashed", alpha=0.7, color = "grey") + coord_equal()+ annotate("text", x = .75, y = .15, label = paste("CAD4TB: ", round(roc_CAD6[2],2), " (95% CI:", round(roc_CAD6[1],2), "-", round(roc_CAD6[3],2), ")", sep = ""), size = 3) +
  labs(x = "1-Specificity", y= "Sensitivity", subtitle = paste0("The ROC curves of  CAD4TB v6.3 using Xpert results as the reference (n=", length(MDF$PID_OMRS), ")")) 

### GDG for the top 3 only ###
ggROC <- ggROC + geom_point(data = Radiologist, mapping = aes(X, Sens, shape = AccuracyTableCategory))  
ggROC


tiff("Results/Figure-2 ROCs.tif", width = 10, height = 7.9, units = "in", res = 100)
ggROC
dev.off()

ggROC1 <- ggROC + coord_cartesian(xlim = c(0.25, 0.5), ylim = c(0.90, 1) ) +
  ggtitle("ROC")+theme_light()

tiff("Results/ROC Zoomed-in", width = 8, height = 3.2, units = "in", res = 100)
ggROC1
dev.off()


AUC <- paste("Lunit INSIGHT CXR: ", round(roc_Lunit[2],4), " (95% CI:", round(roc_Lunit[1],4), "-", round(roc_Lunit[3],4), ")", ", ", 
             
             "qXR: ", round(roc_qure[2],4), " (95% CI:", round(roc_qure[1],4), "-", round(roc_qure[3],4), ")", ", ",
             
             "InferReadDR: ", round(roc_IF2[2],4), " (95% CI:", round(roc_IF2[1],4), "-", round(roc_IF2[3],4), ")", ", ", 
             
             "JF CXR-1: ", round(roc_JF1[2],4), " (95% CI:", round(roc_JF1[1],4), "-", round(roc_JF1[3],4), ")", ", ",  
             
             "CAD4TB: ", round(roc_CAD6[2],4), " (95% CI:", round(roc_CAD6[1],4), "-", round(roc_CAD6[3],4), ")",sep = "")



rm(roc_CAD6, roc_JF1, roc_JF2, roc_qure, roc_Lunit, roc_IF1, roc_IF2, roc_IF3, dat)
