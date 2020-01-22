source("DataWrangling/GlobalOption.R")

CAD_Xpert_plot <- read.csv("Results/CAD_Xpert Cutoffs TABLE 4 digits.csv")
CAD_Xpert_plot <- CAD_Xpert_plot[, -1]
CAD_Xpert_plot$NNT <- 1/CAD_Xpert_plot$ppv
CAD_Xpert_plot$NNT_H <- 1/CAD_Xpert_plot$PPV_L
CAD_Xpert_plot$NNT_L <- 1/CAD_Xpert_plot$PPV_H


CAD_Xpert_plot <- CAD_Xpert_plot[!CAD_Xpert_plot$DeepLearningSystem %in% c("IF1", "JF2", "IF3"), ]
CAD_Xpert_plot$Score[CAD_Xpert_plot$DeepLearningSystem %in% "CAD4TB"] <- CAD_Xpert_plot$Score[CAD_Xpert_plot$DeepLearningSystem %in% "CAD4TB"]/100

### Sensitivity vs Score --------------------
base <- ggplot(CAD_Xpert_plot, aes(Score, Sens)) + geom_path(aes(color = DeepLearningSystem)) + 
  geom_ribbon(aes(x = Score, ymin = Sens_L, ymax = Sens_H, color = DeepLearningSystem, fill = DeepLearningSystem), alpha= 0.2) 

sensCurve <- base + theme_light() + coord_equal() + labs(x = "Abnormality Score", y= "Sensitivity", subtitle = paste0("The Sensitivity vs abnormality score (n=", length(MDF$PID_OMRS), ")")) +theme(legend.position = c(0.05,0.05),legend.justification = c("left", "bottom")) + theme(panel.grid.minor = element_line(colour="grey", size=0.5)) + scale_x_continuous(minor_breaks = seq(0 , 1, 0.1), breaks = seq(0, 1, 0.1))+ scale_y_continuous(minor_breaks = seq(0 , 1, 0.1), breaks = seq(0, 1, 0.1)) + geom_vline(xintercept = c(0.75, 0.5), linetype="dotted", color = "blue", size=1) +theme(legend.position = "none")
# sensCurveZoomIn <- sensCurve + coord_cartesian(xlim = c(0.5, 0.9), ylim = c(0.90, 1) ) +
# ggtitle("Figure 2b: Zoomed-in view of the square marked part of Fig. 2a")+theme_light()



### NNT --------------------
base <- ggplot(CAD_Xpert_plot, aes(Score, NNT)) + geom_path(aes(color = DeepLearningSystem)) 
# + geom_ribbon(data = CAD_Xpert_plot %>% filter(Score <0.95), aes(x = Score, ymin = NNT_L, ymax = NNT_H, color = DeepLearningSystem, fill = DeepLearningSystem), alpha= 0.2) 

NNTCurve <- base + theme_light()  + labs(x = "Abnormality Score", y= "NNT", subtitle = paste0("The NNT vs abnormality score (n=", length(MDF$PID_OMRS), ")")) +theme(legend.position = "none") + theme(panel.grid.minor = element_line(colour="grey", size=0.5)) + scale_x_continuous(minor_breaks = seq(0 , 1, 0.1), breaks = seq(0, 1, 0.1))+ geom_vline(xintercept = c(0.75, 0.5), linetype="dotted", color = "blue", size=1)
# +theme(legend.position = c(0.05,0.95), legend.justification = c("left", "top"))


### Xpert Saved vs Score --------------------
base <- ggplot(CAD_Xpert_plot, aes(Score, X.XpertSaved)) + geom_path(aes(color = DeepLearningSystem))  

XpertSavingCurve <- base + theme_light() + coord_equal() + labs(x = "Abnormality Score", y= "Xpert Saved", subtitle = paste0("The Xpert Saved vs abnormality score (n=", length(MDF$PID_OMRS), ")")) + theme(legend.position = "none") + theme(panel.grid.minor = element_line(colour="grey", size=0.5)) + scale_x_continuous(minor_breaks = seq(0 , 1, 0.1), breaks = seq(0, 1, 0.1))+ scale_y_continuous(minor_breaks = seq(0 , 1, 0.1), breaks = seq(0, 1, 0.1))+ geom_vline(xintercept = c(0.75, 0.5), linetype="dotted", color = "blue", size=1)
# +theme(  legend.position = c(0.05,0.95),  legend.justification = c("left", "top"))

### Sens vs Xpert Saved--------------------
base <- ggplot(CAD_Xpert_plot, aes(X.XpertSaved, Sens)) + geom_path(aes(color = DeepLearningSystem)) + 
  geom_ribbon(aes(x = X.XpertSaved, ymin = Sens_L, ymax = Sens_H, color = DeepLearningSystem, fill = DeepLearningSystem), alpha= 0.2) 

XpertSens <- base + theme_light() + coord_equal() + labs(x = "Xpert Saved", y= "Sensitivity", subtitle = paste0("The Xpert Saved vs sensitivity (n=", length(MDF$PID_OMRS), ")")) +theme(legend.position = c(0.05,0.05), legend.justification = c("left", "bottom")) + theme(panel.grid.minor = element_line(colour="grey", size=0.5)) + scale_x_continuous(minor_breaks = seq(0 , 1, 0.1), breaks = seq(0, 1, 0.1))+ scale_y_continuous(minor_breaks = seq(0 , 1, 0.1), breaks = seq(0, 1, 0.1)) 

### Specificity vs Score --------------------
base <- ggplot(CAD_Xpert_plot, aes(Score, Spec)) + geom_path(aes(color = DeepLearningSystem))  + 
  geom_ribbon(data = CAD_Xpert_plot %>% filter(Score <0.95), aes(x = Score, ymin = Spec_L, ymax = Spec_H, color = DeepLearningSystem, fill = DeepLearningSystem), alpha= 0.2) 

SpecScore <- base + theme_light() + coord_equal() + labs(x = "Score", y= "Specificity", subtitle = paste0("The specificity vs abnormality score (n=", length(MDF$PID_OMRS), ")")) +theme(
  legend.position = c(0.05,0.95),
  legend.justification = c("left", "top")) + theme(panel.grid.minor = element_line(colour="grey", size=0.5)) + scale_x_continuous(minor_breaks = seq(0 , 1, 0.1), breaks = seq(0, 1, 0.1))+ scale_y_continuous(minor_breaks = seq(0 , 1, 0.1), breaks = seq(0, 1, 0.1))+ geom_vline(xintercept = c(0.75, 0.5), linetype="dotted", color = "blue", size=1)+theme(legend.position = "none")

#### ROC --------------------
CAD_Xpert_plot$X_L <- 1-CAD_Xpert_plot$Spec_H
CAD_Xpert_plot$X_H <- 1-CAD_Xpert_plot$Spec_L

base <- ggplot(CAD_Xpert_plot, aes(X, Sens)) + geom_path(aes(color = DeepLearningSystem)) + geom_ribbon(aes(x = X, ymin = Sens_L, ymax = Sens_H, color = DeepLearningSystem, fill = DeepLearningSystem), alpha= 0.2) 
# + geom_ribbon(aes(y = Sens, xmin = X_L, xmax = X_H, color = DeepLearningSystem, fill = DeepLearningSystem), alpha= 0.2) 

ROC <- base + theme_light() + geom_abline(slope=1, intercept = 0, linetype = "dashed", alpha=0.7, color = "grey") + coord_equal() +   labs(x = "1-Specificity", y= "Sensitivity", subtitle = paste0("The ROC curves (n=", length(MDF$PID_OMRS), ")"))  +theme(legend.position = c(0.95,0.05), legend.justification = c("right", "bottom")) + theme(panel.grid.minor = element_line(colour="grey", size=0.5)) + scale_x_continuous(minor_breaks = seq(0 , 1, 0.1), breaks = seq(0, 1, 0.1))+ scale_y_continuous(minor_breaks = seq(0 , 1, 0.1), breaks = seq(0, 1, 0.1))
### Save --------------------

tiff("Results/4 Curves.tif", width = 10, height = 10, units = "in", res = 100)
require(gridExtra)
grid.arrange(ROC, sensCurve, XpertSavingCurve, NNTCurve, nrow=2)

dev.off()
