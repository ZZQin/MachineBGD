##################################### Inital ##################
# subgroup 4 curves
SubgroupPlot <- read.csv("Chapter/Subgroup Table/SubgroupTable.csv")
SubgroupPlot <- SubgroupPlot[, -1]
SubgroupPlot$Score[SubgroupPlot$DeepLearningSystem %in% "CAD4TB"] <- SubgroupPlot$Score[SubgroupPlot$DeepLearningSystem %in% "CAD4TB"]/100
SubgroupPlot$subgroup <- as.character(SubgroupPlot$subgroup)
SubgroupPlot$subgroup[SubgroupPlot$subgroup %in% "New"] <- "1.New Case"
SubgroupPlot$subgroup[SubgroupPlot$subgroup %in% "Old"] <- "2.Previously Treated Case"
SubgroupPlot$subgroup[SubgroupPlot$subgroup %in% "Young"] <- "3.Young age [15,25)"
SubgroupPlot$subgroup[SubgroupPlot$subgroup %in% "Middle"] <- "4.Middle age [25,60)"
SubgroupPlot$subgroup[SubgroupPlot$subgroup %in% "Senior"] <- "5.Old age [60,108]"
SubgroupPlot$subgroup[SubgroupPlot$subgroup %in% "DOTS"] <- "7.Public DOTS Retesting"
SubgroupPlot$subgroup[SubgroupPlot$subgroup %in% "Referral"] <-"6.Private Public Referral"
SubgroupPlot$subgroup[SubgroupPlot$subgroup %in% "WalkIn"] <-"8.Walk In"
SubgroupPlot$subgroup <- as.factor(SubgroupPlot$subgroup)

SubgroupPlotOrigin <- SubgroupPlot
# SubgroupPlot <- SubgroupPlotOrigin
# Choose the DL System
# SubgroupPlot <- SubgroupPlotOrigin[SubgroupPlotOrigin$DeepLearningSystem %in% "CAD4TB", ]
# SubgroupPlot <- SubgroupPlotOrigin[SubgroupPlotOrigin$DeepLearningSystem %in% "IF2", ]
# SubgroupPlot <- SubgroupPlotOrigin[SubgroupPlotOrigin$DeepLearningSystem %in% "JF1", ]
# SubgroupPlot <- SubgroupPlotOrigin[SubgroupPlotOrigin$DeepLearningSystem %in% "Lunit", ]
SubgroupPlot <- SubgroupPlotOrigin[SubgroupPlotOrigin$DeepLearningSystem %in% "qXR", ]

### GO TO AND RUN source("Chapter/Subgroup Table/SubGroupPlot.R")
###################################################################




SubgroupPlot$NNT <- 1/SubgroupPlot$ppv
SubgroupPlot$NNT_H <- 1/SubgroupPlot$PPV_L
SubgroupPlot$NNT_L <- 1/SubgroupPlot$PPV_H

### Sensitivity vs Score --------------------
base <- ggplot(SubgroupPlot, aes(Score, Sens)) + geom_path(aes(color = subgroup)) + 
  geom_ribbon(aes(x = Score, ymin = Sens_L, ymax = Sens_H, color = subgroup, fill = subgroup), alpha= 0.2) 

sensCurve <- base + theme_light() + coord_equal() + labs(x = "Abnormality Score", y= "Sensitivity", subtitle = paste0("The Sensitivity vs abnormality score (n=", length(MDF$PID_OMRS), ")")) +theme(legend.position = c(0.05,0.05),legend.justification = c("left", "bottom")) + theme(panel.grid.minor = element_line(colour="grey", size=0.5)) + scale_x_continuous(minor_breaks = seq(0 , 1, 0.1), breaks = seq(0, 1, 0.1))+ scale_y_continuous(minor_breaks = seq(0 , 1, 0.1), breaks = seq(0, 1, 0.1)) + geom_vline(xintercept = c(0.8, 0.7, 0.6), linetype="dotted", color = "blue", size=1) +theme(legend.position = "none")
# sensCurveZoomIn <- sensCurve + coord_cartesian(xlim = c(0.5, 0.9), ylim = c(0.90, 1) ) +
# ggtitle("Figure 2b: Zoomed-in view of the square marked part of Fig. 2a")+theme_light()



### NNT --------------------
base <- ggplot(SubgroupPlot, aes(Score, NNT)) + geom_path(aes(color = subgroup)) 
# + geom_ribbon(data = SubgroupPlot %>% filter(Score <0.95), aes(x = Score, ymin = NNT_L, ymax = NNT_H, color = subgroup, fill = subgroup), alpha= 0.2) 

NNTCurve <- base + theme_light()  + labs(x = "Abnormality Score", y= "NNT", subtitle = paste0("The NNT vs abnormality score (n=", length(MDF$PID_OMRS), ")")) +theme(legend.position = "none") + theme(panel.grid.minor = element_line(colour="grey", size=0.5)) + scale_x_continuous(minor_breaks = seq(0 , 1, 0.1), breaks = seq(0, 1, 0.1))+ geom_vline(xintercept = c(0.8, 0.7, 0.6), linetype="dotted", color = "blue", size=1)
# +theme(legend.position = c(0.05,0.95), legend.justification = c("left", "top"))


### Xpert Saved vs Score --------------------
base <- ggplot(SubgroupPlot, aes(Score, X.XpertSaved)) + geom_path(aes(color = subgroup))  

XpertSavingCurve <- base + theme_light() + coord_equal() + labs(x = "Abnormality Score", y= "Xpert Saved", subtitle = paste0("The Xpert Saved vs abnormality score (n=", length(MDF$PID_OMRS), ")")) + theme(legend.position = "none") + theme(panel.grid.minor = element_line(colour="grey", size=0.5)) + scale_x_continuous(minor_breaks = seq(0 , 1, 0.1), breaks = seq(0, 1, 0.1))+ scale_y_continuous(minor_breaks = seq(0 , 1, 0.1), breaks = seq(0, 1, 0.1))+ geom_vline(xintercept = c(0.8, 0.7, 0.6), linetype="dotted", color = "blue", size=1)
# +theme(  legend.position = c(0.05,0.95),  legend.justification = c("left", "top"))

### Sens vs Xpert Saved--------------------
base <- ggplot(SubgroupPlot, aes(X.XpertSaved, Sens)) + geom_path(aes(color = subgroup)) + 
  geom_ribbon(aes(x = X.XpertSaved, ymin = Sens_L, ymax = Sens_H, color = subgroup, fill = subgroup), alpha= 0.2) 

XpertSens <- base + theme_light() + coord_equal() + labs(x = "Xpert Saved", y= "Sensitivity", subtitle = paste0("The Xpert Saved vs sensitivity (n=", length(MDF$PID_OMRS), ")")) +theme(legend.position = c(0.05,0.05), legend.justification = c("left", "bottom")) + theme(panel.grid.minor = element_line(colour="grey", size=0.5)) + scale_x_continuous(minor_breaks = seq(0 , 1, 0.1), breaks = seq(0, 1, 0.1))+ scale_y_continuous(minor_breaks = seq(0 , 1, 0.1), breaks = seq(0, 1, 0.1)) 

### Specificity vs Score --------------------
base <- ggplot(SubgroupPlot, aes(Score, Spec)) + geom_path(aes(color = subgroup))  + 
  geom_ribbon(data = SubgroupPlot %>% filter(Score <0.95), aes(x = Score, ymin = Spec_L, ymax = Spec_H, color = subgroup, fill = subgroup), alpha= 0.2) 

SpecScore <- base + theme_light() + coord_equal() + labs(x = "Score", y= "Specificity", subtitle = paste0("The specificity vs abnormality score (n=", length(MDF$PID_OMRS), ")")) +theme(
  legend.position = c(0.05,0.95),
  legend.justification = c("left", "top")) + theme(panel.grid.minor = element_line(colour="grey", size=0.5)) + scale_x_continuous(minor_breaks = seq(0 , 1, 0.1), breaks = seq(0, 1, 0.1))+ scale_y_continuous(minor_breaks = seq(0 , 1, 0.1), breaks = seq(0, 1, 0.1))+ geom_vline(xintercept = c(0.8, 0.7, 0.6), linetype="dotted", color = "blue", size=1)+theme(legend.position = "none")

#### ROC --------------------
SubgroupPlot$X_L <- 1-SubgroupPlot$Spec_H
SubgroupPlot$X_H <- 1-SubgroupPlot$Spec_L

base <- ggplot(SubgroupPlot, aes(X, Sens)) + geom_path(aes(color = subgroup)) + geom_ribbon(aes(x = X, ymin = Se
                                                                                                ns_L, ymax = Sens_H, color = subgroup, fill = subgroup), alpha= 0.2) 
# + geom_ribbon(aes(y = Sens, xmin = X_L, xmax = X_H, color = subgroup, fill = subgroup), alpha= 0.2) 

ROC <- base + theme_light() + geom_abline(slope=1, intercept = 0, linetype = "dashed", alpha=0.7, color = "grey") + coord_equal() +   labs(x = "1-Specificity", y= "Sensitivity", subtitle = paste0("The ROC curves (n=", length(MDF$PID_OMRS), ")"))  +theme(legend.position = c(0.95,0.05), legend.justification = c("right", "bottom")) + theme(panel.grid.minor = element_line(colour="grey", size=0.5)) + scale_x_continuous(minor_breaks = seq(0 , 1, 0.1), breaks = seq(0, 1, 0.1))+ scale_y_continuous(minor_breaks = seq(0 , 1, 0.1), breaks = seq(0, 1, 0.1))


### PRC -----------------------------
base <- ggplot(SubgroupPlot, aes(Sens, ppv)) + geom_path(aes(color = subgroup)) + 
  geom_ribbon(data = SubgroupPlot %>% filter(Sens <0.90), aes(x = Sens, ymin = PPV_L, ymax = PPV_H, color = subgroup, fill = subgroup), alpha= 0.2) 

PRC <- base + theme_light() + coord_equal() + labs(x = "Recall (Sensitivity)", y= "Precision (PPV)", subtitle = paste0("The PRC of CAD4TB(v6), Infervision(v2), JF CXR-1, Lunit(4.9.0) and qXR(v2) using Xpert results as the reference (n=", length(MDF$PID_OMRS), ")")) +theme(legend.position = "none") 



### Save


tiff("Results/Subgroup 5 Curves.tif", width = 10, height = 15, units = "in", res = 100)
require(gridExtra)
grid.arrange(sensCurve, ROC, XpertSavingCurve, PRC, NNTCurve, nrow=3)

dev.off()

