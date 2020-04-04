source("DataWrangling/GlobalOption.R")

CAD_Xpert_plot <- read.csv("Results/CAD_Xpert Cutoffs TABLE.csv")


CAD_Xpert_plot <- subset(CAD_Xpert_plot, CAD_Xpert_plot$Site %in% "BGD")

### d. Sensitivity vs Score --------------------
base <- ggplot(CAD_Xpert_plot, aes(Score, Sens)) + geom_path(aes(color = DeepLearningSystem)) + scale_fill_manual(values=c("#e41a1c", "#377eb8", "#4daf4a", "#984ea3", "#ff7f00")) +  scale_color_manual(values=c("#e41a1c", "#377eb8", "#4daf4a", "#984ea3", "#ff7f00")) 
# + geom_ribbon(aes(x = Score, ymin = Sens_L, ymax = Sens_H, color = DeepLearningSystem, fill = DeepLearningSystem), alpha= 0.2) 

sensCurve <- base + theme_light() + coord_equal() + labs(x = "Abnormality Score", y= "Sensitivity", subtitle = paste0("d. The Sensitivity vs abnormality score (n=", length(MDF$PID_OMRS), ")")) +theme(legend.position = c(0.05,0.05),legend.justification = c("left", "bottom")) + theme(panel.grid.minor = element_line(colour="grey", size=0.5)) + scale_x_continuous(minor_breaks = seq(0 , 1, 0.1), breaks = seq(0, 1, 0.1))+ scale_y_continuous(minor_breaks = seq(0 , 1, 0.1), breaks = seq(0, 1, 0.1)) + geom_vline(xintercept = c(0.8, 0.7), linetype="dotted", color = "black", size=1)  




### f. NNT --------------------
base <- ggplot(CAD_Xpert_plot, aes(Score, NNT)) + geom_path(aes(color = DeepLearningSystem)) + scale_fill_manual(values=c("#e41a1c", "#377eb8", "#4daf4a", "#984ea3", "#ff7f00")) +  scale_color_manual(values=c("#e41a1c", "#377eb8", "#4daf4a", "#984ea3", "#ff7f00")) 
# + geom_ribbon(data = CAD_Xpert_plot %>% filter(Score <0.95), aes(x = Score, ymin = NNT_L, ymax = NNT_H, color = DeepLearningSystem, fill = DeepLearningSystem), alpha= 0.2) 

NNTCurve <- base + theme_light()  + labs(x = "Abnormality Score", y= "NNT", subtitle = paste0("f. The NNT vs abnormality score (n=", length(MDF$PID_OMRS), ")")) +theme(legend.position = "none") + theme(panel.grid.minor = element_line(colour="grey", size=0.5)) + scale_x_continuous(minor_breaks = seq(0 , 1, 0.1), breaks = seq(0, 1, 0.1))+ geom_vline(xintercept = c(0.8, 0.7), linetype="dotted", color = "black", size=1) 
# +theme(legend.position = c(0.05,0.95), legend.justification = c("left", "top"))


### e. Xpert Saved vs Score --------------------
base <- ggplot(CAD_Xpert_plot, aes(Score, X.XpertSaved)) + geom_path(aes(color = DeepLearningSystem))  + scale_fill_manual(values=c("#e41a1c", "#377eb8", "#4daf4a", "#984ea3", "#ff7f00")) +  scale_color_manual(values=c("#e41a1c", "#377eb8", "#4daf4a", "#984ea3", "#ff7f00")) 

XpertSavingCurve <- base + theme_light() + coord_equal() + labs(x = "e. Abnormality Score", y= "Xpert Saved", subtitle = paste0("e. The Xpert Saved vs abnormality score (n=", length(MDF$PID_OMRS), ")")) +theme(legend.position = "none") + theme(panel.grid.minor = element_line(colour="grey", size=0.5)) + scale_x_continuous(minor_breaks = seq(0 , 1, 0.1), breaks = seq(0, 1, 0.1))+ scale_y_continuous(minor_breaks = seq(0 , 1, 0.1), breaks = seq(0, 1, 0.1))+ geom_vline(xintercept = c(0.8, 0.7), linetype="dotted", color = "black", size=1) 
# +theme(  legend.position = c(0.05,0.95),  legend.justification = c("left", "top"))

### c. Sens vs Xpert Saved--------------------
base <- ggplot(CAD_Xpert_plot, aes(X.XpertSaved, Sens)) + geom_path(aes(color = DeepLearningSystem, fill = DeepLearningSystem))  + scale_fill_manual(values=c("#e41a1c", "#377eb8", "#4daf4a", "#984ea3", "#ff7f00")) +  scale_color_manual(values=c("#e41a1c", "#377eb8", "#4daf4a", "#984ea3", "#ff7f00")) + 
  geom_ribbon(aes(x = X.XpertSaved, ymin = Sens_L, ymax = Sens_H, color = DeepLearningSystem, fill = DeepLearningSystem), alpha= 0.2) 

XpertSens <- base + theme_light() + coord_equal() + labs(x = "Xpert Saved", y= "Sensitivity", subtitle = paste0("c. The Xpert Saved vs sensitivity (n=", length(MDF$PID_OMRS), ")")) +theme(legend.position = c(0.05,0.05), legend.justification = c("left", "bottom")) + theme(panel.grid.minor = element_line(colour="grey", size=0.5)) + scale_x_continuous(minor_breaks = seq(0 , 1, 0.1), breaks = seq(0, 1, 0.1))+ scale_y_continuous(minor_breaks = seq(0 , 1, 0.1), breaks = seq(0, 1, 0.1)) + geom_vline(xintercept = c(0.5, 2/3), linetype="dotted", color = c("yellow", "blue"), size=1)
XpertSens

### Specificity vs Score --------------------
base <- ggplot(CAD_Xpert_plot, aes(Score, Spec)) + geom_path(aes(color = DeepLearningSystem))  + scale_fill_manual(values=c("#e41a1c", "#377eb8", "#4daf4a", "#984ea3", "#ff7f00")) +  scale_color_manual(values=c("#e41a1c", "#377eb8", "#4daf4a", "#984ea3", "#ff7f00")) + 
  geom_ribbon(data = CAD_Xpert_plot %>% filter(Score <0.95), aes(x = Score, ymin = Spec_L, ymax = Spec_H, color = DeepLearningSystem, fill = DeepLearningSystem), alpha= 0.2) 

SpecScore <- base + theme_light() + coord_equal() + labs(x = "Score", y= "Specificity", subtitle = paste0("The specificity vs abnormality score (n=", length(MDF$PID_OMRS), ")")) +theme(
  legend.position = c(0.05,0.95),
  legend.justification = c("left", "top")) + theme(panel.grid.minor = element_line(colour="grey", size=0.5)) + scale_x_continuous(minor_breaks = seq(0 , 1, 0.1), breaks = seq(0, 1, 0.1))+ scale_y_continuous(minor_breaks = seq(0 , 1, 0.1), breaks = seq(0, 1, 0.1))+ geom_vline(xintercept = c(0.8, 0.7), linetype="dotted", color = "black", size=1)+theme(legend.position = "none") 

#### a. ROC --------------------
CAD_Xpert_plot$X_L <- 1-CAD_Xpert_plot$Spec_H
CAD_Xpert_plot$X_H <- 1-CAD_Xpert_plot$Spec_L

base <- ggplot(CAD_Xpert_plot, aes(X, Sens)) + geom_path(aes(color = DeepLearningSystem)) + geom_ribbon(aes(x = X, ymin = Sens_L, ymax = Sens_H, color = DeepLearningSystem, fill = DeepLearningSystem), alpha= 0.2) + scale_fill_manual(values=c("#e41a1c", "#377eb8", "#4daf4a", "#984ea3", "#ff7f00")) +  scale_color_manual(values=c("#e41a1c", "#377eb8", "#4daf4a", "#984ea3", "#ff7f00")) 
# + geom_ribbon(aes(y = Sens, xmin = X_L, xmax = X_H, color = DeepLearningSystem, fill = DeepLearningSystem), alpha= 0.2) 

ROC <- base + theme_light() + geom_abline(slope=1, intercept = 0, linetype = "dashed", alpha=0.7, color = "grey") + coord_equal() +   labs(x = "1-Specificity", y= "Sensitivity", subtitle = paste0("a. The ROC curves (n=", length(MDF$PID_OMRS), ")"))  +theme(legend.position = c(0.95,0.05), legend.justification = c("right", "bottom")) + theme(panel.grid.minor = element_line(colour="grey", size=0.5)) + scale_x_continuous(minor_breaks = seq(0 , 1, 0.1), breaks = seq(0, 1, 0.1))+ scale_y_continuous(minor_breaks = seq(0 , 1, 0.1), breaks = seq(0, 1, 0.1)) + theme(legend.position = "none")

ggROC <- ROC + theme(legend.position = "none") 

### b. PRC ------
base <- ggplot(CAD_Xpert_plot, aes(Sens, ppv)) + geom_path(aes(color = DeepLearningSystem, fill = DeepLearningSystem)) + scale_fill_manual(values=c("#e41a1c", "#377eb8", "#4daf4a", "#984ea3", "#ff7f00")) +  scale_color_manual(values=c("#e41a1c", "#377eb8", "#4daf4a", "#984ea3", "#ff7f00")) + 
  geom_ribbon(data = CAD_Xpert_plot %>% filter(Sens >0.02), aes(x = Sens, ymin = PPV_L, ymax = PPV_H, color = DeepLearningSystem, fill = DeepLearningSystem), alpha= 0.2) 

PRC <- base + theme_light() + coord_equal() + labs(x = "Recall (Sensitivity)", y= "Precision (PPV)", subtitle = paste0("b. The Precision Recall Curve (n=", length(MDF$PID_OMRS), ")"))  + theme(legend.position = "none") 

# +theme(legend.position = c(0.95,-0.7), legend.justification = c("right", "bottom")) 



### Save --------------------

tiff("Results/5 Curves.tif", width = 10, height = 15, units = "in", res = 100)
require(gridExtra)
grid.arrange(ggROC, sensCurve, PRC, XpertSavingCurve, XpertSens, NNTCurve,  nrow=3)

# grid.arrange(sensCurve, ggROC, XpertSavingCurve, PRC, NNTCurve, XpertSens, nrow=3)

dev.off()
