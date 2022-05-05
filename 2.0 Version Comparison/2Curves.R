source("2.0 Version Comparison/Global.R")
source("2.0 Version Comparison/radiologist.R")


# qXR ----
CAD_Xpert_plot <- read.csv("2.0 Version Comparison/Cutoffs TABLE.csv")
CAD_Xpert_plot <- CAD_Xpert_plot[CAD_Xpert_plot$DeepLearningSystem %in% c("qXRv2", "qXRv3"), ]
CAD_Xpert_plot$DeepLearningSystem <- as.character(CAD_Xpert_plot$DeepLearningSystem)


### d. Sensitivity vs Score --------------------
base <- ggplot(CAD_Xpert_plot, aes(Score, Sens)) + geom_path(aes(color = DeepLearningSystem)) + scale_fill_manual(values=c("#e41a1c", "#377eb8", "#4daf4a", "#984ea3", "#ff7f00")) +  scale_color_manual(values=c("#e41a1c", "#377eb8", "#4daf4a", "#984ea3", "#ff7f00")) 
# + geom_ribbon(aes(x = Score, ymin = Sens_L, ymax = Sens_H, color = DeepLearningSystem, fill = DeepLearningSystem), alpha= 0.2) 

sensCurveqXR <- base + theme_light() + labs(x = "Abnormality Score", y= "Sensitivity", subtitle = paste0("a. qXRv2 vs qXRv3: The Sensitivity vs abnormality score (n=", length(MDF$PID_OMRS), ")")) +theme(legend.position = c(5,5),legend.justification = c("left", "bottom")) + theme(panel.grid.minor = element_line(colour="grey", size=0.5)) + scale_x_continuous(minor_breaks = seq(0 , 100, 10), breaks = seq(0, 100, 10))+ scale_y_continuous(minor_breaks = seq(0 , 1, 0.1), breaks = seq(0, 1, 0.1))   



### e. Xpert Saved vs Score --------------------
base <- ggplot(CAD_Xpert_plot, aes(Score, XpertSaved)) + geom_path(aes(color = DeepLearningSystem))  + scale_fill_manual(values=c("#e41a1c", "#377eb8", "#4daf4a", "#984ea3", "#ff7f00")) +  scale_color_manual(values=c("#e41a1c", "#377eb8", "#4daf4a", "#984ea3", "#ff7f00")) 

XpertSavingCurveqXR <- base + theme_light()  + labs(x = "e. Abnormality Score", y= "Xpert Saved", subtitle = paste0("b. qXRv2 vs qXRv3: The Xpert Saved vs abnormality score (n=", length(MDF$PID_OMRS), ")")) + theme(panel.grid.minor = element_line(colour="grey", size=0.5)) + scale_x_continuous(minor_breaks = seq(0 , 100, 10), breaks = seq(0, 100, 10))+ scale_y_continuous(minor_breaks = seq(0 , 1, 0.1), breaks = seq(0, 1, 0.1)) +theme(  legend.position = c(0.05,0.95),  legend.justification = c("left", "top"))



# CAD4TB ----
CAD_Xpert_plot <- read.csv("2.0 Version Comparison/Cutoffs TABLE.csv")
CAD_Xpert_plot <- CAD_Xpert_plot[CAD_Xpert_plot$DeepLearningSystem %in% c("CAD4TBv6", "CAD4TBv7"), ]
CAD_Xpert_plot$DeepLearningSystem <- as.character(CAD_Xpert_plot$DeepLearningSystem)



### d. Sensitivity vs Score --------------------
base <- ggplot(CAD_Xpert_plot, aes(Score, Sens)) + geom_path(aes(color = DeepLearningSystem)) + scale_fill_manual(values=c("#e41a1c", "#377eb8", "#4daf4a", "#984ea3", "#ff7f00")) +  scale_color_manual(values=c("#e41a1c", "#377eb8", "#4daf4a", "#984ea3", "#ff7f00")) 
# + geom_ribbon(aes(x = Score, ymin = Sens_L, ymax = Sens_H, color = DeepLearningSystem, fill = DeepLearningSystem), alpha= 0.2) 

sensCurveCAD4TB <- base + theme_light()  + labs(x = "Abnormality Score", y= "Sensitivity", subtitle = paste0("c. CAD4TBv6 vs CAD4TBv7: The Sensitivity vs abnormality score (n=", length(MDF$PID_OMRS), ")")) +theme(legend.position = c(5,5),legend.justification = c("left", "bottom")) + theme(panel.grid.minor = element_line(colour="grey", size=0.5)) + scale_x_continuous(minor_breaks = seq(0 , 100, 10), breaks = seq(0, 100, 10))+ scale_y_continuous(minor_breaks = seq(0 , 1, 0.1), breaks = seq(0, 1, 0.1))   



### e. Xpert Saved vs Score --------------------
base <- ggplot(CAD_Xpert_plot, aes(Score, XpertSaved)) + geom_path(aes(color = DeepLearningSystem))  + scale_fill_manual(values=c("#e41a1c", "#377eb8", "#4daf4a", "#984ea3", "#ff7f00")) +  scale_color_manual(values=c("#e41a1c", "#377eb8", "#4daf4a", "#984ea3", "#ff7f00")) 

XpertSavingCurveCAD4TB <- base + theme_light()  + labs(x = "e. Abnormality Score", y= "Xpert Saved", subtitle = paste0("d. CAD4TBv6 vs CAD4TBv7: The Xpert Saved vs abnormality score (n=", length(MDF$PID_OMRS), ")")) + theme(panel.grid.minor = element_line(colour="grey", size=0.5)) + scale_x_continuous(minor_breaks = seq(0 , 100, 10), breaks = seq(0, 100, 10))+ scale_y_continuous(minor_breaks = seq(0 , 1, 0.1), breaks = seq(0, 1, 0.1)) +theme(legend.position = c(0.05,0.95),  legend.justification = c("left", "top"))



### Save --------------------

tiff("2.0 Version Comparison/2 Curves.tif", width = 10, height = 10, units = "in", res = 100)
require(gridExtra)
grid.arrange(sensCurveqXR, XpertSavingCurveqXR, sensCurveCAD4TB, XpertSavingCurveCAD4TB,  nrow=2)
dev.off()

