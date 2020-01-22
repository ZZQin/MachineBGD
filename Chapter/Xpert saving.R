source("DataWrangling/GlobalOption.R")
CAD_Xpert_plot <- read.csv("Results/CAD_Xpert Cutoffs TABLE 4 digits.csv")
CAD_Xpert_plot <- CAD_Xpert_plot[, -1]

CAD_Xpert_plot <- CAD_Xpert_plot[!CAD_Xpert_plot$DeepLearningSystem %in% c("IF1", "JF2", "IF3"), ]
CAD_Xpert_plot$Score[CAD_Xpert_plot$DeepLearningSystem %in% "CAD4TB"] <- CAD_Xpert_plot$Score[CAD_Xpert_plot$DeepLearningSystem %in% "CAD4TB"]/100

### Xpert saving vs Score
base <- ggplot(CAD_Xpert_plot, aes(Score, X.XpertSaved)) + geom_path(aes(color = DeepLearningSystem)) 

XpertSavingCurve <- base + theme_light() + coord_equal() + labs(x = "Abnormality Score", y= "Xpert Saving", subtitle = paste0("The Xpert Saving of CAD4TB(v6), Infervision(v2), JF CXR-1, Lunit(4.9.0) and qXR(v2) using Xpert results as the reference (n=", length(MDF$PID_OMRS), ")")) 

### Sens vs Xpert saving
base <- ggplot(CAD_Xpert_plot, aes(Sens, X.XpertSaved)) + geom_path(aes(color = DeepLearningSystem)) 

XpertSens <- base + theme_light() + coord_equal() + labs(x = "Sensitivity", y= "Xpert Saving", subtitle = paste0("The Xpert Saving vs Sens of CAD4TB(v6), Infervision(v2), JF CXR-1, Lunit(4.9.0) and qXR(v2) using Xpert results as the reference (n=", length(MDF$PID_OMRS), ")")) 



tiff("Results/XpertSavingCurve.tif", width = 10, height = 7.9, units = "in", res = 200)
require(gridExtra)
grid.arrange(XpertSavingCurve, XpertSens, ncol=2)
dev.off()





