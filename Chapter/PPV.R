source("DataWrangling/GlobalOption.R")

CAD_Xpert_plot <- read.csv("Results/CAD_Xpert Cutoffs TABLE 4 digits.csv")
CAD_Xpert_plot <- CAD_Xpert_plot[, -1]

CAD_Xpert_plot <- CAD_Xpert_plot[!CAD_Xpert_plot$DeepLearningSystem %in% c("IF1", "JF2", "IF3"), ]
CAD_Xpert_plot$Score[CAD_Xpert_plot$DeepLearningSystem %in% "CAD4TB"] <- CAD_Xpert_plot$Score[CAD_Xpert_plot$DeepLearningSystem %in% "CAD4TB"]/100

base <- ggplot(CAD_Xpert_plot, aes(Score, ppv)) + geom_path(aes(color = DeepLearningSystem)) + 
  geom_ribbon(data = CAD_Xpert_plot %>% filter(Score <0.90), aes(x = Score, ymin = PPV_L, ymax = PPV_H, color = DeepLearningSystem, fill = DeepLearningSystem), alpha= 0.2) 

ppvCurve <- base + theme_light() + coord_equal() + labs(x = "Abnormality Score", y= "PPV", subtitle = paste0("The PPV of CAD4TB(v6), Infervision(v2), JF CXR-1, Lunit(4.9.0) and qXR(v2) using Xpert results as the reference (n=", length(MDF$PID_OMRS), ")")) 
ppvCurve

tiff("Results/ppvCurve.tif", width = 10, height = 7.9, units = "in", res = 100)
ppvCurve
dev.off()

# scoreCurveZoomIn <- scoreCurve + coord_cartesian(xlim = c(0.5, 0.9), ylim = c(0.90, 1) ) +
# ggtitle("Figure 2b: Zoomed-in view of the square marked part of Fig. 2a")+theme_light()

