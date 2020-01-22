### Sensitivity vs Score
base <- ggplot(SubgroupPlot, aes(Score, Sens)) + geom_path(aes(color = subgroup)) + 
  geom_ribbon(aes(x = Score, ymin = Sens_L, ymax = Sens_H, color = subgroup, fill = subgroup), alpha= 0.2) 

sensCurve <- base + theme_light() + coord_equal() + labs(x = "Abnormality Score", y= "Sensitivity", subtitle = paste0("The Sensitivity vs abnormality score of ", unique(SubgroupPlot$DeepLearningSystem, " in different subgroups"))) +theme(legend.position = c(0.05,0.05), legend.justification = c("left", "bottom"))



### PPV
base <- ggplot(SubgroupPlot, aes(Score, ppv)) + geom_path(aes(color = subgroup)) + 
  geom_ribbon(data = SubgroupPlot %>% filter(Score <0.95), aes(x = Score, ymin = PPV_L, ymax = PPV_H, color = subgroup, fill = subgroup), alpha= 0.2) 

ppvCurve <- base + theme_light() + coord_equal() + labs(x = "Abnormality Score", y= "PPV", subtitle = paste0("The PPV vs abnormality score of ", unique(SubgroupPlot$DeepLearningSystem, " in different subgroups"))) +theme(legend.position = c(0.05,0.95), legend.justification = c("left", "top"))


### Xpert Saved vs Score
base <- ggplot(SubgroupPlot, aes(Score, X.XpertSaved)) + geom_path(aes(color = subgroup)) 

XpertSavingCurve <- base + theme_light() + coord_equal() + labs(x = "Abnormality Score", y= "Xpert Saved", subtitle = paste0("The Xpert Saved vs abnormality score of ", unique(SubgroupPlot$DeepLearningSystem, " in different subgroups"))) +theme(legend.position = c(0.05,0.95), legend.justification = c("left", "top"))

### Sens vs Xpert Saved
base <- ggplot(SubgroupPlot, aes(X.XpertSaved, Sens)) + geom_path(aes(color = subgroup)) + 
  geom_ribbon(aes(x = X.XpertSaved, ymin = Sens_L, ymax = Sens_H, color = subgroup, fill = subgroup), alpha= 0.2) 

XpertSens <- base + theme_light() + coord_equal() + labs(y = "Sensitivity", x= "Xpert Saved", subtitle = paste0("The Xpert Saved vs sensitivity of ", unique(SubgroupPlot$DeepLearningSystem, " in different subgroups"))) +theme(legend.position = c(0.05,0.05), legend.justification = c("left", "bottom"))

### Specificity vs Score
base <- ggplot(SubgroupPlot, aes(Score, Spec)) + geom_path(aes(color = subgroup))  + 
  geom_ribbon(data = SubgroupPlot %>% filter(Score <0.95), aes(x = Score, ymin = Spec_L, ymax = Spec_H, color = subgroup, fill = subgroup), alpha= 0.2) 

SpecScore <- base + theme_light() + coord_equal() + labs(x = "Score", y= "Specificity", subtitle = paste0("The specificity vs abnormality score of ", unique(SubgroupPlot$DeepLearningSystem, " in different subgroups"))) +theme(legend.position = c(0.05,0.95), legend.justification = c("left", "top"))

### ROC
base <- ggplot(SubgroupPlot, aes(X, Sens)) + geom_path(aes(color = subgroup)) + geom_ribbon(aes(x = X, ymin = Sens_L, ymax = Sens_H, color = subgroup, fill = subgroup), alpha= 0.2) 

ROC <- base + theme_minimal() + geom_abline(slope=1, intercept = 0, linetype = "dashed", alpha=0.7, color = "grey") + coord_equal() +   labs(x = "1-Specificity", y= "Sensitivity", subtitle = paste0("The ROC curves of ", unique(SubgroupPlot$DeepLearningSystem, " in different subgroups")))  +theme(legend.position = c(0.95,0.05), legend.justification = c("right", "bottom"))

### PRC
base <- ggplot(SubgroupPlot, aes(Sens, ppv)) + geom_path(aes(color = subgroup)) + 
  geom_ribbon(data = SubgroupPlot %>% filter(Sens >0.02), aes(x = Sens, ymin = PPV_L, ymax = PPV_H, color = subgroup, fill = subgroup), alpha= 0.2) 

PRCurve <- base + theme_light() + coord_equal() + labs(x = "Sensitivity/Recall", y= "PPV/Precision", subtitle = paste0("The Precision Recall Curves of ", unique(SubgroupPlot$DeepLearningSystem, " in different subgroups"))) +theme(legend.position = "none")

### Save

tiff(paste0("Results/6 Curves of ", unique(SubgroupPlot$DeepLearningSystem, "in subgroups.tif")), width = 18, height = 12, units = "in", res = 100)
require(gridExtra)
grid.arrange(ROC, PRCurve, sensCurve, SpecScore, ppvCurve, XpertSens, ncol=3)

dev.off()

