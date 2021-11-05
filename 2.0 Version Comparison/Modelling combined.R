Modeling_CAD4TB7 <- read_csv("09_Results/Reference both/Modelling/Modeling_CAD4TB7.csv")
Modeling_deeptek <- read_csv("09_Results/Reference both/Modelling/Modeling_deeptek.csv")
Modeling_epcon <- read_csv("09_Results/Reference both/Modelling/Modeling_epcon.csv")
Modeling_Infervision <- read_csv("09_Results/Reference both/Modelling/Modeling_Infervision.csv")
Modeling_JF2 <- read_csv("09_Results/Reference both/Modelling/Modeling_JF2.csv")
Modeling_lunit <- read_csv("09_Results/Reference both/Modelling/Modeling_lunit.csv")
Modeling_oxipit <- read_csv("09_Results/Reference both/Modelling/Modeling_oxipit.csv")
Modeling_qXR3 <- read_csv("09_Results/Reference both/Modelling/Modeling_qXR3.csv")
Modeling_Active <- read_csv("09_Results/Reference both/Modelling/Modeling_Xvision_Active.csv")
Modeling_Inactive <- read_csv("09_Results/Reference both/Modelling/Modeling_Xvision_Inactive.csv")



CAD_Xpert <- rbind(Modeling_CAD4TB7, Modeling_deeptek, Modeling_epcon, Modeling_Infervision, Modeling_JF2, Modeling_lunit, Modeling_oxipit, Modeling_qXR3, Modeling_Active, Modeling_Inactive)

CAD_Xpert[which(CAD_Xpert$AI == "JF2"),]$AI <- "JF CXR-2"
CAD_Xpert[which(CAD_Xpert$AI == "oxipit"),]$AI <- "Chest Eye"
CAD_Xpert[which(CAD_Xpert$AI == "epcon"),]$AI <- "XrayAME"
CAD_Xpert[which(CAD_Xpert$AI == "deeptek"),]$AI <- "Genki"
CAD_Xpert[which(CAD_Xpert$AI == "Infervision"),]$AI <- "InferRead DR Chest"
CAD_Xpert[which(CAD_Xpert$AI == "lunit"),]$AI <- "Lunit INSIGHT CXR"
CAD_Xpert[which(CAD_Xpert$AI == "CAD4TB7"),]$AI <- "CAD4TB"
CAD_Xpert[which(CAD_Xpert$AI == "qXR3"),]$AI <- "qXR"


#################Save ###############
write.csv(CAD_Xpert, "09_Results/Reference both/Modelling/Modeling_combined.csv", row.names = F)


rm(Modeling_CAD4TB7, Modeling_deeptek, Modeling_epcon, Modeling_Infervision, Modeling_JF2, Modeling_lunit, Modeling_oxipit, Modeling_qXR3)


pop_size <- 9000
prev <- 0.02


### PLOT ######
### a. Sensitivity vs Score --------------------
base <- ggplot(CAD_Xpert, aes(Score, Sens)) + geom_path(aes(color = AI))  
# + geom_ribbon(aes(x = Score, ymin = Sens_L, ymax = Sens_H, color = AI, fill = AI), alpha= 0.2) 

sensCurve <- base + theme_light() + coord_equal() + labs(x = "Abnormality Score", y= "Sensitivity", subtitle = paste0("a. The Sensitivity vs abnormality score (n=", pop_size, ")"))  + theme(legend.position = "none") + theme(panel.grid.minor = element_line(colour="grey", size=0.5)) + scale_x_continuous(minor_breaks = seq(0 , 1, 0.1), breaks = seq(0, 1, 0.1))+ scale_y_continuous(minor_breaks = seq(0 , 1, 0.1), breaks = seq(0, 1, 0.1)) + geom_vline(xintercept = c(0.5, 0.3, 0.7), linetype="dotted", color = "black", size=1)  +  geom_hline(yintercept = 0.9, linetype="dotted", color = "black", size=1)  +theme( legend.position = c(0.05,0.6),  legend.justification = c("left", "top"))

### d. NNT --------------------
CAD_Xpert$nnt <- 1/CAD_Xpert$ppv
base <- ggplot(CAD_Xpert, aes(Score, nnt)) + geom_path(aes(color = AI))  

NNTCurve <- base + theme_light()  + labs(x = "Abnormality Score", y= "NNT", subtitle = paste0("d. The NNT vs abnormality score (n=", pop_size, ")")) +theme(legend.position = c(1,1),legend.justification = c("right", "top")) + theme(panel.grid.minor = element_line(colour="grey", size=0.5)) + scale_x_continuous(minor_breaks = seq(0 , 1, 0.1), breaks = seq(0, 1, 0.1))+ geom_vline(xintercept = c(0.5, 0.7, 0.3), linetype="dotted", color = "black", size=1) 
NNTCurve
# +theme(legend.position = c(0.05,0.95), legend.justification = c("left", "top"))


### b. Xpert Saved vs Score --------------------
### Xpert saved ----
base <- ggplot(CAD_Xpert, aes(Score, XpertSaved)) + geom_path(aes(color = AI))  

XpertSavingCurve <- base + theme_light()  + labs(x = "e. Abnormality Score", y= "Xpert Saved",subtitle = paste0("b. The Xpert Saved (n=", pop_size, ")")) + theme(panel.grid.minor = element_line(colour="grey", size=0.5)) + scale_x_continuous(minor_breaks = seq(0 , 1, 0.10), breaks = seq(0, 1, 0.10))+ scale_y_continuous(minor_breaks = seq(0 , 1, 0.1), breaks = seq(0, 1, 0.1))+ geom_vline(xintercept = c(0.5, 0.6, 0.7), linetype="dotted", color = "black", size=1) +theme( legend.position = c(0.8,0.6),  legend.justification = c("left", "top"))


### c. Sens vs Xpert Saved--------------------
base <- ggplot(CAD_Xpert, aes(XpertSaved, Sens)) + geom_path(aes(color = AI, fill = AI))   
# + geom_ribbon(aes(x = XpertSaved, ymin = Sens_L, ymax = Sens_H, color = AI, fill = AI), alpha= 0.2) 

XpertSens <- base + theme_light() + coord_equal() + labs(x = "Xpert Saved", y= "Sensitivity", subtitle = paste0("c. The Xpert Saved vs sensitivity (n=", pop_size, ")")) +theme(legend.position = c(0.05,0.05), legend.justification = c("left", "bottom")) + theme(panel.grid.minor = element_line(colour="grey", size=0.5)) + scale_x_continuous(minor_breaks = seq(0 , 1, 0.1), breaks = seq(0, 1, 0.1))+ scale_y_continuous(minor_breaks = seq(0 , 1, 0.1), breaks = seq(0, 1, 0.1)) + geom_vline(xintercept = c(0.5, 0.66, 0.75), linetype="dotted", color =  "black", size=1) + geom_hline(yintercept = c(0.9, 0.7), linetype="dotted", color = "black", size=1)
XpertSens


### Save --------------------

tiff("09_Results/Reference both/Moduled Curves_all.tif", width = 10, height = 10, units = "in", res = 150)
require(gridExtra)
grid.arrange(sensCurve, XpertSavingCurve, XpertSens, NNTCurve,  nrow=2)
# grid.arrange(sensCurve, ggROC, XpertSavingCurve, PRC, NNTCurve, XpertSens, nrow=3)
dev.off()