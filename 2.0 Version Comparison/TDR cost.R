# Model the effect 
source("2.0 Version Comparison/Global.R")
source("2.0 Version Comparison/radiologist.R")



# CAD_Xpert_plot <- read.csv("2.0 Version Comparison/Cutoffs TABLE.csv")
# CAD_Xpert_plot <- read.csv("2.0 Version Comparison/CAD_Xpert.csv")

# names(MDF)
# write.csv(MDF[, c(1, 13, 28)], "2.0 Version Comparison/Q2.csv", row.names = FALSE)
# write.csv(MDF[, c(1, 13, 29)], "2.0 Version Comparison/Q3.csv", row.names = FALSE)
# write.csv(MDF[, c(1, 13, 26)], "2.0 Version Comparison/D6.csv", row.names = FALSE)
# write.csv(MDF[, c(1, 13, 27)], "2.0 Version Comparison/D7.csv", row.names = FALSE)


## for example, I declare the following values
pop_size <- 54125
prev <- 0.19
XpertCost <- 20


D6_Cost <- read_csv("2.0 Version Comparison/Results/D6_Cost.csv")
D7_Cost <- read_csv("2.0 Version Comparison/Results/D7_Cost.csv")
Q2_Cost <- read_csv("2.0 Version Comparison/Results/Q2_Cost.csv")
Q3_Cost <- read_csv("2.0 Version Comparison/Results/Q3_Cost.csv")

D6_Cost$AI <- "CAD4TBv6"
D7_Cost$AI <- "CAD4TBv7"
Q2_Cost$AI <- "qXRv2"
Q3_Cost$AI <- "qXRv3"

Cost <- rbind(D6_Cost, D7_Cost, Q2_Cost, Q3_Cost)
colnames(Cost) <- gsub(" ","",colnames(Cost))
rm(D6_Cost, D7_Cost, Q2_Cost, Q3_Cost)

Cost <- Cost[, -c(2:4, 9:15, 18)]

Cost$Sens <- as.numeric(sub("%","",Cost$Sens))/100
Cost$Spec <- as.numeric(sub("%","",Cost$Spec))/100
Cost$CADpos <- as.numeric(sub("%","",Cost$CADpos))/100
Cost$PPV <- as.numeric(sub("%","",Cost$PPV))/100
Cost$Totalcostforconfirmatorytest <- as.numeric(sub(",", "", sub("\\$","",Cost$Totalcostforconfirmatorytest)))
Cost$Costpercase <- as.numeric(sub("\\$","",Cost$Costpercase))

Cost$One <- 1
Cost$XpertSaved. <- (1-Cost$CADpos)
names(Cost)[1] <- "Score" 


Cost$XpertTest <- Cost$Totalcostforconfirmatorytest/XpertCost
Cost$Xpertsaved <- 1-Cost$XpertTest/54125




# qXR ----
Cost0 <- Cost
Cost <- Cost[Cost$AI %in% c("qXRv2", "qXRv3"), ]
Cost$AI <- as.character(Cost$AI)


### d. Sensitivity vs Score --------------------
base <- ggplot(Cost, aes(Score, Sens)) + geom_path(aes(color = AI)) + scale_fill_manual(values=c("#e41a1c", "#377eb8", "#4daf4a", "#984ea3", "#ff7f00")) +  scale_color_manual(values=c("#e41a1c", "#377eb8", "#4daf4a", "#984ea3", "#ff7f00")) 
# + geom_ribbon(aes(x = Score, ymin = Sens_L, ymax = Sens_H, color = AI, fill = AI), alpha= 0.2) 

sensCurveqXR <- base + theme_light() + labs(x = "Abnormality Score", y= "Sensitivity", subtitle = paste0("qXRv2 vs qXRv3: The Sensitivity vs abnormality score (n=", length(MDF$PID_OMRS), ")")) +theme(legend.position = c(5,5),legend.justification = c("left", "bottom")) + theme(panel.grid.minor = element_line(colour="grey", size=0.5)) + scale_x_continuous(minor_breaks = seq(0 , 100, 10), breaks = seq(0, 100, 10))+ scale_y_continuous(minor_breaks = seq(0 , 1, 0.1), breaks = seq(0, 1, 0.1)) + geom_vline(xintercept = c(50, 60, 70), linetype="dotted", color = "black", size=1)  



### e. Xpert Saved vs Score --------------------
base <- ggplot(Cost, aes(Score, XpertSaved.)) + geom_path(aes(color = AI))  + scale_fill_manual(values=c("#e41a1c", "#377eb8", "#4daf4a", "#984ea3", "#ff7f00")) +  scale_color_manual(values=c("#e41a1c", "#377eb8", "#4daf4a", "#984ea3", "#ff7f00")) 

XpertSavingCurveqXR <- base + theme_light()  + labs(x = "e. Abnormality Score", y= "Xpert Saved", subtitle = paste0("qXRv2 vs qXRv3: The Xpert Saved vs abnormality score (n=", length(MDF$PID_OMRS), ")")) + theme(panel.grid.minor = element_line(colour="grey", size=0.5)) + scale_x_continuous(minor_breaks = seq(0 , 100, 10), breaks = seq(0, 100, 10))+ scale_y_continuous(minor_breaks = seq(0 , 1, 0.1), breaks = seq(0, 1, 0.1))+ geom_vline(xintercept = c(50, 60, 70), linetype="dotted", color = "black", size=1) +theme(  legend.position = c(0.05,0.95),  legend.justification = c("left", "top"))



# CAD4TB ----
Cost <- Cost0
Cost <- Cost[Cost$AI %in% c("CAD4TBv6", "CAD4TBv7"), ]
Cost$AI <- as.character(Cost$AI)



### d. Sensitivity vs Score --------------------
base <- ggplot(Cost, aes(Score, Sens)) + geom_path(aes(color = AI)) + scale_fill_manual(values=c("#e41a1c", "#377eb8", "#4daf4a", "#984ea3", "#ff7f00")) +  scale_color_manual(values=c("#e41a1c", "#377eb8", "#4daf4a", "#984ea3", "#ff7f00")) 
# + geom_ribbon(aes(x = Score, ymin = Sens_L, ymax = Sens_H, color = AI, fill = AI), alpha= 0.2) 

sensCurveCAD4TB <- base + theme_light()  + labs(x = "Abnormality Score", y= "Sensitivity", subtitle = paste0("CAD4TBv6 vs CAD4TBv7: The Sensitivity vs abnormality score (n=", length(MDF$PID_OMRS), ")")) +theme(legend.position = c(5,5),legend.justification = c("left", "bottom")) + theme(panel.grid.minor = element_line(colour="grey", size=0.5)) + scale_x_continuous(minor_breaks = seq(0 , 100, 10), breaks = seq(0, 100, 10))+ scale_y_continuous(minor_breaks = seq(0 , 1, 0.1), breaks = seq(0, 1, 0.1)) + geom_vline(xintercept = c(50, 60, 70), linetype="dotted", color = "black", size=1)  



### e. Xpert Saved vs Score --------------------
base <- ggplot(Cost, aes(Score, XpertSaved.)) + geom_path(aes(color = AI))  + scale_fill_manual(values=c("#e41a1c", "#377eb8", "#4daf4a", "#984ea3", "#ff7f00")) +  scale_color_manual(values=c("#e41a1c", "#377eb8", "#4daf4a", "#984ea3", "#ff7f00")) 

XpertSavingCurveCAD4TB <- base + theme_light()  + labs(x = "e. Abnormality Score", y= "Xpert Saved", subtitle = paste0("CAD4TBv6 vs CAD4TBv7: The Xpert Saved vs abnormality score (n=", length(MDF$PID_OMRS), ")")) + theme(panel.grid.minor = element_line(colour="grey", size=0.5)) + scale_x_continuous(minor_breaks = seq(0 , 100, 10), breaks = seq(0, 100, 10))+ scale_y_continuous(minor_breaks = seq(0 , 1, 0.1), breaks = seq(0, 1, 0.1))+ geom_vline(xintercept = c(50, 60, 70), linetype="dotted", color = "black", size=1) +theme(legend.position = c(0.05,0.95),  legend.justification = c("left", "top"))



### Save --------------------

tiff("2.0 Version Comparison/2 Curves.tif", width = 10, height = 10, units = "in", res = 100)
require(gridExtra)
grid.arrange(sensCurveqXR, XpertSavingCurveqXR, sensCurveCAD4TB, XpertSavingCurveCAD4TB,  nrow=2)
dev.off()