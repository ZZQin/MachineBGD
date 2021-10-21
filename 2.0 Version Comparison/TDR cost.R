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

names(D6_Cost)

D6_Cost$AI <- "CAD4TBv6"
D7_Cost$AI <- "CAD4TBv7"
Q2_Cost$AI <- "qXRv2"
Q3_Cost$AI <- "qXRv3"

Cost <- rbind(D6_Cost, D7_Cost, Q2_Cost, Q3_Cost)
rm(D6_Cost, D7_Cost, Q2_Cost, Q3_Cost)
Cost <- Cost[, -c(2:4, 9:13)]
Cost$XpertSaved. <- (1-Cost$`CAD pos`)
names(Cost) 
names(Cost) <- c("Score", "Sens", "Spec", "CAD_pos", "PPV", )

### e. Xpert Saved vs Score --------------------
base <- ggplot(Cost, aes(Score, XpertSaved.)) + geom_path(aes(color = DeepLearningSystem))  + scale_fill_manual(values=c("#e41a1c", "#377eb8", "#4daf4a", "#984ea3", "#ff7f00")) +  scale_color_manual(values=c("#e41a1c", "#377eb8", "#4daf4a", "#984ea3", "#ff7f00")) 

XpertSavingCurveqXR <- base + theme_light()  + labs(x = "e. Abnormality Score", y= "Xpert Saved", subtitle = paste0("qXRv2 vs qXRv3: The Xpert Saved vs abnormality score (n=", length(MDF$PID_OMRS), ")")) + theme(panel.grid.minor = element_line(colour="grey", size=0.5)) + scale_x_continuous(minor_breaks = seq(0 , 100, 10), breaks = seq(0, 100, 10))+ scale_y_continuous(minor_breaks = seq(0 , 1, 0.1), breaks = seq(0, 1, 0.1))+ geom_vline(xintercept = c(80, 70), linetype="dotted", color = "black", size=1) +theme(  legend.position = c(0.05,0.95),  legend.justification = c("left", "top"))



