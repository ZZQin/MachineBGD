source("2.0 Version Comparison/Global.R")
source("2.0 Version Comparison/radiologist.R")


## for example, I declare the following values
pop_size <- 54125
prev <- 0.19
XpertCAD_Xpert <- 20
# 
maxV<- 101
sep <- 1



### Set up a function
myfunction <- function(dataset, AI.system, car.cutoff){
  a <- sum(AI.system >= car.cutoff & dataset$Xpert2Outcome_num =="1")
  b <- sum(AI.system >= car.cutoff & dataset$Xpert2Outcome_num =="0")
  c <- sum(AI.system < car.cutoff & dataset$Xpert2Outcome_num =="1")
  d <- sum(AI.system < car.cutoff & dataset$Xpert2Outcome_num =="0")
  dat <- as.table(matrix(c(a,b,c,d), nrow = 2, byrow = TRUE))
  rval <- epi.tests(dat, conf.level = 0.95)
  sensitivity <- as.vector(round((rval$detail$se),8))
  specificity <- as.vector(round((rval$detail$sp),8))
  Score <- AI.score[i]
  
  CAD_Pos_TB <- a/(sum(dataset$Xpert2Outcome_num =="1"))
  CAD_Pos_Normal <- b/(sum(dataset$Xpert2Outcome_num =="0"))
  
  accuracy <- cbind(Score, sensitivity, specificity, CAD_Pos_TB, CAD_Pos_Normal)
  
  return(accuracy)
}

################ CAD4TBv7 #####################################
AI.score <- seq(0, 100, by = sep)
mylist <- NULL
mylist <- as.list(mylist)

# CAD4TBv7
for (i in 1 : maxV){
  cutoff.accuracy <- myfunction(MDF, MDF$CAD4TBv7, AI.score[i])
  mylist[[i]] <- list(cutoff.accuracy)
}
MDF.CAD4TBv7 <- data.frame(matrix(unlist(mylist), nrow=maxV, byrow=T))
MDF.CAD4TBv7$Subgroup  <- paste("MDF")
MDF.CAD4TBv7$AI <- paste("CAD4TBv7")



################ CAD4TBv6 #####################################
AI.score <- seq(0, 100, by = sep)
mylist <- NULL
mylist <- as.list(mylist)

# CAD4TBv6
for (i in 1 : maxV){
  cutoff.accuracy <- myfunction(MDF, MDF$CAD4TBv6, AI.score[i])
  mylist[[i]] <- list(cutoff.accuracy)
}
MDF.CAD4TBv6 <- data.frame(matrix(unlist(mylist), nrow=maxV, byrow=T))
MDF.CAD4TBv6$Subgroup  <- paste("MDF")
MDF.CAD4TBv6$AI <- paste("CAD4TBv6")


################ qXRv2 #####################################
AI.score <- seq(0, 100, by = sep)
mylist <- NULL
mylist <- as.list(mylist)

# qXRv2
for (i in 1 : maxV){
  cutoff.accuracy <- myfunction(MDF, MDF$qXRv2, AI.score[i])
  mylist[[i]] <- list(cutoff.accuracy)
}
MDF.qXRv2 <- data.frame(matrix(unlist(mylist), nrow=maxV, byrow=T))
MDF.qXRv2$Subgroup  <- paste("MDF")
MDF.qXRv2$AI <- paste("qXRv2")


################ qXRv3 #####################################
AI.score <- seq(0, 100, by = sep)
mylist <- NULL
mylist <- as.list(mylist)

# qXRv3
for (i in 1 : maxV){
  cutoff.accuracy <- myfunction(MDF, MDF$qXRv3, AI.score[i])
  mylist[[i]] <- list(cutoff.accuracy)
}
MDF.qXRv3 <- data.frame(matrix(unlist(mylist), nrow=maxV, byrow=T))
MDF.qXRv3$Subgroup  <- paste("MDF")
MDF.qXRv3$AI <- paste("qXRv3")


######### Merge DFs ######
CAD_Xpert <- rbind(MDF.CAD4TBv7, MDF.CAD4TBv6, MDF.qXRv2, MDF.qXRv3)


colnames(CAD_Xpert)[1] <- "Score"
colnames(CAD_Xpert)[2] <- "Sens"
colnames(CAD_Xpert)[3] <- "Sens_L"
colnames(CAD_Xpert)[4] <- "Sens_H"
colnames(CAD_Xpert)[5] <- "Spec"
colnames(CAD_Xpert)[6] <- "Spec_L"
colnames(CAD_Xpert)[7] <- "Spec_H"
colnames(CAD_Xpert)[8] <- "CAD_Pos_TB"
colnames(CAD_Xpert)[9] <- "CAD_Pos_Normal"

CAD_Xpert$ppv <- (CAD_Xpert$Sens * prev)/(CAD_Xpert$Sens * prev + (1-CAD_Xpert$Spec)*(1-prev))
CAD_Xpert$npv <- (CAD_Xpert$Spec * (1-prev))/((CAD_Xpert$Spec * (1-prev))+(1-CAD_Xpert$Sens)*prev)
CAD_Xpert$CAD_Pos <- (pop_size*prev*CAD_Xpert$CAD_Pos_TB + (pop_size-pop_size*prev)*CAD_Xpert$CAD_Pos_Normal)/pop_size
CAD_Xpert$XpertSaved <- 1-CAD_Xpert$CAD_Pos


CAD_Xpert$Comment <- ""
CAD_Xpert$Comment[which(abs(CAD_Xpert$Sens-0.9) == min(abs(CAD_Xpert$Sens-0.9)))] <- "90% sens"
CAD_Xpert$Comment[which(abs(CAD_Xpert$Spec-0.7) == min(abs(CAD_Xpert$Spec-0.7)))] <- "70% spec"
View(CAD_Xpert)
CAD_Xpert$Comment[which(abs(CAD_Xpert$XpertSaved-0.5) == min(abs(CAD_Xpert$XpertSaved-0.5)))] <- "1/2 Xpert Saved"
CAD_Xpert$Comment[which(abs(CAD_Xpert$XpertSaved-2/3) == min(abs(CAD_Xpert$XpertSaved-2/3)))] <- "2/3 Xpert Saved"
CAD_Xpert$Comment[which(abs(CAD_Xpert$XpertSaved-0.75) == min(abs(CAD_Xpert$XpertSaved-0.75)))] <- "3/4 Xpert Saved"
CAD_Xpert$X <- 1-CAD_Xpert$Spec


CAD_Xpert$nnt <- 1/CAD_Xpert$ppv
CAD_Xpert$AI <- as.character(CAD_Xpert$AI)

#################Save ###############
write.csv(CAD_Xpert, "2.0 Version Comparison/Modeling.csv", row.names = F)


# qXR ----
CAD_Xpert <- read_csv("2.0 Version Comparison/Modeling.csv")

CAD_Xpert0 <- CAD_Xpert
CAD_Xpert <- CAD_Xpert0[CAD_Xpert0$AI %in% c("qXRv2", "qXRv3"), ]
CAD_Xpert$AI <- as.character(CAD_Xpert$AI)


### d. Sensitivity vs Score --------------------
base <- ggplot(CAD_Xpert, aes(Score, Sens)) + geom_path(aes(color = AI)) + scale_fill_manual(values=c("#e41a1c", "#377eb8", "#4daf4a", "#984ea3", "#ff7f00")) +  scale_color_manual(values=c("#e41a1c", "#377eb8", "#4daf4a", "#984ea3", "#ff7f00")) +theme(legend.position = c(0.05,0.15),  legend.justification = c("left", "bottom"))

sensCurveqXR <- base + theme_light() + labs(x = "Abnormality Score", y= "Sensitivity", subtitle = paste0("qXRv2 vs qXRv3: The Sensitivity vs abnormality score (n=", length(MDF$PID_OMRS), ")")) +theme(legend.position = c(5,5),legend.justification = c("left", "bottom")) + theme(panel.grid.minor = element_line(colour="grey", size=0.5)) + scale_x_continuous(minor_breaks = seq(0 , 100, 10), breaks = seq(0, 100, 10))+ scale_y_continuous(minor_breaks = seq(0 , 1, 0.1), breaks = seq(0, 1, 0.1)) + geom_vline(xintercept = c(50, 60, 70), linetype="dotted", color = "black", size=1)  
sensCurveqXR


### e. Xpert Saved vs Score --------------------
base <- ggplot(CAD_Xpert, aes(Score, XpertSaved)) + geom_path(aes(color = AI))  + scale_fill_manual(values=c("#e41a1c", "#377eb8", "#4daf4a", "#984ea3", "#ff7f00")) +  scale_color_manual(values=c("#e41a1c", "#377eb8", "#4daf4a", "#984ea3", "#ff7f00")) 

XpertSavingCurveqXR <- base + theme_light()  + labs(x = "e. Abnormality Score", y= "Xpert Saved", subtitle = paste0("qXRv2 vs qXRv3: The Xpert Saved vs abnormality score (n=", length(MDF$PID_OMRS), ")")) + theme(panel.grid.minor = element_line(colour="grey", size=0.5)) + scale_x_continuous(minor_breaks = seq(0 , 100, 10), breaks = seq(0, 100, 10))+ scale_y_continuous(minor_breaks = seq(0 , 1, 0.1), breaks = seq(0, 1, 0.1))+ geom_vline(xintercept = c(50, 60, 70), linetype="dotted", color = "black", size=1) +theme(  legend.position = c(0.05,0.95),  legend.justification = c("left", "top"))
XpertSavingCurveqXR


# CAD4TB ----
CAD_Xpert <- CAD_Xpert0
CAD_Xpert <- CAD_Xpert[CAD_Xpert$AI %in% c("CAD4TBv6", "CAD4TBv7"), ]
CAD_Xpert$AI <- as.character(CAD_Xpert$AI)



### d. Sensitivity vs Score --------------------
base <- ggplot(CAD_Xpert, aes(Score, Sens)) + geom_path(aes(color = AI)) + scale_fill_manual(values=c("#e41a1c", "#377eb8", "#4daf4a", "#984ea3", "#ff7f00")) +  scale_color_manual(values=c("#e41a1c", "#377eb8", "#4daf4a", "#984ea3", "#ff7f00")) 
# + geom_ribbon(aes(x = Score, ymin = Sens_L, ymax = Sens_H, color = AI, fill = AI), alpha= 0.2) 

sensCurveCAD4TB <- base + theme_light()  + labs(x = "Abnormality Score", y= "Sensitivity", subtitle = paste0("CAD4TBv6 vs CAD4TBv7: The Sensitivity vs abnormality score (n=", length(MDF$PID_OMRS), ")")) +theme(legend.position = c(5,5),legend.justification = c("left", "bottom")) + theme(panel.grid.minor = element_line(colour="grey", size=0.5)) + scale_x_continuous(minor_breaks = seq(0 , 100, 10), breaks = seq(0, 100, 10))+ scale_y_continuous(minor_breaks = seq(0 , 1, 0.1), breaks = seq(0, 1, 0.1)) + geom_vline(xintercept = c(50, 60, 70), linetype="dotted", color = "black", size=1)  +theme(legend.position = c(0.05,0.15),  legend.justification = c("left", "bottom"))



### e. Xpert Saved vs Score --------------------
base <- ggplot(CAD_Xpert, aes(Score, XpertSaved)) + geom_path(aes(color = AI))  + scale_fill_manual(values=c("#e41a1c", "#377eb8", "#4daf4a", "#984ea3", "#ff7f00")) +  scale_color_manual(values=c("#e41a1c", "#377eb8", "#4daf4a", "#984ea3", "#ff7f00")) 

XpertSavingCurveCAD4TB <- base + theme_light()  + labs(x = "e. Abnormality Score", y= "Xpert Saved", subtitle = paste0("CAD4TBv6 vs CAD4TBv7: The Xpert Saved vs abnormality score (n=", length(MDF$PID_OMRS), ")")) + theme(panel.grid.minor = element_line(colour="grey", size=0.5)) + scale_x_continuous(minor_breaks = seq(0 , 100, 10), breaks = seq(0, 100, 10))+ scale_y_continuous(minor_breaks = seq(0 , 1, 0.1), breaks = seq(0, 1, 0.1))+ geom_vline(xintercept = c(50, 60, 70), linetype="dotted", color = "black", size=1) +theme(legend.position = c(0.05,0.95),  legend.justification = c("left", "top"))



### Save --------------------
tiff("2.0 Version Comparison/2 Curves.tif", width = 10, height = 10, units = "in", res = 100)
require(gridExtra)
grid.arrange(sensCurveqXR, XpertSavingCurveqXR, sensCurveCAD4TB, XpertSavingCurveCAD4TB,  nrow=2)
dev.off()
