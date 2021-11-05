source("2.0 Version Comparison/Global.R")
source("2.0 Version Comparison/radiologist.R")


## for example, I declare the following values
pop_size <- 54125
prev <- 0.19
XpertCost <- 20
# 
maxV<- 101
sep <- 0.01
sepdeflt <- 1



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

################ MDF #####################################
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


######### Merge DFs ######
CAD_Xpert <- MDF.CAD4TBv7

# CAD_Xpert <- rbind(MDF.CAD4TBv7, Young.age.CAD4TBv7, Middle.age.CAD4TBv7, Old.age.CAD4TBv7, HIVp.CAD4TBv7, HIVn.CAD4TBv7, Prior.CAD4TBv7, New.CAD4TBv7, Female.CAD4TBv7, Male.CAD4TBv7, Smoker.CAD4TBv7, NonSmoker.CAD4TBv7, Symptomatic.CAD4TBv7, NonSymptomatic.CAD4TBv7)

colnames(CAD_Xpert)[1] <- "Score"
colnames(CAD_Xpert)[2] <- "Sens"
colnames(CAD_Xpert)[3] <- "Sens_L"
colnames(CAD_Xpert)[4] <- "Sens_H"
colnames(CAD_Xpert)[5] <- "Spec"
colnames(CAD_Xpert)[6] <- "Spec_L"
colnames(CAD_Xpert)[7] <- "Spec_H"
colnames(CAD_Xpert)[8] <- "CAD_Pos_TB"
colnames(CAD_Xpert)[9] <- "CAD_Pos_Normal"
View(CAD_Xpert)

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
write.csv(CAD_Xpert, "09_Results/Reference both/Modelling/Modeling_CAD4TBv7.csv", row.names = F)




### PLOT ######
### a. Sensitivity vs Score --------------------
base <- ggplot(CAD_Xpert, aes(Score, Sens)) + geom_path(aes(color = AI))  
# + geom_ribbon(aes(x = Score, ymin = Sens_L, ymax = Sens_H, color = AI, fill = AI), alpha= 0.2) 

sensCurve <- base + theme_light() + coord_equal() + labs(x = "Abnormality Score", y= "Sensitivity", subtitle = paste0("a. The Sensitivity vs abnormality score (n=", pop_size, ")"))  + theme(legend.position = "none") + theme(panel.grid.minor = element_line(colour="grey", size=0.5)) + scale_x_continuous(minor_breaks = seq(0 , 1, 0.1), breaks = seq(0, 1, 0.1))+ scale_y_continuous(minor_breaks = seq(0 , 1, 0.1), breaks = seq(0, 1, 0.1)) + geom_vline(xintercept = c(0.5, 0.3, 0.7), linetype="dotted", color = "black", size=1)  +  geom_hline(yintercept = 0.9, linetype="dotted", color = "black", size=1)  

### d. NNT --------------------
CAD_Xpert$nnt <- 1/CAD_Xpert$ppv
base <- ggplot(CAD_Xpert, aes(Score, nnt)) + geom_path(aes(color = AI))  

NNTCurve <- base + theme_light()  + labs(x = "Abnormality Score", y= "NNT", subtitle = paste0("d. The NNT vs abnormality score (n=", pop_size, ")")) +theme(legend.position = c(1,1),legend.justification = c("right", "top")) + theme(panel.grid.minor = element_line(colour="grey", size=0.5)) + scale_x_continuous(minor_breaks = seq(0 , 1, 0.1), breaks = seq(0, 1, 0.1))+ geom_vline(xintercept = c(0.5, 0.7, 0.3), linetype="dotted", color = "black", size=1) 
NNTCurve
# +theme(legend.position = c(0.05,0.95), legend.justification = c("left", "top"))


### b. Xpert Saved vs Score --------------------
### Xpert saved ----
base <- ggplot(CAD_Xpert, aes(Score, XpertSaved)) + geom_path(aes(color = AI))  

XpertSavingCurve <- base + theme_light()  + labs(x = "e. Abnormality Score", y= "Xpert Saved",subtitle = paste0("b. The Xpert Saved (n=", pop_size, ")")) + theme(panel.grid.minor = element_line(colour="grey", size=0.5)) + scale_x_continuous(minor_breaks = seq(0 , 1, 0.10), breaks = seq(0, 1, 0.10))+ scale_y_continuous(minor_breaks = seq(0 , 1, 0.1), breaks = seq(0, 1, 0.1))+ geom_vline(xintercept = c(0.5, 0.6, 0.7), linetype="dotted", color = "black", size=1) +theme(  legend.position = c(0.05,0.95),  legend.justification = c("left", "top"))


### c. Sens vs Xpert Saved--------------------
base <- ggplot(CAD_Xpert, aes(XpertSaved, Sens)) + geom_path(aes(color = AI, fill = AI))   
# + geom_ribbon(aes(x = XpertSaved, ymin = Sens_L, ymax = Sens_H, color = AI, fill = AI), alpha= 0.2) 

XpertSens <- base + theme_light() + coord_equal() + labs(x = "Xpert Saved", y= "Sensitivity", subtitle = paste0("c. The Xpert Saved vs sensitivity (n=", pop_size, ")")) +theme(legend.position = c(0.05,0.05), legend.justification = c("left", "bottom")) + theme(panel.grid.minor = element_line(colour="grey", size=0.5)) + scale_x_continuous(minor_breaks = seq(0 , 1, 0.1), breaks = seq(0, 1, 0.1))+ scale_y_continuous(minor_breaks = seq(0 , 1, 0.1), breaks = seq(0, 1, 0.1)) + geom_vline(xintercept = c(0.5, 0.66, 0.75), linetype="dotted", color =  "black", size=1) + geom_hline(yintercept = c(0.9, 0.7), linetype="dotted", color = "black", size=1)
XpertSens


### Save --------------------

tiff("09_Results/Reference both/Moduled Curves_CAD4TBv7.tif", width = 10, height = 10, units = "in", res = 150)
require(gridExtra)
grid.arrange(sensCurve, XpertSavingCurve, XpertSens, NNTCurve,  nrow=2)
# grid.arrange(sensCurve, ggROC, XpertSavingCurve, PRC, NNTCurve, XpertSens, nrow=3)
dev.off()
