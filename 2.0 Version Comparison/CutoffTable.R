source("2.0 Version Comparison/Global.R")
source("2.0 Version Comparison/radiologist.R")
# maxV <- 10001
# sep <- 0.0001
# sepdeflt <- 0.01

maxV<- 101
sepdeflt <- 1

### Set up a function
myfunction <- function(CountryX, DL.System, car.cutoff){
  a <- sum(DL.System >= car.cutoff & CountryX$XPERT_pos =="1")
  b <- sum(DL.System >= car.cutoff & CountryX$XPERT_pos =="0")
  c <- sum(DL.System < car.cutoff & CountryX$XPERT_pos =="1")
  d <- sum(DL.System < car.cutoff & CountryX$XPERT_pos =="0")
  dat <- as.table(matrix(c(a,b,c,d), nrow = 2, byrow = TRUE))
  rval <- epi.tests(dat, conf.level = 0.95)
  sensitivity <- as.vector(round((rval$elements$sensitivity),8))
  specificity <- as.vector(round((rval$elements$specificity),8))
  pv.positive <- as.vector(round((rval$elements$pv.positive),8))
  pv.negative <- as.vector(round((rval$elements$pv.negative),8))
  correct.rate <- (a+d)/length(CountryX$XPERT_pos)
  XpertSaved<- 1-round(sum(DL.System >= car.cutoff)/length(CountryX$Age), 8)
  Score <- DL.score[i]
  accuracy <- cbind(sensitivity, specificity, pv.positive, pv.negative, XpertSaved, Score, correct.rate)
  return(accuracy)
}



names(MDF)[13] <- "XPERT_pos"



### qXRv2------------
DL.score <- seq(0, 100, by = sepdeflt)
mylist <- ""
# MDF_qXRv2
for (i in 1 : maxV){
  cutoff.accuracy <- myfunction(MDF, MDF$qXRv2, DL.score[i])
  mylist[[i]] <- list(cutoff.accuracy)
}
MDF.qXRv2 <- data.frame(matrix(unlist(mylist), nrow=maxV, byrow=T))
MDF.qXRv2$Country  <- paste("MDF")
MDF.qXRv2$DeepLearningSystem <- paste("qXRv2")
MDF.qXRv2$Comment <- ""
MDF.qXRv2$Comment[which(abs(MDF.qXRv2$X13-0.5) == min(abs(MDF.qXRv2$X13-0.5)))] <- "1/2 Xpert Saved"
MDF.qXRv2$Comment[which(abs(MDF.qXRv2$X13-2/3) == min(abs(MDF.qXRv2$X13-2/3)))] <- "2/3 Xpert Saved"
MDF.qXRv2$Comment[which(abs(MDF.qXRv2$X13-0.75) == min(abs(MDF.qXRv2$X13-0.75)))] <- "3/4 Xpert Saved"

MDF.qXRv2$Comment[which(abs(MDF.qXRv2$X1-Radiologist$Sens[1]) == min(abs(MDF.qXRv2$X1-Radiologist$Sens[1])))] <- paste("Radiologists' specificity = ", Radiologist$Specificity[1], sep = "")
MDF.qXRv2$Comment[which(abs(MDF.qXRv2$X1-Radiologist$Sens[2]) == min(abs(MDF.qXRv2$X1-Radiologist$Sens[2])))] <- paste("Radiologists' specificity = ", Radiologist$Specificity[2], sep = "")
MDF.qXRv2$Comment[which(abs(MDF.qXRv2$X1-Radiologist$Sens[3]) == min(abs(MDF.qXRv2$X1-Radiologist$Sens[3])))] <- paste("Radiologists' specificity = ", Radiologist$Specificity[3], sep = "")


### qXRv3------------
DL.score <- seq(0, 100, by = sepdeflt)
# MDF_qXRv3
for (i in 1 : maxV){
  cutoff.accuracy <- myfunction(MDF, MDF$qXRv3, DL.score[i])
  mylist[[i]] <- list(cutoff.accuracy)
}
MDF.qXRv3 <- data.frame(matrix(unlist(mylist), nrow=maxV, byrow=T))
MDF.qXRv3$Country  <- paste("MDF")
MDF.qXRv3$DeepLearningSystem <- paste("qXRv3")
MDF.qXRv3$Comment <- ""
MDF.qXRv3$Comment[which(abs(MDF.qXRv3$X13-0.5) == min(abs(MDF.qXRv3$X13-0.5)))] <- "1/2 Xpert Saved"
MDF.qXRv3$Comment[which(abs(MDF.qXRv3$X13-2/3) == min(abs(MDF.qXRv3$X13-2/3)))] <- "2/3 Xpert Saved"
MDF.qXRv3$Comment[which(abs(MDF.qXRv3$X13-0.75) == min(abs(MDF.qXRv3$X13-0.75)))] <- "3/4 Xpert Saved"

MDF.qXRv3$Comment[which(abs(MDF.qXRv3$X1-Radiologist$Sens[1]) == min(abs(MDF.qXRv3$X1-Radiologist$Sens[1])))] <- paste("Radiologists' specificity = ", Radiologist$Specificity[1], sep = "")
MDF.qXRv3$Comment[which(abs(MDF.qXRv3$X1-Radiologist$Sens[2]) == min(abs(MDF.qXRv3$X1-Radiologist$Sens[2])))] <- paste("Radiologists' specificity = ", Radiologist$Specificity[2], sep = "")
MDF.qXRv3$Comment[which(abs(MDF.qXRv3$X1-Radiologist$Sens[3]) == min(abs(MDF.qXRv3$X1-Radiologist$Sens[3])))] <- paste("Radiologists' specificity = ", Radiologist$Specificity[3], sep = "")


### CAD4TBv6------------
DL.score <- seq(0, 100, by = sepdeflt)
# MDF_CAD4TBv6
for (i in 1 : maxV){
  cutoff.accuracy <- myfunction(MDF, MDF$CAD4TBv6, DL.score[i])
  mylist[[i]] <- list(cutoff.accuracy)
}
MDF.CAD4TBv6 <- data.frame(matrix(unlist(mylist), nrow=maxV, byrow=T))
MDF.CAD4TBv6$Country  <- paste("MDF")
MDF.CAD4TBv6$DeepLearningSystem <- paste("CAD4TBv6")
MDF.CAD4TBv6$Comment <- ""
MDF.CAD4TBv6$Comment[which(abs(MDF.CAD4TBv6$X13-0.5) == min(abs(MDF.CAD4TBv6$X13-0.5)))] <- "1/2 Xpert Saved"
MDF.CAD4TBv6$Comment[which(abs(MDF.CAD4TBv6$X13-2/3) == min(abs(MDF.CAD4TBv6$X13-2/3)))] <- "2/3 Xpert Saved"
MDF.CAD4TBv6$Comment[which(abs(MDF.CAD4TBv6$X13-0.75) == min(abs(MDF.CAD4TBv6$X13-0.75)))] <- "3/4 Xpert Saved"

MDF.CAD4TBv6$Comment[which(abs(MDF.CAD4TBv6$X1-Radiologist$Sens[1]) == min(abs(MDF.CAD4TBv6$X1-Radiologist$Sens[1])))] <- paste("Radiologists' specificity = ", Radiologist$Specificity[1], sep = "")
MDF.CAD4TBv6$Comment[which(abs(MDF.CAD4TBv6$X1-Radiologist$Sens[2]) == min(abs(MDF.CAD4TBv6$X1-Radiologist$Sens[2])))] <- paste("Radiologists' specificity = ", Radiologist$Specificity[2], sep = "")
MDF.CAD4TBv6$Comment[which(abs(MDF.CAD4TBv6$X1-Radiologist$Sens[3]) == min(abs(MDF.CAD4TBv6$X1-Radiologist$Sens[3])))] <- paste("Radiologists' specificity = ", Radiologist$Specificity[3], sep = "")



### CAD4TBv7------------
DL.score <- seq(0, 100, by = sepdeflt)
# MDF_CAD4TBv7
for (i in 1 : maxV){
  cutoff.accuracy <- myfunction(MDF, MDF$CAD4TBv7, DL.score[i])
  mylist[[i]] <- list(cutoff.accuracy)
}
MDF.CAD4TBv7 <- data.frame(matrix(unlist(mylist), nrow=maxV, byrow=T))
MDF.CAD4TBv7$Country  <- paste("MDF")
MDF.CAD4TBv7$DeepLearningSystem <- paste("CAD4TBv7")
MDF.CAD4TBv7$Comment <- ""
MDF.CAD4TBv7$Comment[which(abs(MDF.CAD4TBv7$X13-0.5) == min(abs(MDF.CAD4TBv7$X13-0.5)))] <- "1/2 Xpert Saved"
MDF.CAD4TBv7$Comment[which(abs(MDF.CAD4TBv7$X13-2/3) == min(abs(MDF.CAD4TBv7$X13-2/3)))] <- "2/3 Xpert Saved"
MDF.CAD4TBv7$Comment[which(abs(MDF.CAD4TBv7$X13-0.75) == min(abs(MDF.CAD4TBv7$X13-0.75)))] <- "3/4 Xpert Saved"

MDF.CAD4TBv7$Comment[which(abs(MDF.CAD4TBv7$X1-Radiologist$Sens[1]) == min(abs(MDF.CAD4TBv7$X1-Radiologist$Sens[1])))] <- paste("Radiologists' specificity = ", Radiologist$Specificity[1], sep = "")
MDF.CAD4TBv7$Comment[which(abs(MDF.CAD4TBv7$X1-Radiologist$Sens[2]) == min(abs(MDF.CAD4TBv7$X1-Radiologist$Sens[2])))] <- paste("Radiologists' specificity = ", Radiologist$Specificity[2], sep = "")
MDF.CAD4TBv7$Comment[which(abs(MDF.CAD4TBv7$X1-Radiologist$Sens[3]) == min(abs(MDF.CAD4TBv7$X1-Radiologist$Sens[3])))] <- paste("Radiologists' specificity = ", Radiologist$Specificity[3], sep = "")

####
CAD_Xpert <- rbind(MDF.qXRv3, MDF.qXRv2, MDF.CAD4TBv6, MDF.CAD4TBv7)
rm(MDF.qXRv3, MDF.qXRv2, MDF.CAD4TBv6, MDF.CAD4TBv7)

######### Merge DFs ######
colnames(CAD_Xpert)[1] <- "Sens"
colnames(CAD_Xpert)[2] <- "Sens_L"
colnames(CAD_Xpert)[3] <- "Sens_H"
colnames(CAD_Xpert)[4] <- "Spec"
colnames(CAD_Xpert)[5] <- "Spec_L"
colnames(CAD_Xpert)[6] <- "Spec_H"
colnames(CAD_Xpert)[7] <- "ppv"
colnames(CAD_Xpert)[8] <- "PPV_L"
colnames(CAD_Xpert)[9] <- "PPV_H"
colnames(CAD_Xpert)[10] <- "npv"
colnames(CAD_Xpert)[11] <- "NPV_L"
colnames(CAD_Xpert)[12] <- "NPV_H"
colnames(CAD_Xpert)[13] <- "XpertSaved%"
colnames(CAD_Xpert)[14] <- "Score"
colnames(CAD_Xpert)[15] <- "accuracy"
colnames(CAD_Xpert)[16] <- "Site"
CAD_Xpert$X <- 1-CAD_Xpert$Spec


CAD_Xpert$Sen_95CI <- paste(percent(CAD_Xpert[, 2], suffix = ""), "-", percent(CAD_Xpert[, 3]), sep = "")
CAD_Xpert$Spe_95CI <- paste(percent(CAD_Xpert[, 5], suffix = ""), "-", percent(CAD_Xpert[, 6]), sep = "")
CAD_Xpert$PPV_95CI <- paste(percent(CAD_Xpert[, 8], suffix = ""), "-", percent(CAD_Xpert[, 9]), sep = "")
CAD_Xpert$NPV_95CI <- paste(percent(CAD_Xpert[, 11], suffix = ""), "-", percent(CAD_Xpert[, 12]), sep = "")
CAD_Xpert$NNT <- 1/CAD_Xpert$ppv
CAD_Xpert$NNT_H <- 1/CAD_Xpert$PPV_L
CAD_Xpert$NNT_L <- 1/CAD_Xpert$PPV_H


CAD_Xpert$Sensitivity <- paste(percent(CAD_Xpert$Sens, accuracy = 0.1), " (", CAD_Xpert$Sen_95CI, ")", sep = "")
CAD_Xpert$Specificity <- paste(percent(CAD_Xpert$Spec, accuracy = 0.1), " (", CAD_Xpert$Spe_95CI, ")", sep = "")
CAD_Xpert$PPV <- paste(percent(CAD_Xpert$ppv, accuracy = 0.1), " (", CAD_Xpert$PPV_95CI, ")", sep = "")
CAD_Xpert$NPV <- paste(percent(CAD_Xpert$npv, accuracy = 0.1), " (", CAD_Xpert$NPV_95CI, ")", sep = "")
CAD_Xpert$nnt <- paste(round(CAD_Xpert$NNT, 1), " (", round(CAD_Xpert$NNT_L, 1), "-", round(CAD_Xpert$NNT_H, 1), ")", sep = "")
# CAD_Xpert$`%XpertSaved%` <- round(CAD_Xpert$`%XpertSaved%`, 3)
CAD_Xpert$accuracy <- round(CAD_Xpert$accuracy, 3)


########### Combine with Modeling#####################
Modelling <- read_csv("2.0 Version Comparison/Modeling.csv")
CAD_Xpert <- read_csv( "2.0 Version Comparison/CAD_Xpert.csv")

Modelling$Index <- paste0(Modelling$AI, "_", Modelling$Score)
CAD_Xpert$Index <- paste0(CAD_Xpert$DeepLearningSystem, "_", CAD_Xpert$Score)
names(CAD_Xpert)
CAD_Xpert <- CAD_Xpert[, -c(7:13, 18, 22:26, 29:31)]
CAD_Xpert$PPV <- Modelling$ppv[match(Modelling$Index, CAD_Xpert$Index)]
CAD_Xpert$NPV <- Modelling$npv[match(Modelling$Index, CAD_Xpert$Index)]
CAD_Xpert$XpertSaved <- Modelling$XpertSaved[match(Modelling$Index, CAD_Xpert$Index)]
CAD_Xpert$nnt <- Modelling$nnt[match(Modelling$Index, CAD_Xpert$Index)]
CAD_Xpert$CAD_Pos <- Modelling$CAD_Pos[match(Modelling$Index, CAD_Xpert$Index)]


SuppTable <- CAD_Xpert %>%
  select(Site, DeepLearningSystem, Score, Sensitivity, Specificity, PPV, NPV,nnt, XpertSaved)
View(SuppTable)

write.csv(SuppTable, "2.0 Version Comparison/Supp Tab.csv", row.names = F)

CAD_Xpert_plot <- CAD_Xpert %>%
  select(Site, DeepLearningSystem, Score, Sensitivity, Specificity, PPV, NPV,nnt, XpertSaved, Sens, Sens_L, Sens_H, Spec, Spec_L, Spec_H, X)

write.csv(CAD_Xpert_plot, "2.0 Version Comparison/Cutoffs TABLE.csv", row.names = F)
write.csv(CAD_Xpert, "2.0 Version Comparison/CAD_Xpert1.csv", row.names = F)

