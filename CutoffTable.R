Nepal_Cameroon <- read.csv(file = "C:/Users/zhizh/OneDrive - Stop TB Partnership/UNOPS/10 Paper Writing/CAR software/03 Nepal_Cameroon/CAR -- 03 Nepal_Cameroon/Clean/Nepal_Cameroon.csv", header = T)
Nepal_Cameroon <- Nepal_Cameroon[, -1]
NPL <- Nepal_Cameroon[Nepal_Cameroon$Country %in% "NPL", ]
CAM <- Nepal_Cameroon[Nepal_Cameroon$Country %in% "CAM", ]

# maxV <- 10001
# sep <- 0.0001
# sepdeflt <- 0.01

maxV<- 101
sep <- 0.01
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

################ Nepal #####################################

### qXR, Lunit, Infervision, JF1 ------------
DL.score <- seq(0, 1, by = sep)
mylist <- NULL
mylist <- as.list(mylist)


# NPL_qXR3
for (i in 1 : maxV){
  cutoff.accuracy <- myfunction(NPL, NPL$qXR3, DL.score[i])
  mylist[[i]] <- list(cutoff.accuracy)
}
NPL.qXR3 <- data.frame(matrix(unlist(mylist), nrow=maxV, byrow=T))
NPL.qXR3$Country  <- paste("NPL")
NPL.qXR3$DeepLearningSystem <- paste("qXR")

NPL.qXR3$Comment <- ""
NPL.qXR3$Comment[which(abs(NPL.qXR3$X13-0.5) == min(abs(NPL.qXR3$X13-0.5)))] <- "1/2 Xpert Saved"
NPL.qXR3$Comment[which(abs(NPL.qXR3$X13-2/3) == min(abs(NPL.qXR3$X13-2/3)))] <- "2/3 Xpert Saved"
NPL.qXR3$Comment[which(abs(NPL.qXR3$X13-0.75) == min(abs(NPL.qXR3$X13-0.75)))] <- "3/4 Xpert Saved"

# NPL.qXR3$Comment[which(abs(NPL.qXR3$X1-Radiologist$Sens[1]) == min(abs(NPL.qXR3$X1-Radiologist$Sens[1])))] <- paste("Radiologists' specificity = ", Radiologist$Specificity[1], sep = "")
# NPL.qXR3$Comment[which(abs(NPL.qXR3$X1-Radiologist$Sens[2]) == min(abs(NPL.qXR3$X1-Radiologist$Sens[2])))] <- paste("Radiologists' specificity = ", Radiologist$Specificity[2], sep = "")
# NPL.qXR3$Comment[which(abs(NPL.qXR3$X1-Radiologist$Sens[3]) == min(abs(NPL.qXR3$X1-Radiologist$Sens[3])))] <- paste("Radiologists' specificity = ", Radiologist$Specificity[3], sep = "")




# NPL_Lunit
for (i in 1 : maxV){
  cutoff.accuracy <- myfunction(NPL, NPL$Lunit, DL.score[i])
  mylist[[i]] <- list(cutoff.accuracy)
}
NPL.Lunit <- data.frame(matrix(unlist(mylist), nrow=maxV, byrow=T))
NPL.Lunit$Country  <- paste("NPL")
NPL.Lunit$DeepLearningSystem <- paste("Lunit")
NPL.Lunit$Comment <- ""
NPL.Lunit$Comment[which(abs(NPL.Lunit$X13-0.5) == min(abs(NPL.Lunit$X13-0.5)))] <- "1/2 Xpert Saved"
NPL.Lunit$Comment[which(abs(NPL.Lunit$X13-2/3) == min(abs(NPL.Lunit$X13-2/3)))] <- "2/3 Xpert Saved"
NPL.Lunit$Comment[which(abs(NPL.Lunit$X13-0.75) == min(abs(NPL.Lunit$X13-0.75)))] <- "3/4 Xpert Saved"

# NPL.Lunit$Comment[which(abs(NPL.Lunit$X1-Radiologist$Sens[1]) == min(abs(NPL.Lunit$X1-Radiologist$Sens[1])))] <- paste("Radiologists' specificity = ", Radiologist$Specificity[1], sep = "")
# NPL.Lunit$Comment[which(abs(NPL.Lunit$X1-Radiologist$Sens[2]) == min(abs(NPL.Lunit$X1-Radiologist$Sens[2])))] <- paste("Radiologists' specificity = ", Radiologist$Specificity[2], sep = "")
# NPL.Lunit$Comment[which(abs(NPL.Lunit$X1-Radiologist$Sens[3]) == min(abs(NPL.Lunit$X1-Radiologist$Sens[3])))] <- paste("Radiologists' specificity = ", Radiologist$Specificity[3], sep = "")



# NPL_Infervision
for (i in 1 : maxV){
  cutoff.accuracy <- myfunction(NPL, NPL$Infervision, DL.score[i])
  mylist[[i]] <- list(cutoff.accuracy)
}
NPL.Infervision <- data.frame(matrix(unlist(mylist), nrow=maxV, byrow=T))
NPL.Infervision$Country  <- paste("NPL")
NPL.Infervision$DeepLearningSystem <- paste("Infervision")
NPL.Infervision$Comment <- ""
NPL.Infervision$Comment[which(abs(NPL.Infervision$X13-0.5) == min(abs(NPL.Infervision$X13-0.5)))] <- "1/2 Xpert Saved"
NPL.Infervision$Comment[which(abs(NPL.Infervision$X13-2/3) == min(abs(NPL.Infervision$X13-2/3)))] <- "2/3 Xpert Saved"
NPL.Infervision$Comment[which(abs(NPL.Infervision$X13-0.75) == min(abs(NPL.Infervision$X13-0.75)))] <- "3/4 Xpert Saved"

# NPL.Infervision$Comment[which(abs(NPL.Infervision$X1-Radiologist$Sens[1]) == min(abs(NPL.Infervision$X1-Radiologist$Sens[1])))] <- paste("Radiologists' specificity = ", Radiologist$Specificity[1], sep = "")
# NPL.Infervision$Comment[which(abs(NPL.Infervision$X1-Radiologist$Sens[2]) == min(abs(NPL.Infervision$X1-Radiologist$Sens[2])))] <- paste("Radiologists' specificity = ", Radiologist$Specificity[2], sep = "")
# NPL.Infervision$Comment[which(abs(NPL.Infervision$X1-Radiologist$Sens[3]) == min(abs(NPL.Infervision$X1-Radiologist$Sens[3])))] <- paste("Radiologists' specificity = ", Radiologist$Specificity[3], sep = "")

# NPL_JF1
for (i in 1 : maxV){
  cutoff.accuracy <- myfunction(NPL, NPL$JF1, DL.score[i])
  mylist[[i]] <- list(cutoff.accuracy)
}
NPL.JF1 <- data.frame(matrix(unlist(mylist), nrow=maxV, byrow=T))
NPL.JF1$Country  <- paste("NPL")
NPL.JF1$DeepLearningSystem <- paste("JF1")
NPL.JF1$Comment <- ""
NPL.JF1$Comment[which(abs(NPL.JF1$X13-0.5) == min(abs(NPL.JF1$X13-0.5)))] <- "1/2 Xpert Saved"
NPL.JF1$Comment[which(abs(NPL.JF1$X13-2/3) == min(abs(NPL.JF1$X13-2/3)))] <- "2/3 Xpert Saved"
NPL.JF1$Comment[which(abs(NPL.JF1$X13-0.75) == min(abs(NPL.JF1$X13-0.75)))] <- "3/4 Xpert Saved"

# NPL.JF1$Comment[which(abs(NPL.JF1$X1-Radiologist$Sens[1]) == min(abs(NPL.JF1$X1-Radiologist$Sens[1])))] <- paste("Radiologists' specificity = ", Radiologist$Specificity[1], sep = "")
# NPL.JF1$Comment[which(abs(NPL.JF1$X1-Radiologist$Sens[2]) == min(abs(NPL.JF1$X1-Radiologist$Sens[2])))] <- paste("Radiologists' specificity = ", Radiologist$Specificity[2], sep = "")
# NPL.JF1$Comment[which(abs(NPL.JF1$X1-Radiologist$Sens[3]) == min(abs(NPL.JF1$X1-Radiologist$Sens[3])))] <- paste("Radiologists' specificity = ", Radiologist$Specificity[3], sep = "")

### CAD4TB------------
DL.score <- seq(0, 100, by = sepdeflt)
# NPL_CAD4TB6
for (i in 1 : maxV){
  cutoff.accuracy <- myfunction(NPL, NPL$CAD4TB6, DL.score[i])
  mylist[[i]] <- list(cutoff.accuracy)
}
NPL.CAD4TB6 <- data.frame(matrix(unlist(mylist), nrow=maxV, byrow=T))
NPL.CAD4TB6$Country  <- paste("NPL")
NPL.CAD4TB6$DeepLearningSystem <- paste("CAD4TB")
NPL.CAD4TB6$Comment <- ""
NPL.CAD4TB6$Comment[which(abs(NPL.CAD4TB6$X13-0.5) == min(abs(NPL.CAD4TB6$X13-0.5)))] <- "1/2 Xpert Saved"
NPL.CAD4TB6$Comment[which(abs(NPL.CAD4TB6$X13-2/3) == min(abs(NPL.CAD4TB6$X13-2/3)))] <- "2/3 Xpert Saved"
NPL.CAD4TB6$Comment[which(abs(NPL.CAD4TB6$X13-0.75) == min(abs(NPL.CAD4TB6$X13-0.75)))] <- "3/4 Xpert Saved"

# NPL.CAD4TB6$Comment[which(abs(NPL.CAD4TB6$X1-Radiologist$Sens[1]) == min(abs(NPL.CAD4TB6$X1-Radiologist$Sens[1])))] <- paste("Radiologists' specificity = ", Radiologist$Specificity[1], sep = "")
# NPL.CAD4TB6$Comment[which(abs(NPL.CAD4TB6$X1-Radiologist$Sens[2]) == min(abs(NPL.CAD4TB6$X1-Radiologist$Sens[2])))] <- paste("Radiologists' specificity = ", Radiologist$Specificity[2], sep = "")
# NPL.CAD4TB6$Comment[which(abs(NPL.CAD4TB6$X1-Radiologist$Sens[3]) == min(abs(NPL.CAD4TB6$X1-Radiologist$Sens[3])))] <- paste("Radiologists' specificity = ", Radiologist$Specificity[3], sep = "")

NPL.DF <- rbind(NPL.qXR3, NPL.Lunit, NPL.Infervision, NPL.JF1, NPL.CAD4TB6)
rm(NPL.qXR3, NPL.Lunit, NPL.Infervision, NPL.JF1, NPL.CAD4TB6)


################ Cameroon #####################################

### qXR, Lunit, Infervision, JF1 ------------
DL.score <- seq(0, 1, by = sep)
mylist <- NULL
mylist <- as.list(mylist)


# CAM_qXR3
for (i in 1 : maxV){
  cutoff.accuracy <- myfunction(CAM, CAM$qXR3, DL.score[i])
  mylist[[i]] <- list(cutoff.accuracy)
}


CAM.qXR3 <- data.frame(matrix(unlist(mylist), nrow=maxV, byrow=T))
CAM.qXR3$Country  <- paste("CAM")
CAM.qXR3$DeepLearningSystem <- paste("qXR")

CAM.qXR3$Comment <- ""
CAM.qXR3$Comment[which(abs(CAM.qXR3$X13-0.5) == min(abs(CAM.qXR3$X13-0.5)))] <- "1/2 Xpert Saved"
CAM.qXR3$Comment[which(abs(CAM.qXR3$X13-2/3) == min(abs(CAM.qXR3$X13-2/3)))] <- "2/3 Xpert Saved"
CAM.qXR3$Comment[which(abs(CAM.qXR3$X13-0.75) == min(abs(CAM.qXR3$X13-0.75)))] <- "3/4 Xpert Saved"

# CAM.qXR3$Comment[which(abs(CAM.qXR3$X1-Radiologist$Sens[1]) == min(abs(CAM.qXR3$X1-Radiologist$Sens[1])))] <- paste("Radiologists' specificity = ", Radiologist$Specificity[1], sep = "")
# CAM.qXR3$Comment[which(abs(CAM.qXR3$X1-Radiologist$Sens[2]) == min(abs(CAM.qXR3$X1-Radiologist$Sens[2])))] <- paste("Radiologists' specificity = ", Radiologist$Specificity[2], sep = "")
# CAM.qXR3$Comment[which(abs(CAM.qXR3$X1-Radiologist$Sens[3]) == min(abs(CAM.qXR3$X1-Radiologist$Sens[3])))] <- paste("Radiologists' specificity = ", Radiologist$Specificity[3], sep = "")




# CAM_Lunit
for (i in 1 : maxV){
  cutoff.accuracy <- myfunction(CAM, CAM$Lunit, DL.score[i])
  mylist[[i]] <- list(cutoff.accuracy)
}
CAM.Lunit <- data.frame(matrix(unlist(mylist), nrow=maxV, byrow=T))
CAM.Lunit$Country  <- paste("CAM")
CAM.Lunit$DeepLearningSystem <- paste("Lunit")
CAM.Lunit$Comment <- ""
CAM.Lunit$Comment[which(abs(CAM.Lunit$X13-0.5) == min(abs(CAM.Lunit$X13-0.5)))] <- "1/2 Xpert Saved"
CAM.Lunit$Comment[which(abs(CAM.Lunit$X13-2/3) == min(abs(CAM.Lunit$X13-2/3)))] <- "2/3 Xpert Saved"
CAM.Lunit$Comment[which(abs(CAM.Lunit$X13-0.75) == min(abs(CAM.Lunit$X13-0.75)))] <- "3/4 Xpert Saved"

# CAM.Lunit$Comment[which(abs(CAM.Lunit$X1-Radiologist$Sens[1]) == min(abs(CAM.Lunit$X1-Radiologist$Sens[1])))] <- paste("Radiologists' specificity = ", Radiologist$Specificity[1], sep = "")
# CAM.Lunit$Comment[which(abs(CAM.Lunit$X1-Radiologist$Sens[2]) == min(abs(CAM.Lunit$X1-Radiologist$Sens[2])))] <- paste("Radiologists' specificity = ", Radiologist$Specificity[2], sep = "")
# CAM.Lunit$Comment[which(abs(CAM.Lunit$X1-Radiologist$Sens[3]) == min(abs(CAM.Lunit$X1-Radiologist$Sens[3])))] <- paste("Radiologists' specificity = ", Radiologist$Specificity[3], sep = "")



# CAM_Infervision
for (i in 1 : maxV){
  cutoff.accuracy <- myfunction(CAM, CAM$Infervision, DL.score[i])
  mylist[[i]] <- list(cutoff.accuracy)
}
CAM.Infervision <- data.frame(matrix(unlist(mylist), nrow=maxV, byrow=T))
CAM.Infervision$Country  <- paste("CAM")
CAM.Infervision$DeepLearningSystem <- paste("Infervision")
CAM.Infervision$Comment <- ""
CAM.Infervision$Comment[which(abs(CAM.Infervision$X13-0.5) == min(abs(CAM.Infervision$X13-0.5)))] <- "1/2 Xpert Saved"
CAM.Infervision$Comment[which(abs(CAM.Infervision$X13-2/3) == min(abs(CAM.Infervision$X13-2/3)))] <- "2/3 Xpert Saved"
CAM.Infervision$Comment[which(abs(CAM.Infervision$X13-0.75) == min(abs(CAM.Infervision$X13-0.75)))] <- "3/4 Xpert Saved"

# CAM.Infervision$Comment[which(abs(CAM.Infervision$X1-Radiologist$Sens[1]) == min(abs(CAM.Infervision$X1-Radiologist$Sens[1])))] <- paste("Radiologists' specificity = ", Radiologist$Specificity[1], sep = "")
# CAM.Infervision$Comment[which(abs(CAM.Infervision$X1-Radiologist$Sens[2]) == min(abs(CAM.Infervision$X1-Radiologist$Sens[2])))] <- paste("Radiologists' specificity = ", Radiologist$Specificity[2], sep = "")
# CAM.Infervision$Comment[which(abs(CAM.Infervision$X1-Radiologist$Sens[3]) == min(abs(CAM.Infervision$X1-Radiologist$Sens[3])))] <- paste("Radiologists' specificity = ", Radiologist$Specificity[3], sep = "")

# CAM_JF1
for (i in 1 : maxV){
  cutoff.accuracy <- myfunction(CAM, CAM$JF1, DL.score[i])
  mylist[[i]] <- list(cutoff.accuracy)
}
CAM.JF1 <- data.frame(matrix(unlist(mylist), nrow=maxV, byrow=T))
CAM.JF1$Country  <- paste("CAM")
CAM.JF1$DeepLearningSystem <- paste("JF1")
CAM.JF1$Comment <- ""
CAM.JF1$Comment[which(abs(CAM.JF1$X13-0.5) == min(abs(CAM.JF1$X13-0.5)))] <- "1/2 Xpert Saved"
CAM.JF1$Comment[which(abs(CAM.JF1$X13-2/3) == min(abs(CAM.JF1$X13-2/3)))] <- "2/3 Xpert Saved"
CAM.JF1$Comment[which(abs(CAM.JF1$X13-0.75) == min(abs(CAM.JF1$X13-0.75)))] <- "3/4 Xpert Saved"

# CAM.JF1$Comment[which(abs(CAM.JF1$X1-Radiologist$Sens[1]) == min(abs(CAM.JF1$X1-Radiologist$Sens[1])))] <- paste("Radiologists' specificity = ", Radiologist$Specificity[1], sep = "")
# CAM.JF1$Comment[which(abs(CAM.JF1$X1-Radiologist$Sens[2]) == min(abs(CAM.JF1$X1-Radiologist$Sens[2])))] <- paste("Radiologists' specificity = ", Radiologist$Specificity[2], sep = "")
# CAM.JF1$Comment[which(abs(CAM.JF1$X1-Radiologist$Sens[3]) == min(abs(CAM.JF1$X1-Radiologist$Sens[3])))] <- paste("Radiologists' specificity = ", Radiologist$Specificity[3], sep = "")

### CAD4TB ------------
DL.score <- seq(0, 100, by = sepdeflt)
# CAM_CAD4TB6
for (i in 1 : maxV){
  cutoff.accuracy <- myfunction(CAM, CAM$CAD4TB6, DL.score[i])
  mylist[[i]] <- list(cutoff.accuracy)
}
CAM.CAD4TB6 <- data.frame(matrix(unlist(mylist), nrow=maxV, byrow=T))
CAM.CAD4TB6$Country  <- paste("CAM")
CAM.CAD4TB6$DeepLearningSystem <- paste("CAD4TB")
CAM.CAD4TB6$Comment <- ""
CAM.CAD4TB6$Comment[which(abs(CAM.CAD4TB6$X13-0.5) == min(abs(CAM.CAD4TB6$X13-0.5)))] <- "1/2 Xpert Saved"
CAM.CAD4TB6$Comment[which(abs(CAM.CAD4TB6$X13-2/3) == min(abs(CAM.CAD4TB6$X13-2/3)))] <- "2/3 Xpert Saved"
CAM.CAD4TB6$Comment[which(abs(CAM.CAD4TB6$X13-0.75) == min(abs(CAM.CAD4TB6$X13-0.75)))] <- "3/4 Xpert Saved"

# CAM.CAD4TB6$Comment[which(abs(CAM.CAD4TB6$X1-Radiologist$Sens[1]) == min(abs(CAM.CAD4TB6$X1-Radiologist$Sens[1])))] <- paste("Radiologists' specificity = ", Radiologist$Specificity[1], sep = "")
# CAM.CAD4TB6$Comment[which(abs(CAM.CAD4TB6$X1-Radiologist$Sens[2]) == min(abs(CAM.CAD4TB6$X1-Radiologist$Sens[2])))] <- paste("Radiologists' specificity = ", Radiologist$Specificity[2], sep = "")
# CAM.CAD4TB6$Comment[which(abs(CAM.CAD4TB6$X1-Radiologist$Sens[3]) == min(abs(CAM.CAD4TB6$X1-Radiologist$Sens[3])))] <- paste("Radiologists' specificity = ", Radiologist$Specificity[3], sep = "")

CAM.DF <- rbind(CAM.qXR3, CAM.Lunit, CAM.Infervision, CAM.JF1, CAM.CAD4TB6)
rm(CAM.qXR3, CAM.Lunit, CAM.Infervision, CAM.JF1, CAM.CAD4TB6)

################ Bangladesh #####################################
source(file = "DataWrangling/GlobalOption.R")
source("radiologist.R")
BGD <- MDF[, ]
names(BGD)[26] <- "CAD4TB6"
names(BGD)[27] <- "qXR3"
names(BGD)[28] <- "Lunit"
names(BGD)[29]<- "JF1"
names(BGD)[32] <- "Infervision"
names(BGD)[13] <- "XPERT_pos"


### qXR, Lunit, Infervision, JF1 ------------
DL.score <- seq(0, 1, by = sep)
mylist <- NULL
mylist <- as.list(mylist)


# BGD_qXR3
for (i in 1 : maxV){
  cutoff.accuracy <- myfunction(BGD, BGD$qXR3, DL.score[i])
  mylist[[i]] <- list(cutoff.accuracy)
}


BGD.qXR3 <- data.frame(matrix(unlist(mylist), nrow=maxV, byrow=T))
BGD.qXR3$Country  <- paste("BGD")
BGD.qXR3$DeepLearningSystem <- paste("qXR")

BGD.qXR3$Comment <- ""
BGD.qXR3$Comment[which(abs(BGD.qXR3$X13-0.5) == min(abs(BGD.qXR3$X13-0.5)))] <- "1/2 Xpert Saved"
BGD.qXR3$Comment[which(abs(BGD.qXR3$X13-2/3) == min(abs(BGD.qXR3$X13-2/3)))] <- "2/3 Xpert Saved"
BGD.qXR3$Comment[which(abs(BGD.qXR3$X13-0.75) == min(abs(BGD.qXR3$X13-0.75)))] <- "3/4 Xpert Saved"

BGD.qXR3$Comment[which(abs(BGD.qXR3$X1-Radiologist$Sens[1]) == min(abs(BGD.qXR3$X1-Radiologist$Sens[1])))] <- paste("Radiologists' specificity = ", Radiologist$Specificity[1], sep = "")
BGD.qXR3$Comment[which(abs(BGD.qXR3$X1-Radiologist$Sens[2]) == min(abs(BGD.qXR3$X1-Radiologist$Sens[2])))] <- paste("Radiologists' specificity = ", Radiologist$Specificity[2], sep = "")
BGD.qXR3$Comment[which(abs(BGD.qXR3$X1-Radiologist$Sens[3]) == min(abs(BGD.qXR3$X1-Radiologist$Sens[3])))] <- paste("Radiologists' specificity = ", Radiologist$Specificity[3], sep = "")




# BGD_Lunit
for (i in 1 : maxV){
  cutoff.accuracy <- myfunction(BGD, BGD$Lunit, DL.score[i])
  mylist[[i]] <- list(cutoff.accuracy)
}
BGD.Lunit <- data.frame(matrix(unlist(mylist), nrow=maxV, byrow=T))
BGD.Lunit$Country  <- paste("BGD")
BGD.Lunit$DeepLearningSystem <- paste("Lunit")
BGD.Lunit$Comment <- ""
BGD.Lunit$Comment[which(abs(BGD.Lunit$X13-0.5) == min(abs(BGD.Lunit$X13-0.5)))] <- "1/2 Xpert Saved"
BGD.Lunit$Comment[which(abs(BGD.Lunit$X13-2/3) == min(abs(BGD.Lunit$X13-2/3)))] <- "2/3 Xpert Saved"
BGD.Lunit$Comment[which(abs(BGD.Lunit$X13-0.75) == min(abs(BGD.Lunit$X13-0.75)))] <- "3/4 Xpert Saved"

BGD.Lunit$Comment[which(abs(BGD.Lunit$X1-Radiologist$Sens[1]) == min(abs(BGD.Lunit$X1-Radiologist$Sens[1])))] <- paste("Radiologists' specificity = ", Radiologist$Specificity[1], sep = "")
BGD.Lunit$Comment[which(abs(BGD.Lunit$X1-Radiologist$Sens[2]) == min(abs(BGD.Lunit$X1-Radiologist$Sens[2])))] <- paste("Radiologists' specificity = ", Radiologist$Specificity[2], sep = "")
BGD.Lunit$Comment[which(abs(BGD.Lunit$X1-Radiologist$Sens[3]) == min(abs(BGD.Lunit$X1-Radiologist$Sens[3])))] <- paste("Radiologists' specificity = ", Radiologist$Specificity[3], sep = "")



# BGD_Infervision
for (i in 1 : maxV){
  cutoff.accuracy <- myfunction(BGD, BGD$Infervision, DL.score[i])
  mylist[[i]] <- list(cutoff.accuracy)
}
BGD.Infervision <- data.frame(matrix(unlist(mylist), nrow=maxV, byrow=T))
BGD.Infervision$Country  <- paste("BGD")
BGD.Infervision$DeepLearningSystem <- paste("Infervision")
BGD.Infervision$Comment <- ""
BGD.Infervision$Comment[which(abs(BGD.Infervision$X13-0.5) == min(abs(BGD.Infervision$X13-0.5)))] <- "1/2 Xpert Saved"
BGD.Infervision$Comment[which(abs(BGD.Infervision$X13-2/3) == min(abs(BGD.Infervision$X13-2/3)))] <- "2/3 Xpert Saved"
BGD.Infervision$Comment[which(abs(BGD.Infervision$X13-0.75) == min(abs(BGD.Infervision$X13-0.75)))] <- "3/4 Xpert Saved"

BGD.Infervision$Comment[which(abs(BGD.Infervision$X1-Radiologist$Sens[1]) == min(abs(BGD.Infervision$X1-Radiologist$Sens[1])))] <- paste("Radiologists' specificity = ", Radiologist$Specificity[1], sep = "")
BGD.Infervision$Comment[which(abs(BGD.Infervision$X1-Radiologist$Sens[2]) == min(abs(BGD.Infervision$X1-Radiologist$Sens[2])))] <- paste("Radiologists' specificity = ", Radiologist$Specificity[2], sep = "")
BGD.Infervision$Comment[which(abs(BGD.Infervision$X1-Radiologist$Sens[3]) == min(abs(BGD.Infervision$X1-Radiologist$Sens[3])))] <- paste("Radiologists' specificity = ", Radiologist$Specificity[3], sep = "")

# BGD_JF1
for (i in 1 : maxV){
  cutoff.accuracy <- myfunction(BGD, BGD$JF1, DL.score[i])
  mylist[[i]] <- list(cutoff.accuracy)
}
BGD.JF1 <- data.frame(matrix(unlist(mylist), nrow=maxV, byrow=T))
BGD.JF1$Country  <- paste("BGD")
BGD.JF1$DeepLearningSystem <- paste("JF1")
BGD.JF1$Comment <- ""
BGD.JF1$Comment[which(abs(BGD.JF1$X13-0.5) == min(abs(BGD.JF1$X13-0.5)))] <- "1/2 Xpert Saved"
BGD.JF1$Comment[which(abs(BGD.JF1$X13-2/3) == min(abs(BGD.JF1$X13-2/3)))] <- "2/3 Xpert Saved"
BGD.JF1$Comment[which(abs(BGD.JF1$X13-0.75) == min(abs(BGD.JF1$X13-0.75)))] <- "3/4 Xpert Saved"

BGD.JF1$Comment[which(abs(BGD.JF1$X1-Radiologist$Sens[1]) == min(abs(BGD.JF1$X1-Radiologist$Sens[1])))] <- paste("Radiologists' specificity = ", Radiologist$Specificity[1], sep = "")
BGD.JF1$Comment[which(abs(BGD.JF1$X1-Radiologist$Sens[2]) == min(abs(BGD.JF1$X1-Radiologist$Sens[2])))] <- paste("Radiologists' specificity = ", Radiologist$Specificity[2], sep = "")
BGD.JF1$Comment[which(abs(BGD.JF1$X1-Radiologist$Sens[3]) == min(abs(BGD.JF1$X1-Radiologist$Sens[3])))] <- paste("Radiologists' specificity = ", Radiologist$Specificity[3], sep = "")

### CAD4TB ------------
DL.score <- seq(0, 100, by = sepdeflt)
# BGD_CAD4TB6
for (i in 1 : maxV){
  cutoff.accuracy <- myfunction(BGD, BGD$CAD4TB6, DL.score[i])
  mylist[[i]] <- list(cutoff.accuracy)
}
BGD.CAD4TB6 <- data.frame(matrix(unlist(mylist), nrow=maxV, byrow=T))
BGD.CAD4TB6$Country  <- paste("BGD")
BGD.CAD4TB6$DeepLearningSystem <- paste("CAD4TB")
BGD.CAD4TB6$Comment <- ""
BGD.CAD4TB6$Comment[which(abs(BGD.CAD4TB6$X13-0.5) == min(abs(BGD.CAD4TB6$X13-0.5)))] <- "1/2 Xpert Saved"
BGD.CAD4TB6$Comment[which(abs(BGD.CAD4TB6$X13-2/3) == min(abs(BGD.CAD4TB6$X13-2/3)))] <- "2/3 Xpert Saved"
BGD.CAD4TB6$Comment[which(abs(BGD.CAD4TB6$X13-0.75) == min(abs(BGD.CAD4TB6$X13-0.75)))] <- "3/4 Xpert Saved"

BGD.CAD4TB6$Comment[which(abs(BGD.CAD4TB6$X1-Radiologist$Sens[1]) == min(abs(BGD.CAD4TB6$X1-Radiologist$Sens[1])))] <- paste("Radiologists' specificity = ", Radiologist$Specificity[1], sep = "")
BGD.CAD4TB6$Comment[which(abs(BGD.CAD4TB6$X1-Radiologist$Sens[2]) == min(abs(BGD.CAD4TB6$X1-Radiologist$Sens[2])))] <- paste("Radiologists' specificity = ", Radiologist$Specificity[2], sep = "")
BGD.CAD4TB6$Comment[which(abs(BGD.CAD4TB6$X1-Radiologist$Sens[3]) == min(abs(BGD.CAD4TB6$X1-Radiologist$Sens[3])))] <- paste("Radiologists' specificity = ", Radiologist$Specificity[3], sep = "")

BGD.DF <- rbind(BGD.qXR3, BGD.Lunit, BGD.Infervision, BGD.JF1, BGD.CAD4TB6)
rm(BGD.qXR3, BGD.Lunit, BGD.Infervision, BGD.JF1, BGD.CAD4TB6)

######### Merge DFs ######
# CAD_Xpert <- rbind(CAM.DF, NPL.DF, BGD.DF)
CAD_Xpert <- BGD.DF


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
colnames(CAD_Xpert)[13] <- "%XpertSaved"
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
CAD_Xpert$`%XpertSaved` <- round(CAD_Xpert$`%XpertSaved`, 8)
CAD_Xpert$accuracy <- round(CAD_Xpert$accuracy, 3)

CAD_Xpert$DeepLearningSystem <- as.character(CAD_Xpert$DeepLearningSystem)
CAD_Xpert$DeepLearningSystem[CAD_Xpert$DeepLearningSystem %in% "Infervision"] <- "InferReadDR"
CAD_Xpert$DeepLearningSystem[CAD_Xpert$DeepLearningSystem %in% "Lunit"] <- "Lunit INSIGHT CXR"
CAD_Xpert$DeepLearningSystem[CAD_Xpert$DeepLearningSystem %in% "CAD4TB"] <- "CAD4TB"
CAD_Xpert$DeepLearningSystem[CAD_Xpert$DeepLearningSystem %in% "qXR"] <- "qXR"
CAD_Xpert$DeepLearningSystem[CAD_Xpert$DeepLearningSystem %in% "JF1"] <- "JF CXR-1"

CAD_Xpert$Score[CAD_Xpert$DeepLearningSystem %in% "CAD4TB"] <- CAD_Xpert$Score[CAD_Xpert$DeepLearningSystem %in% "CAD4TB"]/100
tapply(CAD_Xpert$Score, CAD_Xpert$DeepLearningSystem, summary)


################ updating qXRv3 ----------------------
# CAD_Xpert <- read.csv("Results/CAD_Xpert_Precise.csv")
# 
# CAD_Xpert_noBGDqXR <- CAD_Xpert[!(CAD_Xpert$Site %in% "BGD" & CAD_Xpert$DeepLearningSystem %in% "qXR"), ]
# 
# BGD.qXR3 <- BGD.qXR3[, c(1:23, 27:30, 24:26, 31 )]
# colnames(CAD_Xpert_noBGDqXR)[13] <- "%XpertSaved"
# 
# CAD_Xpert <- rbind(CAD_Xpert_noBGDqXR, BGD.qXR3)


################################
SuppTable <- CAD_Xpert[, c(16, 17, 14, 27:31, 13, 15)]
View(SuppTable)
# CAD_Xpert_plot <- CAD_Xpert[, c(16, 17, 14, 13, 15, 1:12, 19, 28:30)]

# write.csv(CAD_Xpert, "Results/CAD_Xpert_qXR3.csv", row.names = F)
# write.csv(CAD_Xpert_plot, "Results/CAD_Xpert Cutoffs TABLE.csv", row.names = F)
write.csv(SuppTable, "Results/Supp Tab.csv", row.names = F)
# 
# rm(CAM.DF, NPL.DF, BGD.DF, i, DL.score, mylist, cutoff.accuracy, NPL, CAM, BGD.DF, BGD)
# 
# 
