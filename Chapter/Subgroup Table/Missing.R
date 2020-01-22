## Cutoff Table (Full)
source("DataWrangling/GlobalOption.R")
source("radiologist.R")
Radiologist <- Radiologist[c(1:3), ]
Private <- subset(MDF, MDF$Referral %in% "Private Provider")
Public <- subset(MDF, MDF$Referral %in% "Public DOTS Facilities")
WalkIn <- subset(MDF, MDF$Referral %in% "Walk-in")
Missing <- subset(MDF, is.na(MDF$Referral)==T)



###### Qure.AI Score by 0.0001 #########
qure_ai_score <- seq(0,1, by = 0.0001)

mylist <- NULL
mylist <- as.list(mylist)

for (i in 1:10001){
  myfunction <- function(CountryX, car.cutoff){
    a <- sum(CountryX$qXRv2 >= car.cutoff & CountryX$Xpert2Outcome_num =="1")
    b <- sum(CountryX$qXRv2 >= car.cutoff & CountryX$Xpert2Outcome_num =="0")
    c <- sum(CountryX$qXRv2 < car.cutoff & CountryX$Xpert2Outcome_num =="1")
    d <- sum(CountryX$qXRv2 < car.cutoff & CountryX$Xpert2Outcome_num =="0")
    dat <- as.table(matrix(c(a,b,c,d), nrow = 2, byrow = TRUE))
    colnames(dat) <- c("Dis+","Dis-")
    rownames(dat) <- c("Test+","Test-")
    rval <- epi.tests(dat, conf.level = 0.95)
    sensitivity <- as.vector(round((rval$elements$sensitivity),4))
    specificity <- as.vector(round((rval$elements$specificity),4))
    pv.positive <- as.vector(round((rval$elements$pv.positive),4))
    pv.negative <- as.vector(round((rval$elements$pv.negative),4))
    accuracy <- round((a+d)/length(CountryX$Xpert2Outcome_num),4)
    
    perc_xpert_saved<- 1-sum(CountryX$qXRv2 >= car.cutoff)/length(CountryX$TID_OMRS)
    Score <- qure_ai_score[i]
    
    accuracy <- cbind(sensitivity, specificity, pv.positive, pv.negative, perc_xpert_saved, Score, accuracy)
    
    return(accuracy)
  }
  
  accuracy <- myfunction(Missing, qure_ai_score[i])
  mylist[[i]] <- list(accuracy)
  
}

QureDF <- data.frame(matrix(unlist(mylist), nrow=10001, byrow=T))
QureDF$Country <- paste("Bangladesh")

QureDF$Sen_95CI <- paste(round(QureDF[, 2], 2), "-", round(QureDF[, 3], 2), sep = "")
QureDF$Spe_95CI <- paste(round(QureDF[, 5], 2), "-", round(QureDF[, 6], 2), sep = "")
QureDF$PPV_95CI <- paste(round(QureDF[, 8], 2), "-", round(QureDF[, 9], 2), sep = "")
QureDF$NPV_95CI <- paste(round(QureDF[, 11], 2), "-", round(QureDF[, 12], 2), sep = "")
QureDF$DeepLearningSystem <- paste("qXR")
# write.csv(QureDF, "qure.ai by 0.0001.csv")
rm(qure_ai_score)

## Find the radiologist equal threthold -------------
QureDF$Comment <- ""
QureDF$Comment[which(abs(QureDF$X1-Radiologist$Sens[1]) == min(abs(QureDF$X1-Radiologist$Sens[1])))] <- paste("Radiologists' specificity = ", Radiologist$Specificity[1], sep = "")
QureDF$Comment[which(abs(QureDF$X1-Radiologist$Sens[2]) == min(abs(QureDF$X1-Radiologist$Sens[2])))] <- paste("Radiologists' specificity = ", Radiologist$Specificity[2], sep = "")
QureDF$Comment[which(abs(QureDF$X1-Radiologist$Sens[3]) == min(abs(QureDF$X1-Radiologist$Sens[3])))] <- paste("Radiologists' specificity = ", Radiologist$Specificity[3], sep = "")



#### CAD6 Score by 1 ######
CAD6 <- seq(0,100, by = 1)

mylist <- NULL
mylist <- as.list(mylist)

for (i in 1:101){
  myfunction <- function(CountryX, car.cutoff){
    a <- sum(CountryX$CAD4TB6 >= car.cutoff & CountryX$Xpert2Outcome_num =="1")
    b <- sum(CountryX$CAD4TB6 >= car.cutoff & CountryX$Xpert2Outcome_num =="0")
    c <- sum(CountryX$CAD4TB6 < car.cutoff & CountryX$Xpert2Outcome_num =="1")
    d <- sum(CountryX$CAD4TB6 < car.cutoff & CountryX$Xpert2Outcome_num =="0")
    dat <- as.table(matrix(c(a,b,c,d), nrow = 2, byrow = TRUE))
    colnames(dat) <- c("Dis+","Dis-")
    rownames(dat) <- c("Test+","Test-")
    rval <- epi.tests(dat, conf.level = 0.95)
    sensitivity <- as.vector(round((rval$elements$sensitivity),6))
    specificity <- as.vector(round((rval$elements$specificity),6))
    pv.positive <- as.vector(round((rval$elements$pv.positive),6))
    pv.negative <- as.vector(round((rval$elements$pv.negative),6))
    accuracy <- round((a+d)/length(CountryX$Xpert2Outcome_num),6)
    
    
    perc_xpert_saved<- 1-sum(CountryX$CAD4TB6 >= car.cutoff)/length(CountryX$TID_OMRS)
    Score <- CAD6[i]
    
    accuracy <- cbind(sensitivity, specificity, pv.positive, pv.negative, perc_xpert_saved, Score, accuracy)
    
    return(accuracy)
  }
  
  accuracy_both <- myfunction(Missing, CAD6[i])
  mylist[[i]] <- list(accuracy_both)
}
CAD6DF <- data.frame(matrix(unlist(mylist), nrow=101, byrow=T))
CAD6DF$Country <- paste("Bangladesh")

CAD6DF$Sen_95CI <- paste(round(CAD6DF[, 2], 2), "-", round(CAD6DF[, 3], 2), sep = "")
CAD6DF$Spe_95CI <- paste(round(CAD6DF[, 5], 2), "-", round(CAD6DF[, 6], 2), sep = "")
CAD6DF$PPV_95CI <- paste(round(CAD6DF[, 8], 2), "-", round(CAD6DF[, 9], 2), sep = "")
CAD6DF$NPV_95CI <- paste(round(CAD6DF[, 11], 2), "-", round(CAD6DF[, 12], 2), sep = "")
CAD6DF$DeepLearningSystem <- paste("CAD4TB")
## Find the radiologist equal threthold -------------
CAD6DF$Comment <- ""
CAD6DF$Comment[which(abs(CAD6DF$X1-Radiologist$Sens[1]) == min(abs(CAD6DF$X1-Radiologist$Sens[1])))] <- paste("Radiologists' specificity = ", Radiologist$Specificity[1], sep = "")
CAD6DF$Comment[which(abs(CAD6DF$X1-Radiologist$Sens[2]) == min(abs(CAD6DF$X1-Radiologist$Sens[2])))] <- paste("Radiologists' specificity = ", Radiologist$Specificity[2], sep = "")
CAD6DF$Comment[which(abs(CAD6DF$X1-Radiologist$Sens[3]) == min(abs(CAD6DF$X1-Radiologist$Sens[3])))] <- paste("Radiologists' specificity = ", Radiologist$Specificity[3], sep = "")

rm(CAD6)


###### Lunit Score by 0.0001 #########
LunitVector <- seq(0,1, by = 0.0001)

mylist <- NULL
mylist <- as.list(mylist)

for (i in 1:10001){
  myfunction <- function(CountryX, car.cutoff){
    a <- sum(CountryX$LunitScore >= car.cutoff & CountryX$Xpert2Outcome_num =="1")
    b <- sum(CountryX$LunitScore >= car.cutoff & CountryX$Xpert2Outcome_num =="0")
    c <- sum(CountryX$LunitScore < car.cutoff & CountryX$Xpert2Outcome_num =="1")
    d <- sum(CountryX$LunitScore < car.cutoff & CountryX$Xpert2Outcome_num =="0")
    dat <- as.table(matrix(c(a,b,c,d), nrow = 2, byrow = TRUE))
    colnames(dat) <- c("Dis+","Dis-")
    rownames(dat) <- c("Test+","Test-")
    rval <- epi.tests(dat, conf.level = 0.95)
    sensitivity <- as.vector(round((rval$elements$sensitivity),4))
    specificity <- as.vector(round((rval$elements$specificity),4))
    pv.positive <- as.vector(round((rval$elements$pv.positive),4))
    pv.negative <- as.vector(round((rval$elements$pv.negative),4))
    accuracy <- round((a+d)/length(CountryX$Xpert2Outcome_num),4)
    
    perc_xpert_saved<- 1-sum(CountryX$LunitScore >= car.cutoff)/length(CountryX$TID_OMRS)
    Score <- LunitVector[i]
    
    accuracy <- cbind(sensitivity, specificity, pv.positive, pv.negative, perc_xpert_saved, Score, accuracy)
    
    return(accuracy)
  }
  
  accuracy <- myfunction(Missing, LunitVector[i])
  mylist[[i]] <- list(accuracy)
  
}

LunitDF <- data.frame(matrix(unlist(mylist), nrow=10001, byrow=T))
LunitDF$Country <- paste("Bangladesh")

LunitDF$Sen_95CI <- paste(round(LunitDF[, 2], 2), "-", round(LunitDF[, 3], 2), sep = "")
LunitDF$Spe_95CI <- paste(round(LunitDF[, 5], 2), "-", round(LunitDF[, 6], 2), sep = "")
LunitDF$PPV_95CI <- paste(round(LunitDF[, 8], 2), "-", round(LunitDF[, 9], 2), sep = "")
LunitDF$NPV_95CI <- paste(round(LunitDF[, 11], 2), "-", round(LunitDF[, 12], 2), sep = "")
LunitDF$DeepLearningSystem <- paste("Lunit")

## Find the radiologist equal threthold -------------
LunitDF$Comment <- ""
LunitDF$Comment[which(abs(LunitDF$X1-Radiologist$Sens[1]) == min(abs(LunitDF$X1-Radiologist$Sens[1])))] <- paste("Radiologists' specificity = ", Radiologist$Specificity[1], sep = "")
LunitDF$Comment[which(abs(LunitDF$X1-Radiologist$Sens[2]) == min(abs(LunitDF$X1-Radiologist$Sens[2])))] <- paste("Radiologists' specificity = ", Radiologist$Specificity[2], sep = "")
LunitDF$Comment[which(abs(LunitDF$X1-Radiologist$Sens[3]) == min(abs(LunitDF$X1-Radiologist$Sens[3])))] <- paste("Radiologists' specificity = ", Radiologist$Specificity[3], sep = "")

# write.csv(LunitDF, "Lunit by 0.0001.csv")
rm(LunitVector)



###### JF1 Score by 0.0001 #########
JF1Vector <- seq(0,1, by = 0.0001)

mylist <- NULL
mylist <- as.list(mylist)

for (i in 1:10001){
  myfunction <- function(CountryX, car.cutoff){
    a <- sum(CountryX$JF1 >= car.cutoff & CountryX$Xpert2Outcome_num =="1")
    b <- sum(CountryX$JF1 >= car.cutoff & CountryX$Xpert2Outcome_num =="0")
    c <- sum(CountryX$JF1 < car.cutoff & CountryX$Xpert2Outcome_num =="1")
    d <- sum(CountryX$JF1 < car.cutoff & CountryX$Xpert2Outcome_num =="0")
    dat <- as.table(matrix(c(a,b,c,d), nrow = 2, byrow = TRUE))
    colnames(dat) <- c("Dis+","Dis-")
    rownames(dat) <- c("Test+","Test-")
    rval <- epi.tests(dat, conf.level = 0.95)
    sensitivity <- as.vector(round((rval$elements$sensitivity),4))
    specificity <- as.vector(round((rval$elements$specificity),4))
    pv.positive <- as.vector(round((rval$elements$pv.positive),4))
    pv.negative <- as.vector(round((rval$elements$pv.negative),4))
    accuracy <- round((a+d)/length(CountryX$Xpert2Outcome_num),4)
    
    perc_xpert_saved<- 1-sum(CountryX$JF1 >= car.cutoff)/length(CountryX$TID_OMRS)
    Score <- JF1Vector[i]
    
    accuracy <- cbind(sensitivity, specificity, pv.positive, pv.negative, perc_xpert_saved, Score, accuracy)
    
    return(accuracy)
  }
  
  accuracy <- myfunction(Missing, JF1Vector[i])
  mylist[[i]] <- list(accuracy)
  
}

JF1DF <- data.frame(matrix(unlist(mylist), nrow=10001, byrow=T))
JF1DF$Country <- paste("Bangladesh")

JF1DF$Sen_95CI <- paste(round(JF1DF[, 2], 2), "-", round(JF1DF[, 3], 2), sep = "")
JF1DF$Spe_95CI <- paste(round(JF1DF[, 5], 2), "-", round(JF1DF[, 6], 2), sep = "")
JF1DF$PPV_95CI <- paste(round(JF1DF[, 8], 2), "-", round(JF1DF[, 9], 2), sep = "")
JF1DF$NPV_95CI <- paste(round(JF1DF[, 11], 2), "-", round(JF1DF[, 12], 2), sep = "")
JF1DF$DeepLearningSystem <- paste("JF1")
## Find the radiologist equal threthold -------------
JF1DF$Comment <- ""
JF1DF$Comment[which(abs(JF1DF$X1-Radiologist$Sens[1]) == min(abs(JF1DF$X1-Radiologist$Sens[1])))] <- paste("Radiologists' specificity = ", Radiologist$Specificity[1], sep = "")
JF1DF$Comment[which(abs(JF1DF$X1-Radiologist$Sens[2]) == min(abs(JF1DF$X1-Radiologist$Sens[2])))] <- paste("Radiologists' specificity = ", Radiologist$Specificity[2], sep = "")
JF1DF$Comment[which(abs(JF1DF$X1-Radiologist$Sens[3]) == min(abs(JF1DF$X1-Radiologist$Sens[3])))] <- paste("Radiologists' specificity = ", Radiologist$Specificity[3], sep = "")

# write.csv(JF1DF, "JF1 by 0.0001.csv")
rm(JF1Vector)


###### JF2 Score by 0.0001 #########
JF2Vector <- seq(0,1, by = 0.0001)

mylist <- NULL
mylist <- as.list(mylist)

for (i in 1:10001){
  myfunction <- function(CountryX, car.cutoff){
    a <- sum(CountryX$JF2 >= car.cutoff & CountryX$Xpert2Outcome_num =="1")
    b <- sum(CountryX$JF2 >= car.cutoff & CountryX$Xpert2Outcome_num =="0")
    c <- sum(CountryX$JF2 < car.cutoff & CountryX$Xpert2Outcome_num =="1")
    d <- sum(CountryX$JF2 < car.cutoff & CountryX$Xpert2Outcome_num =="0")
    dat <- as.table(matrix(c(a,b,c,d), nrow = 2, byrow = TRUE))
    colnames(dat) <- c("Dis+","Dis-")
    rownames(dat) <- c("Test+","Test-")
    rval <- epi.tests(dat, conf.level = 0.95)
    sensitivity <- as.vector(round((rval$elements$sensitivity),4))
    specificity <- as.vector(round((rval$elements$specificity),4))
    pv.positive <- as.vector(round((rval$elements$pv.positive),4))
    pv.negative <- as.vector(round((rval$elements$pv.negative),4))
    accuracy <- round((a+d)/length(CountryX$Xpert2Outcome_num), 4)
    
    perc_xpert_saved<- 1-sum(CountryX$JF2 >= car.cutoff)/length(CountryX$TID_OMRS)
    Score <- JF2Vector[i]
    
    accuracy <- cbind(sensitivity, specificity, pv.positive, pv.negative, perc_xpert_saved, Score, accuracy)
    
    return(accuracy)
  }
  
  accuracy <- myfunction(Missing, JF2Vector[i])
  mylist[[i]] <- list(accuracy)
  
}

JF2DF <- data.frame(matrix(unlist(mylist), nrow=10001, byrow=T))
JF2DF$Country <- paste("Bangladesh")

JF2DF$Sen_95CI <- paste(round(JF2DF[, 2], 2), "-", round(JF2DF[, 3], 2), sep = "")
JF2DF$Spe_95CI <- paste(round(JF2DF[, 5], 2), "-", round(JF2DF[, 6], 2), sep = "")
JF2DF$PPV_95CI <- paste(round(JF2DF[, 8], 2), "-", round(JF2DF[, 9], 2), sep = "")
JF2DF$NPV_95CI <- paste(round(JF2DF[, 11], 2), "-", round(JF2DF[, 12], 2), sep = "")
JF2DF$DeepLearningSystem <- paste("JF2")
## Find the radiologist equal threthold -------------
JF2DF$Comment <- ""
JF2DF$Comment[which(abs(JF2DF$X1-Radiologist$Sens[1]) == min(abs(JF2DF$X1-Radiologist$Sens[1])))] <- paste("Radiologists' specificity = ", Radiologist$Specificity[1], sep = "")
JF2DF$Comment[which(abs(JF2DF$X1-Radiologist$Sens[2]) == min(abs(JF2DF$X1-Radiologist$Sens[2])))] <- paste("Radiologists' specificity = ", Radiologist$Specificity[2], sep = "")
JF2DF$Comment[which(abs(JF2DF$X1-Radiologist$Sens[3]) == min(abs(JF2DF$X1-Radiologist$Sens[3])))] <- paste("Radiologists' specificity = ", Radiologist$Specificity[3], sep = "")
# write.csv(JF2DF, "JF2 by 0.0001.csv")
rm(JF2Vector)



###### IF1 Score by 0.0001 #########
IF1Vector <- seq(0,1, by = 0.0001)

mylist <- NULL
mylist <- as.list(mylist)

for (i in 1:10001){
  myfunction <- function(CountryX, car.cutoff){
    a <- sum(CountryX$IF1 >= car.cutoff & CountryX$Xpert2Outcome_num =="1")
    b <- sum(CountryX$IF1 >= car.cutoff & CountryX$Xpert2Outcome_num =="0")
    c <- sum(CountryX$IF1 < car.cutoff & CountryX$Xpert2Outcome_num =="1")
    d <- sum(CountryX$IF1 < car.cutoff & CountryX$Xpert2Outcome_num =="0")
    dat <- as.table(matrix(c(a,b,c,d), nrow = 2, byrow = TRUE))
    colnames(dat) <- c("Dis+","Dis-")
    rownames(dat) <- c("Test+","Test-")
    rval <- epi.tests(dat, conf.level = 0.95)
    sensitivity <- as.vector(round((rval$elements$sensitivity),4))
    specificity <- as.vector(round((rval$elements$specificity),4))
    pv.positive <- as.vector(round((rval$elements$pv.positive),4))
    pv.negative <- as.vector(round((rval$elements$pv.negative),4))
    accuracy <- round((a+d)/length(CountryX$Xpert2Outcome_num),4)
    
    perc_xpert_saved<- 1-sum(CountryX$IF1 >= car.cutoff)/length(CountryX$TID_OMRS)
    Score <- IF1Vector[i]
    
    accuracy <- cbind(sensitivity, specificity, pv.positive, pv.negative, perc_xpert_saved, Score, accuracy)
    
    return(accuracy)
  }
  
  accuracy <- myfunction(Missing, IF1Vector[i])
  mylist[[i]] <- list(accuracy)
  
}

IF1DF <- data.frame(matrix(unlist(mylist), nrow=10001, byrow=T))
IF1DF$Country <- paste("Bangladesh")

IF1DF$Sen_95CI <- paste(round(IF1DF[, 2], 2), "-", round(IF1DF[, 3], 2), sep = "")
IF1DF$Spe_95CI <- paste(round(IF1DF[, 5], 2), "-", round(IF1DF[, 6], 2), sep = "")
IF1DF$PPV_95CI <- paste(round(IF1DF[, 8], 2), "-", round(IF1DF[, 9], 2), sep = "")
IF1DF$NPV_95CI <- paste(round(IF1DF[, 11], 2), "-", round(IF1DF[, 12], 2), sep = "")
IF1DF$DeepLearningSystem <- paste("IF1")
## Find the radiologist equal threthold -------------
IF1DF$Comment <- ""
IF1DF$Comment[which(abs(IF1DF$X1-Radiologist$Sens[1]) == min(abs(IF1DF$X1-Radiologist$Sens[1])))] <- paste("Radiologists' specificity = ", Radiologist$Specificity[1], sep = "")
IF1DF$Comment[which(abs(IF1DF$X1-Radiologist$Sens[2]) == min(abs(IF1DF$X1-Radiologist$Sens[2])))] <- paste("Radiologists' specificity = ", Radiologist$Specificity[2], sep = "")
IF1DF$Comment[which(abs(IF1DF$X1-Radiologist$Sens[3]) == min(abs(IF1DF$X1-Radiologist$Sens[3])))] <- paste("Radiologists' specificity = ", Radiologist$Specificity[3], sep = "")

# write.csv(IF1DF, "IF1 by 0.0001.csv")
rm(IF1Vector)


###### IF2 Score by 0.0001 #########
IF2Vector <- seq(0,1, by = 0.0001)

mylist <- NULL
mylist <- as.list(mylist)

for (i in 1:10001){
  myfunction <- function(CountryX, car.cutoff){
    a <- sum(CountryX$IF2 >= car.cutoff & CountryX$Xpert2Outcome_num =="1")
    b <- sum(CountryX$IF2 >= car.cutoff & CountryX$Xpert2Outcome_num =="0")
    c <- sum(CountryX$IF2 < car.cutoff & CountryX$Xpert2Outcome_num =="1")
    d <- sum(CountryX$IF2 < car.cutoff & CountryX$Xpert2Outcome_num =="0")
    dat <- as.table(matrix(c(a,b,c,d), nrow = 2, byrow = TRUE))
    colnames(dat) <- c("Dis+","Dis-")
    rownames(dat) <- c("Test+","Test-")
    rval <- epi.tests(dat, conf.level = 0.95)
    sensitivity <- as.vector(round((rval$elements$sensitivity),4))
    specificity <- as.vector(round((rval$elements$specificity),4))
    pv.positive <- as.vector(round((rval$elements$pv.positive),4))
    pv.negative <- as.vector(round((rval$elements$pv.negative),4))
    accuracy <- round((a+d)/length(CountryX$Xpert2Outcome_num), 4)
    
    perc_xpert_saved<- 1-sum(CountryX$IF2 >= car.cutoff)/length(CountryX$TID_OMRS)
    Score <- IF2Vector[i]
    
    accuracy <- cbind(sensitivity, specificity, pv.positive, pv.negative, perc_xpert_saved, Score, accuracy)
    
    return(accuracy)
  }
  
  accuracy <- myfunction(Missing, IF2Vector[i])
  mylist[[i]] <- list(accuracy)
  
}

IF2DF <- data.frame(matrix(unlist(mylist), nrow=10001, byrow=T))
IF2DF$Country <- paste("Bangladesh")

IF2DF$Sen_95CI <- paste(round(IF2DF[, 2], 2), "-", round(IF2DF[, 3], 2), sep = "")
IF2DF$Spe_95CI <- paste(round(IF2DF[, 5], 2), "-", round(IF2DF[, 6], 2), sep = "")
IF2DF$PPV_95CI <- paste(round(IF2DF[, 8], 2), "-", round(IF2DF[, 9], 2), sep = "")
IF2DF$NPV_95CI <- paste(round(IF2DF[, 11], 2), "-", round(IF2DF[, 12], 2), sep = "")
IF2DF$DeepLearningSystem <- paste("IF2")
## Find the radiologist equal threthold -------------
IF2DF$Comment <- ""
IF2DF$Comment[which(abs(IF2DF$X1-Radiologist$Sens[1]) == min(abs(IF2DF$X1-Radiologist$Sens[1])))] <- paste("Radiologists' specificity = ", Radiologist$Specificity[1], sep = "")
IF2DF$Comment[which(abs(IF2DF$X1-Radiologist$Sens[2]) == min(abs(IF2DF$X1-Radiologist$Sens[2])))] <- paste("Radiologists' specificity = ", Radiologist$Specificity[2], sep = "")
IF2DF$Comment[which(abs(IF2DF$X1-Radiologist$Sens[3]) == min(abs(IF2DF$X1-Radiologist$Sens[3])))] <- paste("Radiologists' specificity = ", Radiologist$Specificity[3], sep = "")
# write.csv(IF2DF, "IF2 by 0.0001.csv")
rm(IF2Vector)

###### IF3 Score by 0.0001 #########
IF3Vector <- seq(0,1, by = 0.0001)

mylist <- NULL
mylist <- as.list(mylist)

for (i in 1:10001){
  myfunction <- function(CountryX, car.cutoff){
    a <- sum(CountryX$IF3 >= car.cutoff & CountryX$Xpert2Outcome_num =="1")
    b <- sum(CountryX$IF3 >= car.cutoff & CountryX$Xpert2Outcome_num =="0")
    c <- sum(CountryX$IF3 < car.cutoff & CountryX$Xpert2Outcome_num =="1")
    d <- sum(CountryX$IF3 < car.cutoff & CountryX$Xpert2Outcome_num =="0")
    dat <- as.table(matrix(c(a,b,c,d), nrow = 2, byrow = TRUE))
    colnames(dat) <- c("Dis+","Dis-")
    rownames(dat) <- c("Test+","Test-")
    rval <- epi.tests(dat, conf.level = 0.95)
    sensitivity <- as.vector(round((rval$elements$sensitivity),4))
    specificity <- as.vector(round((rval$elements$specificity),4))
    pv.positive <- as.vector(round((rval$elements$pv.positive),4))
    pv.negative <- as.vector(round((rval$elements$pv.negative),4))
    accuracy <- round((a+d)/length(CountryX$Xpert2Outcome_num), 4)
    
    perc_xpert_saved<- 1-sum(CountryX$IF3 >= car.cutoff)/length(CountryX$TID_OMRS)
    Score <- IF3Vector[i]
    
    accuracy <- cbind(sensitivity, specificity, pv.positive, pv.negative, perc_xpert_saved, Score, accuracy)
    
    return(accuracy)
  }
  
  accuracy <- myfunction(Missing, IF3Vector[i])
  mylist[[i]] <- list(accuracy)
  
}

IF3DF <- data.frame(matrix(unlist(mylist), nrow=10001, byrow=T))
IF3DF$Country <- paste("Bangladesh")

IF3DF$Sen_95CI <- paste(round(IF3DF[, 2], 2), "-", round(IF3DF[, 3], 2), sep = "")
IF3DF$Spe_95CI <- paste(round(IF3DF[, 5], 2), "-", round(IF3DF[, 6], 2), sep = "")
IF3DF$PPV_95CI <- paste(round(IF3DF[, 8], 2), "-", round(IF3DF[, 9], 2), sep = "")
IF3DF$NPV_95CI <- paste(round(IF3DF[, 11], 2), "-", round(IF3DF[, 12], 2), sep = "")
IF3DF$DeepLearningSystem <- paste("IF3")
## Find the radiologist equal threthold -------------
IF3DF$Comment <- ""
IF3DF$Comment[which(abs(IF3DF$X1-Radiologist$Sens[1]) == min(abs(IF3DF$X1-Radiologist$Sens[1])))] <- paste("Radiologists' specificity = ", Radiologist$Specificity[1], sep = "")
IF3DF$Comment[which(abs(IF3DF$X1-Radiologist$Sens[2]) == min(abs(IF3DF$X1-Radiologist$Sens[2])))] <- paste("Radiologists' specificity = ", Radiologist$Specificity[2], sep = "")
IF3DF$Comment[which(abs(IF3DF$X1-Radiologist$Sens[3]) == min(abs(IF3DF$X1-Radiologist$Sens[3])))] <- paste("Radiologists' specificity = ", Radiologist$Specificity[3], sep = "")

rm(IF3Vector)

## Find the cost saving threthold -------------
CAD6DF$Comment[which(abs(CAD6DF$X13-0.5) == min(abs(CAD6DF$X13-0.5)))] <- "1/2 Xpert Saved"
CAD6DF$Comment[which(abs(CAD6DF$X13-2/3) == min(abs(CAD6DF$X13-2/3)))] <- "2/3 Xpert Saved"
CAD6DF$Comment[which(abs(CAD6DF$X13-0.75) == min(abs(CAD6DF$X13-0.75)))] <- "3/4 Xpert Saved"



QureDF$Comment[which(abs(QureDF$X13-0.5) == min(abs(QureDF$X13-0.5)))] <- "1/2 Xpert Saved"
QureDF$Comment[which(abs(QureDF$X13-2/3) == min(abs(QureDF$X13-2/3)))] <- "2/3 Xpert Saved"
QureDF$Comment[which(abs(QureDF$X13-0.75) == min(abs(QureDF$X13-0.75)))] <- "3/4 Xpert Saved"


JF1DF$Comment[which(abs(JF1DF$X13-0.5) == min(abs(JF1DF$X13-0.5)))] <- "1/2 Xpert Saved"
JF1DF$Comment[which(abs(JF1DF$X13-2/3) == min(abs(JF1DF$X13-2/3)))] <- "2/3 Xpert Saved"
JF1DF$Comment[which(abs(JF1DF$X13-0.75) == min(abs(JF1DF$X13-0.75)))] <- "3/4 Xpert Saved"


JF2DF$Comment[which(abs(JF2DF$X13-0.5) == min(abs(JF2DF$X13-0.5)))] <- "1/2 Xpert Saved"
JF2DF$Comment[which(abs(JF2DF$X13-2/3) == min(abs(JF2DF$X13-2/3)))] <- "2/3 Xpert Saved"
JF2DF$Comment[which(abs(JF2DF$X13-0.75) == min(abs(JF2DF$X13-0.75)))] <- "3/4 Xpert Saved"


IF1DF$Comment[which(abs(IF1DF$X13-0.5) == min(abs(IF1DF$X13-0.5)))] <- "1/2 Xpert Saved"
IF1DF$Comment[which(abs(IF1DF$X13-2/3) == min(abs(IF1DF$X13-2/3)))] <- "2/3 Xpert Saved"
IF1DF$Comment[which(abs(IF1DF$X13-0.75) == min(abs(IF1DF$X13-0.75)))] <- "3/4 Xpert Saved"


IF2DF$Comment[which(abs(IF2DF$X13-0.5) == min(abs(IF2DF$X13-0.5)))] <- "1/2 Xpert Saved"
IF2DF$Comment[which(abs(IF2DF$X13-2/3) == min(abs(IF2DF$X13-2/3)))] <- "2/3 Xpert Saved"
IF2DF$Comment[which(abs(IF2DF$X13-0.75) == min(abs(IF2DF$X13-0.75)))] <- "3/4 Xpert Saved"


IF3DF$Comment[which(abs(IF3DF$X13-0.5) == min(abs(IF3DF$X13-0.5)))] <- "1/2 Xpert Saved"
IF3DF$Comment[which(abs(IF3DF$X13-2/3) == min(abs(IF3DF$X13-2/3)))] <- "2/3 Xpert Saved"
IF3DF$Comment[which(abs(IF3DF$X13-0.75) == min(abs(IF3DF$X13-0.75)))] <- "3/4 Xpert Saved"


LunitDF$Comment[which(abs(LunitDF$X13-0.5) == min(abs(LunitDF$X13-0.5)))] <- "1/2 Xpert Saved"
LunitDF$Comment[which(abs(LunitDF$X13-2/3) == min(abs(LunitDF$X13-2/3)))] <- "2/3 Xpert Saved"
LunitDF$Comment[which(abs(LunitDF$X13-0.75) == min(abs(LunitDF$X13-0.75)))] <- "3/4 Xpert Saved"


######### Merge DFs ######
CAD_Xpert <- rbind(CAD6DF, QureDF, JF1DF, JF2DF, IF1DF, IF2DF, IF3DF, LunitDF)
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
CAD_Xpert$Sensitivity <- paste(round(CAD_Xpert$Sens, 4), "(", CAD_Xpert$Sen_95CI, ")", sep = "")
CAD_Xpert$Specificity <- paste(round(CAD_Xpert$Spec, 4), "(", CAD_Xpert$Spe_95CI, ")", sep = "")
CAD_Xpert$PPV <- paste(round(CAD_Xpert$ppv, 4), "(", CAD_Xpert$PPV_95CI, ")", sep = "")
CAD_Xpert$NPV <- paste(round(CAD_Xpert$npv, 4), "(", CAD_Xpert$NPV_95CI, ")", sep = "")
CAD_Xpert$`%XpertSaved` <- round(CAD_Xpert$`%XpertSaved`, 4)
CAD_Xpert$X <- 1-CAD_Xpert$Spec

CAD_Xpert_select <- CAD_Xpert[, c(16, 21, 14, 13, 15, 1, 23:27, 22)]
CAD_Xpert_plot_Missing <- CAD_Xpert[, c(16, 21, 14, 13, 15, 1:3, 4,7, 5,6,8:12, 23:27, 22)]

CAD_Xpert_plot_Missing$subgroup <- "Missing"

# CAD_Xpert_select <- CAD_Xpert_select[!CAD_Xpert_select$DeepLearningSystem %in% c("IF1", "JF2", "IF3"), ]

# write.csv(CAD_Xpert_plot_Missing, "Results/CAD_Xpert Cutoffs TABLE_Missing.csv")
# write.csv(CAD_Xpert_select, "Results/Supp Tab_Missing.csv", row.names = F)
rm(accuracy, accuracy_both, CAD6DF, QureDF, JF1DF, JF2DF, LunitDF, IF1DF, IF2DF, IF3DF, mylist, Radiologist, i, dat)

