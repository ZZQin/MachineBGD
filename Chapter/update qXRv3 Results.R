colnames(BGD.qXR3)[1] <- "Sens"
colnames(BGD.qXR3)[2] <- "Sens_L"
colnames(BGD.qXR3)[3] <- "Sens_H"
colnames(BGD.qXR3)[4] <- "Spec"
colnames(BGD.qXR3)[5] <- "Spec_L"
colnames(BGD.qXR3)[6] <- "Spec_H"
colnames(BGD.qXR3)[7] <- "ppv"
colnames(BGD.qXR3)[8] <- "PPV_L"
colnames(BGD.qXR3)[9] <- "PPV_H"
colnames(BGD.qXR3)[10] <- "npv"
colnames(BGD.qXR3)[11] <- "NPV_L"
colnames(BGD.qXR3)[12] <- "NPV_H"
colnames(BGD.qXR3)[13] <- "%XpertSaved"
colnames(BGD.qXR3)[14] <- "Score"
colnames(BGD.qXR3)[15] <- "accuracy"
colnames(BGD.qXR3)[16] <- "Site"

BGD.qXR3$X <- 1-BGD.qXR3$Spec

BGD.qXR3$Sen_95CI <- paste(percent(BGD.qXR3[, 2], suffix = ""), "-", percent(BGD.qXR3[, 3]), sep = "")
BGD.qXR3$Spe_95CI <- paste(percent(BGD.qXR3[, 5], suffix = ""), "-", percent(BGD.qXR3[, 6]), sep = "")
BGD.qXR3$PPV_95CI <- paste(percent(BGD.qXR3[, 8], suffix = ""), "-", percent(BGD.qXR3[, 9]), sep = "")
BGD.qXR3$NPV_95CI <- paste(percent(BGD.qXR3[, 11], suffix = ""), "-", percent(BGD.qXR3[, 12]), sep = "")
BGD.qXR3$NNT <- 1/BGD.qXR3$ppv
BGD.qXR3$NNT_H <- 1/BGD.qXR3$PPV_L
BGD.qXR3$NNT_L <- 1/BGD.qXR3$PPV_H



BGD.qXR3$Sensitivity <- paste(percent(BGD.qXR3$Sens, accuracy = 0.1), " (", BGD.qXR3$Sen_95CI, ")", sep = "")
BGD.qXR3$Specificity <- paste(percent(BGD.qXR3$Spec, accuracy = 0.1), " (", BGD.qXR3$Spe_95CI, ")", sep = "")
BGD.qXR3$PPV <- paste(percent(BGD.qXR3$ppv, accuracy = 0.1), " (", BGD.qXR3$PPV_95CI, ")", sep = "")
BGD.qXR3$NPV <- paste(percent(BGD.qXR3$npv, accuracy = 0.1), " (", BGD.qXR3$NPV_95CI, ")", sep = "")
BGD.qXR3$nnt <- paste(round(BGD.qXR3$NNT, 1), " (", round(BGD.qXR3$NNT_L, 1), "-", round(BGD.qXR3$NNT_H, 1), ")", sep = "")
BGD.qXR3$`%XpertSaved` <- round(BGD.qXR3$`%XpertSaved`, 8)
BGD.qXR3$accuracy <- round(BGD.qXR3$accuracy, 3)

write.csv(BGD.qXR3, "Results/BGD.qXR3.csv")

CAD_Xpert <- read.csv("Results/CAD_Xpert_Precise.csv")
