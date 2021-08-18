source("2.0 Version Comparison/radiologist.R")

########### Human vs AI ####################
CAD_Xpert_plot <- read.csv("2.0 Version Comparison//CAD_Xpert_CAD6.3.csv")
CAD_Xpert_plot <- subset(CAD_Xpert_plot, CAD_Xpert_plot$Site %in% "BGD")


# knitr::kable(Radiologist[(Radiologist$Referral %in% "MDF"), c(8, 10, 11)])
# Human <- Radiologist[(Radiologist$Referral %in% "MDF"), c(8, 10, 11, 4)]
Human <- Radiologist[(Radiologist$Referral %in% "MDF"), ]
Human <- Human[c(3,2,1), ]
Human <- rbind(Human, Human, Human, Human, Human)


AI <- CAD_Xpert_plot[CAD_Xpert_plot$Comment !="" , c(17, 14, 4, 24, 25, 18,27,28, 7:12, 29,30)]
AI$Subject <- paste(AI$DeepLearningSystem, AI$Comment)
require(data.table)
AI <- as.data.table(AI)
# # knitr::kable(AI[AI[, .I[which.max(Spec)], by=Subject]$V1])
AI <- AI[AI[, .I[which.max(Spec)], by=Subject]$V1]

AI <- AI[grep("Radiologists' ", AI$Comment), ]


humanAI <- cbind(Human, AI)

# write.csv(humanAI, "2.0 Version Comparison//HumanAI.csv", row.names = F)

rm(AI, Human, humanAI)

humanAI <- read_csv("2.0 Version Comparison//humanAI.csv")


humanAI$specD <- percent(humanAI$AI_Spec- humanAI$Huan_Spec)
humanAI$PPVD <- percent(humanAI$AI_PPV- humanAI$Huan_PPV)
humanAI$NPVD <- percent(humanAI$AI_NPV- humanAI$Huan_NPV)


# humanAI <- humanAI[, c(1,2,3,5:8, 10, 9,4)]
# colnames(humanAI) <- c("Human Benchmark", "Sensitivity", "Specificy", "DL Product", "Score", "DL Sensitivity", "DL Specificity", "Difference", "AI_Spec", "specH")


### McNemar test specificity 
healthy <- sum(MDF$Xpert2Outcome_num %in% "0")

# library(readr)
# humanAI <- read_csv("2.0 Version Comparison//humanAI.csv", col_types = cols(`Diff.specificity` = col_number()))

humanAI$specCI <- ""

for (i in 1:15){
  test <- prop.test(x=c(humanAI$AI_Spec[i]*healthy, healthy*humanAI$Huan_Spec[i]), n=c(healthy, healthy))
  humanAI$specCI[i] <- paste0(percent(test$conf.int[1], suffix = ""), "-", percent(test$conf.int[2]))
  # return(humanAI)
}


humanAI$PPVCI <- ""

for (i in 1:15){
  test <- prop.test(x=c(humanAI$AI_PPV[i]*healthy, healthy*humanAI$Huan_PPV[i]), n=c(healthy, healthy))
  humanAI$PPVCI[i] <- paste0(percent(test$conf.int[1], suffix = ""), "-", percent(test$conf.int[2]))
  # return(humanAI)
}


humanAI$NPVCI <- ""

for (i in 1:15){
  test <- prop.test(x=c(humanAI$AI_NPV[i]*healthy, healthy*humanAI$Huan_NPV[i]), n=c(healthy, healthy))
  humanAI$NPVCI[i] <- paste0(percent(test$conf.int[1], suffix = ""), "-", percent(test$conf.int[2]))
  # return(humanAI)
}


humanAI$SpecificyIncrease <- paste0(humanAI$Difference, " (", humanAI$CI, ")")
humanAI <- humanAI[c(3,6,9,12,15, 2,5,8,11,14, 1,4,7,10,13), -c(6, 8)]
humanAI[2:5, 1:3] <- " "
humanAI[7:10, 1:3] <- " "
humanAI[12:15, 1:3] <- " "
View(humanAI)
write.csv(humanAI, "Results/table4.csv", row.names = F)
# rm(AI, Human)
