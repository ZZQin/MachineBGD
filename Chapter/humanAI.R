source("radiologist.R")

########### Human vs AI ####################
CAD_Xpert_plot <- read.csv("Results/CAD_Xpert_CAD6.3.csv")
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

# write.csv(humanAI, "Results/HumanAI.csv", row.names = F)

rm(AI, Human, humanAI)

humanAI <- read_csv("Results/humanAI.csv")


humanAI$specD <- percent(humanAI[, 9]- humanAI[, 4])
# humanAI <- humanAI[, c(1,2,3,5:8, 10, 9,4)]
# colnames(humanAI) <- c("Human Benchmark", "Sensitivity", "Specificy", "DL Product", "Score", "DL Sensitivity", "DL Specificity", "Difference", "specAI", "specH")


### McNemar test specificity 
healthy <- sum(MDF$Xpert2Outcome_num %in% "0")

# library(readr)
# humanAI <- read_csv("Results/humanAI.csv", 
#                     col_types = cols(`Diff.specificity` = col_number()))
humanAI$specCI <- ""

for (i in 1:15){
  test <- prop.test(x=c(humanAI$specAI[i]*healthy, healthy*humanAI$specH[i]), n=c(healthy, healthy))
  humanAI$CI[i] <- paste0(percent(test$conf.int[1], suffix = ""), "-", percent(test$conf.int[2]))
  # return(humanAI)
}


humanAI$specCI <- ""

for (i in 1:15){
  test <- prop.test(x=c(humanAI$specAI[i]*healthy, healthy*humanAI$specH[i]), n=c(healthy, healthy))
  humanAI$CI[i] <- paste0(percent(test$conf.int[1], suffix = ""), "-", percent(test$conf.int[2]))
  # return(humanAI)
}


humanAI$specCI <- ""

for (i in 1:15){
  test <- prop.test(x=c(humanAI$specAI[i]*healthy, healthy*humanAI$specH[i]), n=c(healthy, healthy))
  humanAI$CI[i] <- paste0(percent(test$conf.int[1], suffix = ""), "-", percent(test$conf.int[2]))
  # return(humanAI)
}


humanAI$SpecificyIncrease <- paste0(humanAI$Difference, " (", humanAI$CI, ")")
humanAI <- humanAI[c(3,6,9,12,15, 2,5,8,11,14, 1,4,7,10,13), -c(6, 8)]
humanAI[2:5, 1:3] <- " "
humanAI[7:10, 1:3] <- " "
humanAI[12:15, 1:3] <- " "
View(humanAI)
write.csv(humanAI, "Results/humanAI.csv", row.names = F)
# rm(AI, Human)
