source("2.0 Version Comparison/radiologist.R")
source("2.0 Version Comparison/Global.R")

# ########### Human vs AI ####################
# CAD_Xpert_plot <- read.csv("2.0 Version Comparison/CAD_Xpert.csv")
# 
# 
# # knitr::kable(Radiologist[(Radiologist$Referral %in% "MDF"), c(8, 10, 11)])
# # Human <- Radiologist[(Radiologist$Referral %in% "MDF"), c(8, 10, 11, 4)]
# Human <- Radiologist[(Radiologist$Referral %in% "MDF"), ]
# Human <- Human[c(3,2,1), ]
# Human <- rbind(Human, Human, Human, Human)
# 
# 
# AI <- CAD_Xpert_plot[CAD_Xpert_plot$Comment !="" , c(17, 14, 4, 24, 25, 18,27,28, 7:12, 29,30)]
# AI$Subject <- paste(AI$DeepLearningSystem, AI$Comment)
# require(data.table)
# AI <- as.data.table(AI)
# # # knitr::kable(AI[AI[, .I[which.max(Spec)], by=Subject]$V1])
# AI <- AI[AI[, .I[which.max(Spec)], by=Subject]$V1]
# 
# AI <- AI[grep("Radiologists' ", AI$Comment), ]
# 
# 
# humanAI <- cbind(Human, AI)
# 
# write.csv(humanAI, "2.0 Version Comparison/HumanAI.csv", row.names = F)
# 
# 
# 


rm(AI, Human, humanAI)
humanAI <- read_csv("2.0 Version Comparison/humanAI.csv")
names(humanAI)
names(humanAI)[22] <- "AI_Spec"
names(humanAI)[4] <- "Huan_Spec"
names(humanAI)[28] <- "AI_PPV"
names(humanAI)[7] <- "Huan_PPV"
names(humanAI)[31] <- "AI_NPV"
names(humanAI)[10] <- "Huan_NPV"

humanAI$specD <- percent(humanAI$AI_Spec- humanAI$Huan_Spec)
humanAI$PPVD <- percent(humanAI$AI_PPV- humanAI$Huan_PPV)
humanAI$NPVD <- percent(humanAI$AI_NPV- humanAI$Huan_NPV)



### McNemar test specificity 
healthy <- sum(MDF$Xpert2Outcome_num %in% "0")

# library(readr)
# humanAI <- read_csv("2.0 Version Comparison/humanAI.csv", col_types = cols(`Diff.specificity` = col_number()))

humanAI$specCI <- ""

for (i in 1:12){
  test <- prop.test(x=c(humanAI$AI_Spec[i]*healthy, healthy*humanAI$Huan_Spec[i]), n=c(healthy, healthy))
  humanAI$specCI[i] <- paste0(percent(test$conf.int[1], suffix = ""), "-", percent(test$conf.int[2]))
  # return(humanAI)
}


humanAI$PPVCI <- ""

for (i in 1:12){
  test <- prop.test(x=c(humanAI$AI_PPV[i]*healthy, healthy*humanAI$Huan_PPV[i]), n=c(healthy, healthy))
  humanAI$PPVCI[i] <- paste0(percent(test$conf.int[1], suffix = ""), "-", percent(test$conf.int[2]))
  # return(humanAI)
}


humanAI$NPVCI <- ""

for (i in 1:12){
  test <- prop.test(x=c(humanAI$AI_NPV[i]*healthy, healthy*humanAI$Huan_NPV[i]), n=c(healthy, healthy))
  humanAI$NPVCI[i] <- paste0(percent(test$conf.int[1], suffix = ""), "-", percent(test$conf.int[2]))
  # return(humanAI)
}


humanAI$SpecificyIncrease <- paste0(humanAI$specD, " (", humanAI$specCI, ")")
humanAI$PPVIncrease <- paste0(humanAI$PPVD, " (", humanAI$PPVCI, ")")
humanAI$NPVIncrease <- paste0(humanAI$NPVD, " (", humanAI$NPVCI, ")")

humanAI <- humanAI[, c(14, 16:19, 20:21, 26,27, 34:35, 43:45)]

write.csv(humanAI, "2.0 Version Comparison/AI vs Human Results.csv", row.names = F)
# rm(AI, Human)
