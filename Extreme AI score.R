library(tableone)

MDF[MDF$Xpert2Outcome_num %in% "1" & MDF$qXRv2_100 <0, c(1, 3, 7, 9, 14:19, 26:29)]

LowQXR <- MDF[MDF$Xpert2Outcome_num %in% "1" & MDF$qXRv2_100 <45, c(1, 3, 7, 9, 14:19, 26:29)]
# LowLunit <- MDF[MDF$Xpert2Outcome_num %in% "1" & MDF$LunitScore_100 <20, c(1, 5, 9,11, 16:21, 28, 31, 32)]
LowCAD4TB <- MDF[MDF$Xpert2Outcome_num %in% "1" & MDF$CAD4TB6 <45, c(1, 3, 7, 9, 14:19, 26:29)]
HighJF <- MDF[MDF$Xpert2Outcome_num %in% "0" & MDF$qXRv2_100 >80, c(1, 3, 7, 9, 14:19, 26:29)]


## High JF score, false positive ------------------
MDF$HighJF <- "Normal"
MDF$HighJF[MDF$Xpert2Outcome_num %in% "0" & MDF$qXRv2_100 >80] <- "FP"

# Define numeric variables
listVar <- c("Age", "Gender",  "Xpert2Outcome_num", "MTB.Burden", "RIF.Result", "Radiology.Result", "rad.highly.TB", "rad.TB", "rad.abn", "CAD4TB6", "qXRv2", "LunitScore", "JF1", "HighJF")

#Define categorical variables
catVars <- c("Gender", "MTB.Burden", "Xpert2Outcome_num","RIF.Result",  "Radiology.Result", "rad.highly.TB", "rad.TB", "rad.abn", "HighJF")
table1 <- CreateTableOne(vars = listVar, strata=c("HighJF"), data = MDF, factorVars = catVars)
print(table1)


# Every 5 point incremental ----------------
MDF$CAD4TB_C <- cut(MDF$CAD4TB6, breaks = 50)
MDF$qXR_C <- cut(MDF$qXRv2_100, breaks = 50)
MDF$Lunit_C <- cut(MDF$LunitScore_100, breaks = 50)
MDF$JF1_C <- cut(MDF$JF1_100, breaks = 50)
MDF$JF2_C <- cut(MDF$JF2_100, breaks = 50)


# Calculate the ration of Xpert negative &Xpert positive -------------------
ddply(MDF, .(JF1_C, Xpert2Outcome_num), summarise, freq = length(JF1_C))  # this is just to check
View(MDF[MDF$JF1_C %in% "(98,100]", c(8, 28)])


CAD4TB <- ddply(MDF, .(CAD4TB_C), summarise, 
      Total = length(CAD4TB_C),
      XpertP = sum(Xpert2Outcome_num))
CAD4TB$PNratio <- round(CAD4TB$XpertP/(CAD4TB$Total-CAD4TB$XpertP),2)


qXR <- ddply(MDF, .(qXR_C), summarise, 
                Total = length(qXR_C),
                XpertP = sum(Xpert2Outcome_num))
qXR$PNratio <- round(qXR$XpertP/(qXR$Total-qXR$XpertP),2)


Lunit <- ddply(MDF, .(Lunit_C), summarise, 
                Total = length(Lunit_C),
                XpertP = sum(Xpert2Outcome_num))
Lunit$PNratio <- round(Lunit$XpertP/(Lunit$Total-Lunit$XpertP),2)


JF1 <- ddply(MDF, .(JF1_C), summarise, 
               Total = length(JF1_C),
               XpertP = sum(Xpert2Outcome_num))
JF1$PNratio <- round(JF1$XpertP/(JF1$Total-JF1$XpertP),2)


JF2 <- ddply(MDF, .(JF2_C), summarise, 
               Total = length(JF2_C),
               XpertP = sum(Xpert2Outcome_num))
JF2$PNratio <- round(JF2$XpertP/(JF2$Total-JF2$XpertP),2)



low <- 0.9
high <- 1.1


teleRead1 <- MDF[MDF$CAD4TB_C %in% CAD4TB$CAD4TB_C[CAD4TB$PNratio >low & CAD4TB$PNratio <high],]
teleRead2 <- MDF[MDF$qXR_C %in% qXR$qXR_C[qXR$PNratio >low & qXR$PNratio <high],]
teleRead3 <- MDF[MDF$JF1_C %in% JF1$JF1_C[JF1$PNratio >low & JF1$PNratio <high],]
teleRead4 <- MDF[MDF$JF2_C %in% JF2$JF2_C[JF2$PNratio >low & JF2$PNratio <high],]

teleReadM <- rbind(teleRead1, teleRead2, teleRead3, teleRead4)
teleReadM <-unique(teleReadM)
rm(teleRead1, teleRead2, teleRead3, teleRead4)





# All missed anlaysis
source("DataWrangling/GlobalOption.R")

table(MDF$Radiology.Result, MDF$Xpert2Outcome_num)

CAD_Xpert_plot <- read.csv("Results/CAD_Xpert Cutoffs TABLE 4 digits.csv")
CAD_Xpert_plot <- CAD_Xpert_plot[, -1]
CAD_Xpert_plot2 <- CAD_Xpert_plot

CAD_Xpert_plot <- CAD_Xpert_plot2[CAD_Xpert_plot2$DeepLearningSystem %in% "CAD4TB", ]
cutoff <- CAD_Xpert_plot$Score[which(abs(CAD_Xpert_plot$Sens -0.95)==min(abs(CAD_Xpert_plot$Sens -0.95)))]*100
length(MDF[MDF$CAD4TB6 < cutoff & MDF$Xpert2Outcome_num %in% "1", 2])
MDF$CADMissed[MDF$CAD4TB6 < cutoff & MDF$Xpert2Outcome_num %in% "1"] <- "Missed by CAD4TB"

CAD_Xpert_plot <- CAD_Xpert_plot2[CAD_Xpert_plot2$DeepLearningSystem %in% "InferReadDR", ]
cutoff <- CAD_Xpert_plot$Score[which(abs(CAD_Xpert_plot$Sens -0.95)==min(abs(CAD_Xpert_plot$Sens -0.95)))]
MDF$IF2Missed[MDF$IF2 < cutoff & MDF$Xpert2Outcome_num %in% "1"] <- "Missed by InferReadDR"

CAD_Xpert_plot <- CAD_Xpert_plot2[CAD_Xpert_plot2$DeepLearningSystem %in% "JF CXR-1", ]
cutoff <- CAD_Xpert_plot$Score[which(abs(CAD_Xpert_plot$Sens -0.95)==min(abs(CAD_Xpert_plot$Sens -0.95)))]
MDF$JF1Missed[MDF$JF1 < cutoff & MDF$Xpert2Outcome_num %in% "1"] <- "Missed by JF CXR-1"

CAD_Xpert_plot <- CAD_Xpert_plot2[CAD_Xpert_plot2$DeepLearningSystem %in% "Lunit INSIGHT CXR", ]
cutoff <- CAD_Xpert_plot$Score[which(abs(CAD_Xpert_plot$Sens -0.95)==min(abs(CAD_Xpert_plot$Sens -0.95)))]
MDF$LunitMissed[MDF$LunitScore < cutoff & MDF$Xpert2Outcome_num %in% "1"] <- "Missed by Lunit INSIGHT CXR"

CAD_Xpert_plot <- CAD_Xpert_plot2[CAD_Xpert_plot2$DeepLearningSystem %in% "qXR", ]
cutoff <- CAD_Xpert_plot$Score[which(abs(CAD_Xpert_plot$Sens -0.95)==min(abs(CAD_Xpert_plot$Sens -0.95)))]
length(MDF[MDF$qXRv2 < cutoff & MDF$Xpert2Outcome_num %in% "1", 2])
MDF$qXRMissed[MDF$qXRv2 < cutoff & MDF$Xpert2Outcome_num %in% "1"] <- "Missed by qXR"


MDF$Comment <- ""

MDF$Comment[MDF$Radiology.Result == "X-Ray Normal" & is.na(MDF$CADMissed) == T & is.na(MDF$IF2Missed) == T & is.na(MDF$JF1Missed) == T & is.na(MDF$LunitMissed) == T & is.na(MDF$qXRMissed) == T & MDF$Xpert2Outcome_num %in% "1"] <- "Missed by human reader only"

MDF$Comment[MDF$Radiology.Result != "X-Ray Normal" & (is.na(MDF$CADMissed) == F | is.na(MDF$IF2Missed) == F | is.na(MDF$JF1Missed) == F | is.na(MDF$LunitMissed) == F | is.na(MDF$qXRMissed) == F) & MDF$Xpert2Outcome_num %in% "1"] <- "Missed by at least one AI but not missed by human"

MDF$Comment[MDF$Radiology.Result == "X-Ray Normal" & (is.na(MDF$CADMissed) == F | is.na(MDF$IF2Missed) == F | is.na(MDF$JF1Missed) == F | is.na(MDF$LunitMissed) == F | is.na(MDF$qXRMissed) == F) & MDF$Xpert2Outcome_num %in% "1"] <- "Missed by at least one AI and by human"

MDF$count <- apply(MDF[, 45:49], 1, function(x) length(grep("Missed by", x)))


MDF$Comment[MDF$Radiology.Result != "X-Ray Normal" & MDF$count>=2 & MDF$Xpert2Outcome_num %in% "1"] <- "Missed by at least 2 AI but not by human "
MDF$Comment[MDF$Radiology.Result == "X-Ray Normal" & MDF$count>=2 & MDF$Xpert2Outcome_num %in% "1"] <- "Missed by at least 2 AI AND by human "

MDF$Comment[MDF$Radiology.Result != "X-Ray Normal" & MDF$count>=3 & MDF$Xpert2Outcome_num %in% "1"] <- "Missed by at least 3 AI but not by human "
MDF$Comment[MDF$Radiology.Result == "X-Ray Normal" & MDF$count>=3 & MDF$Xpert2Outcome_num %in% "1"] <- "Missed by at least 3 AI AND by human "

MDF$Comment[MDF$Radiology.Result != "X-Ray Normal" & is.na(MDF$CADMissed) == F & is.na(MDF$IF2Missed) == F & is.na(MDF$JF1Missed) == F & is.na(MDF$LunitMissed) == F & is.na(MDF$qXRMissed) == F & MDF$Xpert2Outcome_num %in% "1"] <- "Missed by all AI but not by human"
table(MDF$Comment)



MDF$Comment2 <- ""
MDF$Comment2[MDF$JF1>=0.9 & MDF$Xpert2Outcome_num %in% "1" & MDF$TB.Medication.History %in% "No"] <- "Bac negative high JF score New (0.5%)"
MDF$Comment2[MDF$JF1>=0.9 & MDF$Xpert2Outcome_num %in% "1" & MDF$TB.Medication.History %in% "Yes"] <- "Bac negative high JF score with TB History (1%)"

MDF$Comment2[MDF$Radiology.Result == "X-Ray Normal" & MDF$Xpert2Outcome_num %in% "0"] <- "Bac Neg human normal" 
# sample_n(MDF[MDF$Comment2 %in% "Bac negative high JF score New (0.5%)", ], 14)
# sample_n(MDF[MDF$Comment2 %in% "Bac negative high JF score with TB History (1%)", ], 6)
# sample_n(MDF[MDF$Comment2 %in% "Bac Neg human normal", ], 19)
Teleradiology<- rbind(sample_n(MDF[MDF$Comment2 %in% "Bac negative high JF score New (0.5%)", ], 14), sample_n(MDF[MDF$Comment2 %in% "Bac negative high JF score with TB History (1%)", ], 6),sample_n(MDF[MDF$Comment2 %in% "Bac Neg human normal", ], 19), MDF[!MDF$Comment %in% "", ])



write.csv(Teleradiology, "Teleradiology.csv", row.names = FALSE)

# MDF <- read.csv("US Human read.csv")












