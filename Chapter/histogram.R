source("DataWrangling/GlobalOption.R")
MDF_long$XpertHistory <- ""
MDF_long$XpertHistory[MDF_long$TB.Medication.History %in% "No" & MDF_long$Xpert2Outcome_num %in% "0"] <- "Xpert Negative - no TB History"
MDF_long$XpertHistory[MDF_long$TB.Medication.History %in% "Yes" & MDF_long$Xpert2Outcome_num %in% "0"] <- "Xpert Negative - with TB History"

MDF_long$XpertHistory[MDF_long$TB.Medication.History %in% "No" & MDF_long$Xpert2Outcome_num %in% "1"] <- "Xpert Positive - no TB History"
MDF_long$XpertHistory[MDF_long$TB.Medication.History %in% "Yes" & MDF_long$Xpert2Outcome_num %in% "1"] <- "Xpert Positive - with TB History"


## Histogram
p <- ggplot(MDF_long,aes(x=AbnormalityScore, fill=XpertHistory,))+ geom_histogram(position = 'stack', alpha=0.5, breaks=seq(0,100, by=5), aes(y = ..count.., fill = XpertHistory)) + xlab("Abnormality Scores of the Deep Learning Systems") 
hist <- p + facet_wrap(~DeepLearningSystem) + theme_minimal() + theme(legend.position = "top") +scale_fill_manual(values=c("#f7f7f7", "#fbb4b9", "#91003f", "#a1dab4", "#006d2c"))


tiff("Results/Histogram.tif", width = 14, height = 8, units = "in", res = 100)
hist
dev.off()

## Density plot
p <- ggplot(MDF_long,aes(x=AbnormalityScore, fill=XpertHistory,))+ geom_histogram(position = 'identity', alpha=0.5, breaks=seq(0,100, by=5), aes(y = ..density.., fill = XpertHistory)) + xlab("Abnormality Scores of the Deep Learning Systems") 
density <- p + facet_wrap(~DeepLearningSystem) + theme_minimal() + theme(legend.position = "top") +scale_fill_manual(values=c("#f7f7f7", "#fbb4b9", "#91003f", "#a1dab4", "#006d2c"))
density

tiff("Results/density.tif", width = 14, height = 8, units = "in", res = 100)
density
dev.off()



# New_long <- MDF_long[MDF_long$TB.Medication.History %in% "No",]
# Old_long <- MDF_long[MDF_long$TB.Medication.History %in% "Yes",]
# 
# p <- ggplot(New_long,aes(x=AbnormalityScore, fill=Xpert2Outcome_num))+ geom_histogram(position = 'stack', alpha=0.5, breaks=seq(0,100, by=5), aes(y = ..count.., fill = Xpert2Outcome_num)) + xlab("Abnormality Scores of the Deep Learning Systems") + scale_fill_discrete(name ="", labels = c("Xpert Negative", "Xpert Positive"))
# hist_New <- p + facet_wrap(~DeepLearningSystem) + theme_minimal() + theme(legend.position = "top")+ scale_color_brewer(palette="Accent")
# # + geom_density(alpha=.4)
# 
# tiff("Results/Histogram New.tif", width = 14, height = 8, units = "in", res = 100)
# hist_New
# dev.off()

# 
# p <- ggplot(Old_long,aes(x=AbnormalityScore, fill=Xpert2Outcome_num))+ geom_histogram(position = 'stack', alpha=0.5, breaks=seq(0,100, by=5), aes(y = ..count.., fill = Xpert2Outcome_num)) + xlab("Abnormality Scores of the Deep Learning Systems") + scale_fill_discrete(name ="", labels = c("Xpert Negative", "Xpert Positive"))
# hist_Old <- p + facet_wrap(~DeepLearningSystem) + theme_minimal() + theme(legend.position = "top")+ scale_color_brewer(palette="Accent")
# # + geom_density(alpha=.4)
# 
# tiff("Results/Histogram Old.tif", width = 14, height = 8, units = "in", res = 100)
# hist_Old
# dev.off()

# print("Xpert Positive vs Negative")
# paste(colnames(MDF)[27], ": ",round(skewness(MDF[MDF$Xpert2Outcome_num ==1, 27]),4), "vs", round(skewness(MDF[MDF$Xpert2Outcome_num ==0, 27]),4))
# paste(colnames(MDF)[28], ": ",round(skewness(MDF[MDF$Xpert2Outcome_num ==1, 28]),4), "vs", round(skewness(MDF[MDF$Xpert2Outcome_num ==0, 28]),4))
# paste(colnames(MDF)[29], ": ",round(skewness(MDF[MDF$Xpert2Outcome_num ==1, 29]),4), "vs", round(skewness(MDF[MDF$Xpert2Outcome_num ==0, 29]),4))
# paste(colnames(MDF)[30], ": ",round(skewness(MDF[MDF$Xpert2Outcome_num ==1, 30]),4), "vs", round(skewness(MDF[MDF$Xpert2Outcome_num ==0, 30]),4))
# paste(colnames(MDF)[31], ": ",round(skewness(MDF[MDF$Xpert2Outcome_num ==1, 31]),4), "vs", round(skewness(MDF[MDF$Xpert2Outcome_num ==0, 31]),4))
# paste(colnames(MDF)[32], ": ",round(skewness(MDF[MDF$Xpert2Outcome_num ==1, 32]),4), "vs", round(skewness(MDF[MDF$Xpert2Outcome_num ==0, 32]),4))
# paste(colnames(MDF)[33], ": ",round(skewness(MDF[MDF$Xpert2Outcome_num ==1, 33]),4), "vs", round(skewness(MDF[MDF$Xpert2Outcome_num ==0, 33]),4))

# Mode(MDF[MDF$Xpert2Outcome_num ==1, 28])