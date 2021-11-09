source("2.0 Version Comparison/radiologist.R")

table(MDF_long$DeepLearningSystem)
MDF_long$DeepLearningSystem <- as.character(MDF_long$DeepLearningSystem)

# MDF_long$DeepLearningSystem[grep("CAD4TB", MDF_long$DeepLearningSystem)] <- "P1"
# MDF_long$DeepLearningSystem[grep("qXR", MDF_long$DeepLearningSystem)] <- "P3"
# MDF_long$DeepLearningSystem[grep("Lunit INSIGHT CXR", MDF_long$DeepLearningSystem)] <- "P2"




## Histogram
p <- ggplot(MDF_long,aes(x=AbnormalityScore, fill=XpertHistory,))+ geom_histogram(position = 'stack', alpha=0.5, breaks=seq(0,100, by=5), aes(y = ..count.., fill = XpertHistory)) + xlab("Abnormality Scores of the Deep Learning Systems") 
hist <- p + facet_wrap(~DeepLearningSystem) + theme_minimal() + theme(legend.position = "top") +scale_fill_manual(values=c("#f7f7f7", "#fbb4b9", "#91003f", "#a1dab4", "#006d2c"))


tiff("2.0 Version Comparison//Histogram.tif", width = 14, height = 8, units = "in", res = 100)
hist
dev.off()

## Density plot
p <- ggplot(MDF_long,aes(x=AbnormalityScore, fill=XpertHistory,))+ geom_histogram(position = 'stack', alpha=0.5, breaks=seq(0,100, by=5), aes(y = ..density.., fill = XpertHistory)) + xlab("Abnormality Scores of the AI products") 
density <- p + facet_wrap(~DeepLearningSystem) + theme_minimal() + theme(legend.position = "bottom") +scale_fill_manual(values=c("#f7f7f7", "#fbb4b9", "#91003f", "#a1dab4", "#006d2c"))+ scale_x_continuous(minor_breaks = seq(0 , 100, 10), breaks = seq(0 , 100, 10)) 
density

tiff("2.0 Version Comparison/density.tif", width = 10, height = 6, units = "in", res = 100)
density
dev.off()



# New_long <- MDF_long[MDF_long$TB.Medication.History %in% "No",]
# Old_long <- MDF_long[MDF_long$TB.Medication.History %in% "Yes",]
# 
# p <- ggplot(New_long,aes(x=AbnormalityScore, fill=Xpert2Outcome_num))+ geom_histogram(position = 'stack', alpha=0.5, breaks=seq(0,100, by=5), aes(y = ..count.., fill = Xpert2Outcome_num)) + xlab("Abnormality Scores of the Deep Learning Systems") + scale_fill_discrete(name ="", labels = c("Xpert Negative", "Xpert Positive"))
# hist_New <- p + facet_wrap(~DeepLearningSystem) + theme_minimal() + theme(legend.position = "top")+ scale_color_brewer(palette="Accent")
# # + geom_density(alpha=.4)
# 
# tiff("2.0 Version Comparison/Histogram New.tif", width = 14, height = 8, units = "in", res = 100)
# hist_New
# dev.off()

# 
# p <- ggplot(Old_long,aes(x=AbnormalityScore, fill=Xpert2Outcome_num))+ geom_histogram(position = 'stack', alpha=0.5, breaks=seq(0,100, by=5), aes(y = ..count.., fill = Xpert2Outcome_num)) + xlab("Abnormality Scores of the Deep Learning Systems") + scale_fill_discrete(name ="", labels = c("Xpert Negative", "Xpert Positive"))
# hist_Old <- p + facet_wrap(~DeepLearningSystem) + theme_minimal() + theme(legend.position = "top")+ scale_color_brewer(palette="Accent")
# # + geom_density(alpha=.4)
# 
# tiff("2.0 Version Comparison/Histogram Old.tif", width = 14, height = 8, units = "in", res = 100)
# hist_Old
# dev.off()

library(moments)
library(scales)

print("Xpert Positive vs Negative")
paste(colnames(MDF)[27], ": ",round(skewness(MDF[MDF$Xpert2Outcome_num ==1, 27]),4), "vs", round(skewness(MDF[MDF$Xpert2Outcome_num ==0, 27]),4))
paste(colnames(MDF)[28], ": ",round(skewness(MDF[MDF$Xpert2Outcome_num ==1, 28]),4), "vs", round(skewness(MDF[MDF$Xpert2Outcome_num ==0, 28]),4))
paste(colnames(MDF)[29], ": ",round(skewness(MDF[MDF$Xpert2Outcome_num ==1, 29]),4), "vs", round(skewness(MDF[MDF$Xpert2Outcome_num ==0, 29]),4))
paste(colnames(MDF)[30], ": ",round(skewness(MDF[MDF$Xpert2Outcome_num ==1, 30]),4), "vs", round(skewness(MDF[MDF$Xpert2Outcome_num ==0, 30]),4))
paste(colnames(MDF)[31], ": ",round(skewness(MDF[MDF$Xpert2Outcome_num ==1, 31]),4), "vs", round(skewness(MDF[MDF$Xpert2Outcome_num ==0, 31]),4))
paste(colnames(MDF)[32], ": ",round(skewness(MDF[MDF$Xpert2Outcome_num ==1, 32]),4), "vs", round(skewness(MDF[MDF$Xpert2Outcome_num ==0, 32]),4))
paste(colnames(MDF)[23], ": ",round(skewness(MDF[MDF$Xpert2Outcome_num ==1, 23]),4), "vs", round(skewness(MDF[MDF$Xpert2Outcome_num ==0, 23]),4))

# Mode(MDF[MDF$Xpert2Outcome_num ==1, 28])




### Box plot 
MDF_long$DeepLearningSystem <- as.factor(MDF_long$DeepLearningSystem )
MDF_long <- MDF_long[! MDF_long$XpertHistory=="", ]
BoxPlot <- ggplot(MDF_long, aes(x=AbnormalityScore, y=XpertHistory, fill=XpertHistory)) + 
  geom_boxplot(alpha=0.6) +
  theme_minimal() +
  facet_wrap(~DeepLearningSystem) +
  scale_fill_manual(values=c("#a1dab4", "#006d2c", "#fbb4b9", "#91003f"))+
  theme(legend.position = "none", legend.title= element_blank()) + scale_x_continuous(minor_breaks = seq(0 , 100, 5), breaks = seq(0 ,100, 10))
BoxPlot

tiff("2.0 Version Comparison/BoxPlot.tif", width = 8, height = 3, units = "in", res = 100)
BoxPlot
dev.off()
