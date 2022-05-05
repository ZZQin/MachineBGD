source("2.0 Version Comparison/radiologist.R")

table(MDF_long$DeepLearningSystem)
MDF_long$DeepLearningSystem <- as.character(MDF_long$DeepLearningSystem)


# MDF_long$DeepLearningSystem[grep("CAD4TB", MDF_long$DeepLearningSystem)] <- "P1"
# MDF_long$DeepLearningSystem[grep("qXR", MDF_long$DeepLearningSystem)] <- "P3"
# MDF_long$DeepLearningSystem[grep("Lunit INSIGHT CXR", MDF_long$DeepLearningSystem)] <- "P2"

### mean of the difference and t-test for paird data
MDF$diff_cad <- MDF$CAD4TBv7-MDF$CAD4TBv6
MDF$diff_qXR <- MDF$qXRv3-MDF$qXRv2
summary(MDF$diff_cad)
summary(MDF$diff_qXR)

# CAD4TB: Save the data in two different vector
before <-  MDF$CAD4TBv6
after <- MDF$CAD4TBv7
# Compute t-test
res <- t.test(after, before, paired = TRUE,
              alternative = "less")
res

# qXR: Save the data in two different vector
before <-  MDF$qXRv2
after <- MDF$qXRv3
# Compute t-test
res <- t.test(after, before, paired = TRUE,
              alternative = "less")
res


#define quantiles of interest
q = c(.1, .9)

#calculate quantiles by grouping variable
MDF_long%>%
  group_by(DeepLearningSystem, GXP.Result) %>%
  summarize(quant10 = quantile(AbnormalityScore, probs = q[1]), 
            quant90 = quantile(AbnormalityScore, probs = q[2]))


## Histogram
p <- ggplot(MDF_long,aes(x=AbnormalityScore, fill=XpertHistory,))+ geom_histogram(position = 'stack', alpha=0.5, breaks=seq(0,100, by=5), aes(y = ..count.., fill = XpertHistory)) + xlab("Abnormality Scores of the Deep Learning Systems") 
hist <- p + facet_wrap(~DeepLearningSystem) + theme_minimal() + theme(legend.position = "top") +scale_fill_manual(values=c("#f7f7f7", "#fbb4b9", "#91003f", "#a1dab4", "#006d2c"))


tiff("2.0 Version Comparison//Histogram.tif", width = 14, height = 8, units = "in", res = 100)
hist
dev.off()

# facet by AI
p <- ggplot(MDF_long,aes(x=AbnormalityScore, fill=Xpert2Outcome_num,))+ geom_histogram(position = 'stack', alpha=0.5, breaks=seq(0,100, by=5), aes(y = ..count.., fill = Xpert2Outcome_num)) + xlab("Abnormality Scores of the Deep Learning Systems") 
hist <- p + facet_wrap(~DeepLearningSystem) + theme_minimal() + theme(legend.position = "top") +scale_fill_manual(values=c("#91003f", "#a1dab4"))
hist

# color of different AI
MDF_long_CAD <- MDF_long[MDF_long$DeepLearningSystem %in% (c("CAD4TBv6", "CAD4TBv7")), ]
p <- ggplot(MDF_long_CAD,aes(x=AbnormalityScore, fill=DeepLearningSystem,))+ geom_histogram(position = 'identity', alpha=0.5, breaks=seq(0,100, by=5), aes(y = ..count.., fill = DeepLearningSystem)) + xlab("Abnormality Scores of the Deep Learning Systems") 
hist <- p + facet_wrap(~GXP.Result) + theme_minimal() + theme(legend.position = "top") +scale_fill_manual(values=c("#2166ac", "#ef8a62"))
hist

MDF_long_qXR <- MDF_long[MDF_long$DeepLearningSystem %in% (c("qXRv2", "qXRv3")), ]
p <- ggplot(MDF_long_qXR,aes(x=AbnormalityScore, fill=DeepLearningSystem,))+ geom_histogram(position = 'identity', alpha=0.5, breaks=seq(0,100, by=5), aes(y = ..count.., fill = DeepLearningSystem)) + xlab("Abnormality Scores of the Deep Learning Systems") 
hist <- p + facet_wrap(~GXP.Result) + theme_minimal() + theme(legend.position = "top") +scale_fill_manual(values=c("#2166ac", "#ef8a62"))
hist








p <- ggplot(MDF_long,aes(x=AbnormalityScore, fill=Xpert2Outcome_num,))+ geom_histogram(position = 'stack', alpha=0.5, breaks=seq(0,100, by=5), aes(y = ..count.., fill = Xpert2Outcome_num)) + xlab("Abnormality Scores of the Deep Learning Systems") 
hist <- p + facet_wrap(~DeepLearningSystem) + theme_minimal() + theme(legend.position = "top") +scale_fill_manual(values=c("#91003f", "#a1dab4"))
hist


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
