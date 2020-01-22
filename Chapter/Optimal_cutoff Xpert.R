
library(OptimalCutpoints)

BestCutoff <- optimal.cutpoints(X = "AbnormalityScore", status = "Xpert2Outcome_num", tag.healthy = 0, methods = "ROC01", data = MDF_long, pop.prev = NULL, categorical.cov = "DeepLearningSystem", control = control.cutpoints(valueSe=0.95, maxSp=TRUE), ci.fit = FALSE, conf.level = 0.95, trace = FALSE)
summary(BestCutoff)

BestCutoff <- optimal.cutpoints(X = "AbnormalityScore", status = "Xpert2Outcome_num", tag.healthy = 0, methods = "MinValueSe", data = MDF_long, pop.prev = NULL, categorical.cov = "DeepLearningSystem", control = control.cutpoints(valueSe=0.98, maxSp=TRUE), ci.fit = FALSE, conf.level = 0.95, trace = FALSE)
summary(BestCutoff)

# plot(BestCutoff)
rm(BestCutoff)
