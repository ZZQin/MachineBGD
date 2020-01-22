```


library(caTools)
# set.seed(88)
# split <- sample.split(MDF$Xpert2Outcome_num, SplitRatio = 0.75)
# #get training and test data
# MDF_train <- subset(MDF, split == TRUE)
# MDF_test <- subset(MDF, split == FALSE)
# NC.reg <- MDF
# NC.reg <- subset(MDF, MDF$Country %in% "Nepal")


model.null = glm(Xpert2Outcome_num ~ 1, 
                 data=NC.reg,
                 family = binomial(link="logit")
)

model.full <- glm (Xpert2Outcome_num ~ Age + Sex + Cough + Fever + WeightLoss + NightSweat + highLunit + TBHistory, 
                   data = NC.reg, 
                   family = binomial(link="logit"))


step(model.null,
     scope = list(upper=model.full),
     direction="both",
     test="Chisq",
     data=NC.reg)

final.model <- logistic.regression.or.ci(glm(formula = Xpert2Outcome_num ~ highLunit + Age + Cough + Sex + WeightLoss + Fever, family = binomial(link = "logit"), data = NC.reg))

adjOR <- final.model$OR.ci
adjOR <- round(cbind(final.model$OR, adjOR), 2)
adjOR
# OR <- paste(adjOR[,1], "(", adjOR[,2], "-", adjOR[,3], ")")
