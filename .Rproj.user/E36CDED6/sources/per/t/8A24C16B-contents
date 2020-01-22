library(readxl)
Bangladesh_8k <- read_excel("C:/Users/zzq/Downloads/Bangladesh_8k.xlsx")

## both-direction stepwise algorithsmm based on AIC to select the model  
logistic.regression.or.ci <- function(regress.out, level=0.95)
{
  ################################################################
  #                                                              #
  #  This function takes the output from a glm                   #
  #  (logistic model) command in R and provides not              #
  #  only the usual output from the summary command, but         #
  #  adds confidence intervals for all coefficients and OR's.    #
  #                                                              #
  #  This version accommodates multiple regression parameters    #
  #                                                              #
  ################################################################
  usual.output <- summary(regress.out)
  z.quantile <- qnorm(1-(1-level)/2)
  number.vars <- length(regress.out$coefficients)
  OR <- exp(regress.out$coefficients[-1])
  temp.store.result <- matrix(rep(NA, number.vars*2), nrow=number.vars)
  for(i in 1:number.vars)
  {
    temp.store.result[i,] <- summary(regress.out)$coefficients[i] +
      c(-1, 1) * z.quantile * summary(regress.out)$coefficients[i+number.vars]
  }
  intercept.ci <- temp.store.result[1,]
  slopes.ci <- temp.store.result[-1,]
  OR.ci <- exp(slopes.ci)
  output <- list(regression.table = usual.output, intercept.ci = intercept.ci,
                 slopes.ci = slopes.ci, OR=OR, OR.ci = OR.ci)
  return(output)
}


MDF$Xpert2Outcome_num <- as.factor(MDF$Xpert2Outcome_num)
# ggplot(MDF, aes(x=Age, color=Xpert2Outcome_num)) + geom_histogram(position = 'identity', alpha=0.5, binwidth = 10, aes (y = ..density.., fill = Xpert2Outcome_num)) 

## Missed by CAR ###
MDF$CADMissed[MDF$highCAD == 0 & MDF$Xpert2Outcome_num %in% "1"] <- "Missed by CAD6" 
MDF$LunitMissed[MDF$highLunit == 0  & MDF$Xpert2Outcome_num %in% "1"] <- "Missed by Lunit"  
MDF$qXRMissed[MDF$HighqXR == 0  & MDF$Xpert2Outcome_num %in% "1"] <- "Missed by qXR"
MDF$CADMissed <- as.factor(MDF$CADMissed)
MDF$LunitMissed <- as.factor(MDF$LunitMissed)
MDF$qXRMissed <- as.factor(MDF$qXRMissed)

summary(MDF[, c(39:41)])
# View(MDF[, c(1, 19, 27:29, 31,33, 38:41)])
# View(MDF[complete.cases(MDF[, 39:41]),  c(1, 19, 27:29, 31,33, 38:41)])
# View(MDF[complete.cases(MDF[, 39]),  c(1, 19, 27:29, 31,33, 38:41)])
# View(MDF[complete.cases(MDF[, 40]),  c(1, 19, 27:29, 31,33, 38:41)])
# View(MDF[complete.cases(MDF[, 41]),  c(1, 19, 27:29, 31,33, 38:41)])
MDF$temp <- paste(MDF$CAD6CADMissed64, MDF$LunitMissed, MDF$qXRMissed)
(MDF[order(MDF$temp),  c(1, 19, 27:29, 31,33, 38:45)])




### Final multiple logistic regression model
# rm(df)
NC.reg <- MDF
# NC.reg <- subset(MDF, MDF$Country %in% "Cameroon")
model.uni <- glm(formula = Xpert2Outcome_num ~  Age, family = binomial(link = "logit"), data = NC.reg)
Age <- paste(round(logistic.regression.or.ci(model.uni)$OR, 2), "(", round(logistic.regression.or.ci(model.uni)$OR.ci[1], 2), "-", round(logistic.regression.or.ci(model.uni)$OR.ci[2], 2), ")")

model.uni <- glm(formula = Xpert2Outcome_num ~  Sex, family = binomial(link = "logit"), data = NC.reg)
Sex <- paste(round(logistic.regression.or.ci(model.uni)$OR, 2), "(", round(logistic.regression.or.ci(model.uni)$OR.ci[1], 2), "-", round(logistic.regression.or.ci(model.uni)$OR.ci[2], 2), ")")

model.uni <- glm(formula = Xpert2Outcome_num ~  Cough, family = binomial(link = "logit"), data = NC.reg)
Cough <- paste(round(logistic.regression.or.ci(model.uni)$OR, 2), "(", round(logistic.regression.or.ci(model.uni)$OR.ci[1], 2), "-", round(logistic.regression.or.ci(model.uni)$OR.ci[2], 2), ")")

model.uni <- glm(formula = Xpert2Outcome_num ~  Fever, family = binomial(link = "logit"), data = NC.reg)
Fever <- paste(round(logistic.regression.or.ci(model.uni)$OR, 2), "(", round(logistic.regression.or.ci(model.uni)$OR.ci[1], 2), "-", round(logistic.regression.or.ci(model.uni)$OR.ci[2], 2), ")")

model.uni <- glm(formula = Xpert2Outcome_num ~  WeightLoss, family = binomial(link = "logit"), data = NC.reg)
WeightLoss <- paste(round(logistic.regression.or.ci(model.uni)$OR, 2), "(", round(logistic.regression.or.ci(model.uni)$OR.ci[1], 2), "-", round(logistic.regression.or.ci(model.uni)$OR.ci[2], 2), ")")


model.uni <- glm(formula = Xpert2Outcome_num ~  NightSweat, family = binomial(link = "logit"), data = NC.reg)
NightSweat <- paste(round(logistic.regression.or.ci(model.uni)$OR, 2), "(", round(logistic.regression.or.ci(model.uni)$OR.ci[1], 2), "-", round(logistic.regression.or.ci(model.uni)$OR.ci[2], 2), ")")

model.uni <- glm(formula = Xpert2Outcome_num ~  TBHistory, family = binomial(link = "logit"), data = NC.reg)
TBHistory <- paste(round(logistic.regression.or.ci(model.uni)$OR, 2), "(", round(logistic.regression.or.ci(model.uni)$OR.ci[1], 2), "-", round(logistic.regression.or.ci(model.uni)$OR.ci[2], 2), ")")

model.uni <- glm(formula = Xpert2Outcome_num ~  highCAD, family = binomial(link = "logit"), data = NC.reg)
highCAD <- paste(round(logistic.regression.or.ci(model.uni)$OR, 2), "(", round(logistic.regression.or.ci(model.uni)$OR.ci[1], 2), "-", round(logistic.regression.or.ci(model.uni)$OR.ci[2], 2), ")")

model.uni <- glm(formula = Xpert2Outcome_num ~  highLunit, family = binomial(link = "logit"), data = NC.reg)
highLunit <- paste(round(logistic.regression.or.ci(model.uni)$OR, 2), "(", round(logistic.regression.or.ci(model.uni)$OR.ci[1], 2), "-", round(logistic.regression.or.ci(model.uni)$OR.ci[2], 2), ")")

model.uni <- glm(formula = Xpert2Outcome_num ~  HighqXR, family = binomial(link = "logit"), data = NC.reg)
HighqXR <- paste(round(logistic.regression.or.ci(model.uni)$OR, 2), "(", round(logistic.regression.or.ci(model.uni)$OR.ci[1], 2), "-", round(logistic.regression.or.ci(model.uni)$OR.ci[2], 2), ")")

df <- rbind(Age, Sex, Cough, Fever, WeightLoss, NightSweat, TBHistory, highCAD, highLunit, HighqXR)
View(df[,])
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

