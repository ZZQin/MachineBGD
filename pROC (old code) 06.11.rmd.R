```{r ROC curving, echo= T}
# ## CAD V3 score comparing with Xpert ####
# roc(Xpert2Outcome_num ~ CAD4TB3, Master_df_complete)
# ci.auc(Xpert2Outcome_num ~ CAD4TB3, Master_df_complete)
# roc_all <- roc(Master_df_complete$Xpert2Outcome_num,
#                Master_df_complete$CAD4TB3, percent=TRUE,
#                plot=TRUE, auc.polygon=FALSE, max.auc.polygon=FALSE, grid=TRUE,
#                print.auc=TRUE, show.thres=TRUE)
# 
# #### CAD V5 score (2016) comparing with Xpert ####
# roc(Xpert2Outcome_num ~ CAD4TB5.old, Master_df_complete)
# ci.auc(Xpert2Outcome_num ~ CAD4TB5.old, Master_df_complete)
# 
# roc_all <- roc(Master_df_complete$Xpert2Outcome_num,
#                Master_df_complete$CAD4TB5.old, percent=TRUE,
#                plot=TRUE, auc.polygon=FALSE, max.auc.polygon=FALSE, grid=TRUE,
#                print.auc=TRUE, show.thres=TRUE)

#### CAD V5 score (2018) comparing with Xpert ####
roc(Xpert2Outcome_num ~ CAD4TB5, Master_df_complete)
ci.auc(Xpert2Outcome_num ~ CAD4TB5, Master_df_complete)

roc_all <- roc(Master_df_complete$Xpert2Outcome_num,
               Master_df_complete$CAD4TB5, percent=TRUE,
               plot=TRUE, auc.polygon=FALSE, max.auc.polygon=FALSE, grid=TRUE,
               print.auc=TRUE, show.thres=TRUE)

#### CAD V6 score (2018) comparing with Xpert ####
roc(Xpert2Outcome_num ~ CAD4TB6, Master_df_complete)
ci.auc(Xpert2Outcome_num ~ CAD4TB6, Master_df_complete)

roc_all <- roc(Master_df_complete$Xpert2Outcome_num,
               Master_df_complete$CAD4TB6, percent=TRUE,
               plot=TRUE, auc.polygon=FALSE, max.auc.polygon=FALSE, grid=TRUE,
               print.auc=TRUE, show.thres=TRUE)

#### Qure.AI comparing with Xpert ####
roc(Xpert2Outcome_num ~ QA072018, Master_df_complete)
ci.auc(Xpert2Outcome_num ~ QA072018, Master_df_complete)

roc_all <- roc(Master_df_complete$Xpert2Outcome_num,
               Master_df_complete$QA072018, percent=TRUE,
               plot=TRUE, auc.polygon=FALSE, max.auc.polygon=FALSE, grid=TRUE,
               print.auc=TRUE, show.thres=TRUE)

# #### CAD V3 comparing with radiologist ########
# roc(radiology_dic ~ CAD4TB3, Master_df_complete)
# ci.auc(radiology_dic ~ CAD4TB3, Master_df_complete)
# 
# roc_all <- roc(Master_df_complete$radiology_dic,
#                Master_df_complete$CAD4TB3, percent=TRUE,
#                plot=TRUE, auc.polygon=FALSE, max.auc.polygon=FALSE, grid=TRUE,
#                print.auc=TRUE, show.thres=TRUE)
# 
#### CAD V5 (2018) comparing with radiologist ########
roc(radiology_dic ~ CAD4TB5, Master_df_complete)
ci.auc(radiology_dic ~ CAD4TB5, Master_df_complete)

roc_all <- roc(Master_df_complete$radiology_dic,
               Master_df_complete$CAD4TB5, percent=TRUE,
               plot=TRUE, auc.polygon=FALSE, max.auc.polygon=FALSE, grid=TRUE,
               print.auc=TRUE, show.thres=TRUE)

#### CAD V6 (2018) comparing with radiologist ########
roc(radiology_dic ~ CAD4TB6, Master_df_complete)
ci.auc(radiology_dic ~ CAD4TB6, Master_df_complete)

roc_all <- roc(Master_df_complete$radiology_dic,
               Master_df_complete$CAD4TB6, percent=TRUE,
               plot=TRUE, auc.polygon=FALSE, max.auc.polygon=FALSE, grid=TRUE,
               print.auc=TRUE, show.thres=TRUE) 

#### QureAI (2018) comparing with radiologist ########
roc(radiology_dic ~ QA072018, Master_df_complete)
ci.auc(radiology_dic ~ QA072018, Master_df_complete)

roc_all <- roc(Master_df_complete$radiology_dic,
               Master_df_complete$QA072018, percent=TRUE,
               plot=TRUE, auc.polygon=FALSE, max.auc.polygon=FALSE, grid=TRUE,
               print.auc=TRUE, show.thres=TRUE) 

# #######################
# ci.auc(radiology_dic ~ CAD4TB3, Master_df_complete)
# 
# roc_all <- roc(Master_df_complete$Xpert2Outcome_num,
#                Master_df_complete$CAD4TB3, percent=TRUE,
#                plot=TRUE, auc.polygon=FALSE, max.auc.polygon=FALSE, grid=TRUE,
#                print.auc=FALSE, show.thres=TRUE)

```


```{r all CAD scores in table (replaced by calculate_roc), eval=F}
# CAD4TB3 <- as.matrix(quantile(Master_df_complete$CAD4TB3, seq(0,1, by=0.01)))
CAD4TB <- seq(1, 100, by = 1)
CAD4TB <- as.data.frame(CAD4TB)

Xpert.test.saved <- row.names(CAD4TB3)

CAD.score <- as.numeric(CAD4TB3[,1])
table3 <- cbind(Xpert.test.saved, CAD.score)
table3 <- as.data.frame(table3)

mylist <- NULL
mylist <- as.list(mylist)

for (i in 1:101){
myfunction <- function(car.cutoff){
a <- sum(Master_df_complete$CAD4TB3 >= car.cutoff & Master_df_complete$Xpert2Outcome_num =="1")
b <- sum(Master_df_complete$CAD4TB3 >= car.cutoff & Master_df_complete$Xpert2Outcome_num =="0")
c <- sum(Master_df_complete$CAD4TB3 < car.cutoff & Master_df_complete$Xpert2Outcome_num =="1")
d <- sum(Master_df_complete$CAD4TB3 < car.cutoff & Master_df_complete$outcome_num =="0")
dat <- as.table(matrix(c(a,b,c,d), nrow = 2, byrow = TRUE))
colnames(dat) <- c("Dis+","Dis-")
rownames(dat) <- c("Test+","Test-")
rval <- epi.tests(dat, conf.level = 0.95)
sensitivity <- as.vector(round((rval$elements$sensitivity),3))
specificity <- as.vector(round((rval$elements$specificity),3))
pv.positive <- as.vector(round((rval$elements$pv.positive),3))
pv.negative <- as.vector(round((rval$elements$pv.negative),3))

perc_above_CAR_Score<- sum(Master_df_complete$CAD4TB3 >= car.cutoff)/length(Master_df_complete$ResearchID)


accuracy <- cbind(sensitivity, specificity, pv.positive, pv.negative, perc_above_CAR_Score)
row.names(accuracy) <- paste("car score=", CAD4TB[i])
colnames(accuracy)[1] <- "Sensitivity"
colnames(accuracy)[2] <- "Sensitivity (95%CI low)"
colnames(accuracy)[3] <- "Sensitivity (95%CI high)"
colnames(accuracy)[4] <- "Specificity"
colnames(accuracy)[5] <- "Specificity (95%CI low)"
colnames(accuracy)[6] <- "Specificity (95%CI high)"
colnames(accuracy)[7] <- "PPV"
colnames(accuracy)[8] <- "PPV (95%CI low)"
colnames(accuracy)[9] <- "PPV (95%CI high)"
colnames(accuracy)[10] <- "NPV"
colnames(accuracy)[11] <- "NPV (95%CI low)"
colnames(accuracy)[12] <- "NPV (95%CI high)"

return(accuracy)
}

accuracy <- myfunction(CAD4TB[i])
mylist[[i]] <- list(accuracy)

}
df <- data.frame(matrix(unlist(mylist), nrow=101, byrow=T))
colnames(df)[1] <- "Sensitivity"
colnames(df)[2] <- "Sensitivity (95%CI low)"
colnames(df)[3] <- "Sensitivity (95%CI high)"
colnames(df)[4] <- "Specificity"
colnames(df)[5] <- "Specificity (95%CI low)"
colnames(df)[6] <- "Specificity (95%CI high)"
colnames(df)[7] <- "PPV"
colnames(df)[8] <- "PPV (95%CI low)"
colnames(df)[9] <- "PPV (95%CI high)"
colnames(df)[10] <- "NPV"
colnames(df)[11] <- "NPV (95%CI low)"
colnames(df)[12] <- "NPV (95%CI high)"
```
