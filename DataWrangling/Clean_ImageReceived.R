### Clean Image Received
### Image Received ###
ReceviedIndex <- read.csv(file = "Image index received.csv")
colnames(ReceviedIndex)[1] <- "TID"
ReceviedIndex$TID <- as.character(ReceviedIndex$TID)
ReceviedIndex$FileFolder <- paste(ReceviedIndex$TID, ReceviedIndex$Folder, sep = "_")

# File Received
ReceviedIndex$Center <- 0 
ReceviedIndex$Center[ReceviedIndex$Folder %in% "1st Batch\\Dhanmondi_TBSTC"] <- "Dhanmondi"
ReceviedIndex$Center[ReceviedIndex$Folder %in% "1st Batch\\Golapbagh_TBSTC"] <- "Golapbagh"
ReceviedIndex$Center[ReceviedIndex$Folder %in% "1st Batch\\Mohakhali_TBSTC"] <- "Mohakhali"
ReceviedIndex$Center[ReceviedIndex$Folder %in% "2nd Batch\\Dhanmondi2"] <- "Dhanmondi"
ReceviedIndex$Center[ReceviedIndex$Folder %in% "2nd Batch\\Golapbagh2"] <- "Golapbagh"
ReceviedIndex$Center[ReceviedIndex$Folder %in% "2nd Batch\\Mohakhali2"] <- "Mohakhali"

##### Image Received:: Duplicated TID ##### --> Solved
n_occur <- data.frame(table(ReceviedIndex$TID))
Multi_TID_Received <- (ReceviedIndex[ReceviedIndex$TID %in% n_occur$Var1[n_occur$Freq >1], ]) 
# Remove the duplicated records from the 2nd Batch
Uni1Batch <- Multi_TID_Received[!duplicated(Multi_TID_Received[c("TID", "Center")]), ]
TID_2ndBatch_Remove <- subset(Multi_TID_Received, !(Multi_TID_Received$FileFolder %in% Uni1Batch$FileFolder))
# write.csv(TID_2ndBatch_Remove, "TID_2ndBatch_Remove.csv")

rm(n_occur, TID_2ndBatch_Remove, Multi_TID_Received, Uni1Batch)

ReceviedIndex <- ReceviedIndex[!duplicated(ReceviedIndex["TID"]), ]
BGD8168 <- read.delim("BGD8168List.txt", header = FALSE, sep = " ")
BGD8168 <- as.data.frame(BGD8168[, 13])
colnames(BGD8168)[1] <- "TID"
