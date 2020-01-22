dcm_list <- read.delim2("AI Scores/dcm_list_BD.txt", header = FALSE, sep = "/")
dcm_list$V3 <- gsub(".dcm", "", dcm_list$V3)


Lunit <- read_csv("AI Scores/result.csv")
Lunit <- unique(Lunit)
Lunit <- Lunit[, -c(1, 3:8)]
colnames(Lunit)[2] <- "LunitScore"
Lunit <- Lunit[is.na(Lunit$LunitScore)==F, ]
Lunit$PatientID <- as.factor(Lunit$PatientID)

dcm_list$complete <- Lunit$LunitScore[match(dcm_list$V3, Lunit$PatientID)]
dcm_list <- dcm_list[, -c(1,2)]
dcm_list <- dcm_list[is.na(dcm_list$complete), ]
dcm_list$incomplete <- paste0("dicom_dir_BD/24K/", dcm_list$V3, ".dcm")

dcm_list <- dcm_list[, -c(1,2)]

write.table(dcm_list, file = "dcm_list_BD_IC.txt", sep = ",",row.names = FALSE, col.names = FALSE)
