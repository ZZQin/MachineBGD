# Extract missing (9k) from delft 
# 01.May.2019
glimpse(CAD6_delft)

# Filename in delft and also in the missing_9k

fileExtract <- CAD6_delft[CAD6_delft$TID_Delft %in% Missing$TID_OMRS, 1]
# View(fileExtract)
fileExtract <- as.data.frame(fileExtract)
# write.csv(fileExtract, "fileExtractDelft.csv")

write.table(fileExtract, sep = "\t", file = "fileExtract.txt", row.names = FALSE)
