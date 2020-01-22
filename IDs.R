BGD.PID.TID <- read.csv(file = "./Bangladesh PID_TID.csv", header=T, sep=",")

summary(BGD.PID.TID)
BGD.PID.TID$PID[BGD.PID.TID$Test.ID..DICOM.IMAGE.ID. %in% "#N/A"]
BGD.PID.TID$PID[BGD.PID.TID$Test.ID..DICOM.IMAGE.ID. %in% "2010100001-4"]
