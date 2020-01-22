library(scatterplot3d)
CAD_Xpert_plot <- read.csv("Results/CAD_Xpert Cutoffs TABLE 4 digits.csv")
CAD_Xpert_plot <- CAD_Xpert_plot[, -1]

CAD_Xpert_plot <- CAD_Xpert_plot[!CAD_Xpert_plot$DeepLearningSystem %in% c("IF1", "JF2", "IF2"), ]
CAD_Xpert_plot$DeepLearningSystem <- as.factor(as.character(CAD_Xpert_plot2$DeepLearningSystem))

CAD_Xpert_plot$Score[CAD_Xpert_plot$DeepLearningSystem %in% "CAD4TB"] <- CAD_Xpert_plot$Score[CAD_Xpert_plot$DeepLearningSystem %in% "CAD4TB"]/100

colors <- c("#A7226E", "#EC2049", "#2F9599", "#F7DB4F", "#56B4E9")
colors <- colors[as.numeric(CAD_Xpert_plot$DeepLearningSystem)]

# 2. Empty 3D scatter plot using pch=""
s3d <- scatterplot3d(CAD_Xpert_plot$Score, CAD_Xpert_plot$Sens, CAD_Xpert_plot$Spec, color = colors, grid=FALSE, box=FALSE)
legend("top", legend = levels(CAD_Xpert_plot$DeepLearningSystem),
       col =  c("#A7226E", "#EC2049", "#2F9599", "#F7DB4F", "#56B4E9"), pch = 16,  inset = -0.1, xpd = TRUE,horiz = TRUE)

# 3. Add grids
source("DataWrangling/addgrids3d.R")
addgrids3d(CAD_Xpert_plot$Score, CAD_Xpert_plot$Sens, CAD_Xpert_plot$Spec, grid = c("xy", "xz", "yz"))

