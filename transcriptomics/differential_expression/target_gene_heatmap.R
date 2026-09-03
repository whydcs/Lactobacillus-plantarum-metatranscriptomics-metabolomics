library(readxl)
library(dplyr)
library(tibble)
library(pheatmap)
genelist <- read_excel("AR-RbeforeAR-R.diff.exp.anno.xlsx")
targetgene <- read_excel("AR-Rtargetgene.xlsx")
filtered_data <- genelist[genelist$seq_id %in% targetgene$seq_id, ]
new_filtered_data <- filtered_data[, c(2:9,32)]
library(writexl)
#write_xlsx(new_filtered_data,path = "AR-Rheatmapgene.xlsx")#

data <- read_excel("AR-Rheatmapgene.xlsx")
GROUP=c(rep("beforer AR-R",4),rep("after AR-R",4))
annotation_c <- data.frame(GROUP)
union_unique1 <- column_to_rownames(data,var="gene_name")
library(pheatmap)
rownames(annotation_c) <- colnames(union_unique1)
ann_colors <- list(Group = c(Group1="#1B9E77", Group2="#D95F02"))
pheatmap(union_unique1,
         main = "Gene",
         color = colorRampPalette(c("navy", "white", "firebrick3"))(50), 
         scale = "row",        
         cluster_rows = TRUE,  
         cluster_cols = TRUE, 
         show_rownames = TRUE,
         show_colnames = FALSE,
         annotation_col = annotation_c,
         annotation_colors = ann_colors,
         fontsize_row = 8,
         border_color = NA)
