
library(readxl)
library(dplyr)
library(tibble)
library(pheatmap)
gene_df <- read_excel("AR-Rtop30gene.xlsx", sheet = 1)
gene_sample_cols <- c("D17", "D19", "D18", "D20", "D3", "D2", "D1", "D4")

gene_mat <- gene_df %>%
  dplyr::select(gene_name, all_of(gene_sample_cols)) %>%
  column_to_rownames("gene_name") %>%
  as.matrix()

dim(gene_mat)  # 应为 30 x 8

metab_df <- read_excel("AR-Rtop30met.xlsx", sheet = 1)


metab_sample_cols <- c("D1", "D2", "D3", "D4", "D17", "D18", "D19", "D20")


metab_mat <- metab_df %>%
  dplyr::select(Metabolite, all_of(metab_sample_cols)) %>%
  column_to_rownames("Metabolite") %>%
  as.matrix()



dim(metab_mat) 

gene_mat <- gene_mat[, metab_sample_cols]


colnames(gene_mat)
colnames(metab_mat)

gene_mat_log <- log2(gene_mat + 1)

gene_use <- gene_mat_log
metab_use <- metab_mat

cor_matrix <- cor(t(gene_use), t(metab_use), method = "spearman")

dim(cor_matrix)  
color_palette <- colorRampPalette(c("navy", "white", "firebrick3"))(100)
pheatmap(cor_matrix,
         main = "Gene-Metabolite Spearman Correlation (AR-R Top30)",
         color = color_palette,
         breaks = seq(-1, 1, length.out = 101),
         fontsize_row = 10,
         fontsize_col = 10,
         cluster_rows = TRUE,
         cluster_cols = TRUE,
         show_rownames = TRUE,
         show_colnames = TRUE,
         width = 12,
         height = 10)
