setwd() 

diff_genes_data <- read_excel("AR-R_vs_BAR-R.xlsx")
log2FC_threshold <- 1 
padj_threshold <- 0.05 

diff_genes_data <- diff_genes_data %>%
  mutate(
    expression = case_when(
      `logFC(G1/G6)` >= log2FC_threshold & FDR <= padj_threshold ~ "Up-regulated",
      `logFC(G1/G6)` <= -log2FC_threshold& FDR <= padj_threshold ~ "Down-regulated", 
      TRUE ~ "Not significant"
    )
  )
de_stats <- table(diff_genes_data$expression)
print(de_stats)

volcano_plot <- ggplot(diff_genes_data, aes(x =  `logFC(AR-R/BAR-R)`, y = -log10(FDR))) +
  geom_point(aes(color = expression, alpha = expression), size = 2) +
  scale_color_manual(
    values = c(
      "Up-regulated" = "#E64B35",    
      "Down-regulated" = "#3182BD",   
      "Not significant" = "#BDBDBD"   
    )
  ) +
  scale_alpha_manual(
    values = c(
      "Up-regulated" = 0.8,
      "Down-regulated" = 0.8,
      "Not significant" = 0.4
    )
  ) +
 
  geom_vline(xintercept = c(-log2FC_threshold, log2FC_threshold), 
             linetype = "dashed", color = "black", alpha = 0.5) +
  geom_hline(yintercept = -log10(padj_threshold), 
             linetype = "dashed", color = "black", alpha = 0.5) +

  labs(
    title = "Differential Gene Expression Volcano Plot",
    subtitle = paste("Up-regulated:", de_stats["Up-regulated"], 
                     "| Down-regulated:", de_stats["Down-regulated"]),
    x = "log2(Fold Change)",
    y = "-log10(Adjusted p-value)",
    color = "Expression"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
    plot.subtitle = element_text(hjust = 0.5, size = 10),
    legend.position = "bottom",
    panel.grid.major = element_line(color = "grey90"),
    panel.grid.minor = element_blank()
  )
print(volcano_plot)
