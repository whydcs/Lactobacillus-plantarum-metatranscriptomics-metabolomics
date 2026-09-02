
Transcriptome enrichment results
             +
Metabolome enrichment results
             ↓
      shared pathways
             ↓
  -log10(P value) + pathway counts
             ↓
   Transcriptome vs Metabolome
             ↓
      butterfly plot

library(writexl) 
library(ggplot2)
library(dplyr)
library(tidyr)
library(forcats)
library(readxl)
enrichment_data <- read_excel("AR-Rmetgenebutterfly.xlsx")
plot_data <- enrichment_data %>%
 
  mutate(
    nlog10_p_transcriptome = -log10(pvalue_transcriptome),
    nlog10_p_metabolome = -log10(pvalue_metabolome)
  ) %>%

  arrange(desc(nlog10_p_transcriptome)) %>%

  pivot_longer(
    cols = c(nlog10_p_transcriptome, nlog10_p_metabolome,
             count_transcriptome, count_metabolome),
    names_to = "variable",
    values_to = "value"
  ) %>%

  mutate(
    type = ifelse(grepl("nlog10", variable), "significance", "count"),
    omics = ifelse(grepl("transcriptome", variable), "Transcriptome", "Metabolome")
  ) %>%
  select(-variable) %>%
  pivot_wider(
    names_from = type,
    values_from = value
  ) %>%
 
  mutate(pathway = fct_reorder(pathway, significance, .fun = max))
omics_colors <- c("Transcriptome" = "#1f77b4", "Metabolome" = "#ff7f0e")
max_significance <- max(plot_data$significance, na.rm = TRUE)
max_count <- max(plot_data$count, na.rm = TRUE)
p <- ggplot(plot_data, aes(x = pathway)) +
  

  geom_col(aes(y = significance, fill = omics),
           position = position_dodge(width = 0.7),
           width = 0.6,
           alpha = 0.8) +
  

  geom_col(aes(y = -count, fill = omics),
           position = position_dodge(width = 0.7),
           width = 0.6,
           alpha = 0.8) +
  

  coord_flip() +
  
 
  scale_fill_manual(values = omics_colors)+
  
 
  scale_y_continuous(
    name = "-log10(Pvalue)",
    sec.axis = sec_axis(~ -. , name = "Number"))+
  

  theme_minimal(base_size = 12) +
  theme(
    axis.title.y = element_blank(),
    axis.title.x = element_text(size = 11, face = "bold", color = "black"),
    axis.title.x.top = element_text(size = 11, face = "bold", color = "black"),
    axis.text = element_text(size = 10, color = "black"),
    axis.text.y = element_text(size = 9),
    legend.position = "top",
    legend.title = element_blank(),
    legend.text = element_text(size = 10),
    panel.grid.major = element_line(color = "grey90"),
    panel.grid.minor = element_blank(),
    plot.margin = margin(1, 1, 1, 1, "cm")
  )
