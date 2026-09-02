# KEGG enrichment analysis visualization
# AR-R3

library(readxl)
library(dplyr)
library(ggplot2)

# Read KEGG enrichment results
data <- read_excel("AR-R3_KEGG_enrichment_FDR.xlsx")

# Standardize column names
colnames(data)[colnames(data) == "Pathway Description"] <- "Pathway"
colnames(data)[colnames(data) == "Enrich Factor"] <- "EnrichFactor"
colnames(data)[colnames(data) == "P_adjust"] <- "FDR"

# Select the top 20 pathways according to FDR
plot_data <- data %>%
  filter(!is.na(FDR)) %>%
  arrange(FDR) %>%
  slice_head(n = 20) %>%
  mutate(
    negLogFDR = -log10(FDR),
    Pathway = factor(
      Pathway,
      levels = rev(Pathway)
    )
  )

# Generate KEGG enrichment dot plot
p <- ggplot(
  plot_data,
  aes(
    x = EnrichFactor,
    y = Pathway
  )
) +
  geom_point(
    aes(
      size = Num,
      color = negLogFDR
    ),
    alpha = 0.9
  ) +
  scale_size_continuous(
    name = "Number",
    range = c(3, 10)
  ) +
  scale_color_gradient(
    name = expression(-log[10](FDR)),
    low = "blue",
    high = "red"
  ) +
  labs(
    title = "KEGG Enrichment Analysis",
    x = "Enrichment Factor",
    y = NULL
  ) +
  theme_classic() +
  theme(
    axis.text.y = element_text(
      size = 11,
      color = "black"
    ),
    axis.text.x = element_text(
      size = 10,
      color = "black"
    ),
    axis.title.x = element_text(
      size = 12
    ),
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 9)
  )

print(p)
