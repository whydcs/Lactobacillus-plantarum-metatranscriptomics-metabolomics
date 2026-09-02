# Lactobacillus-plantarum-metatranscriptomics-metabolomics
Code and analysis workflows for the manuscript on Lactobacillus plantarum mono-colonization in germ-free mice.
library(readxl)
library(dplyr)
library(tidyr)
library(igraph)
library(ggraph)
library(ggplot2)

# Function for metabolite-pathway network construction
create_network <- function(input_file, output_file = NULL) {
  
  df <- read_excel(input_file)
  
  df_sig <- df %>%
    filter(P_adjust < 0.05)
  
  df_top <- df_sig %>%
    arrange(P_adjust) %>%
    slice_head(n = 10)
  
  network_long <- df_top %>%
    separate_rows(`代谢物(|分割)`, sep = "\\|") %>%
    rename(
      Metabolite = `代谢物(|分割)`,
      Pathway = `Pathway Description`
    )
  
  pathway_nodes <- network_long %>%
    distinct(Pathway) %>%
    left_join(
      df_top %>%
        select(
          Pathway = `Pathway Description`,
          P_adjust,
          Enrich_Factor = `Enrich Factor`
        ),
      by = "Pathway"
    ) %>%
    mutate(type = "Pathway")
  
  metabolite_nodes <- network_long %>%
    distinct(Metabolite) %>%
    mutate(type = "Metabolite")
  
  edges <- network_long %>%
    select(
      from = Pathway,
      to = Metabolite
    ) %>%
    distinct()
  
  nodes <- bind_rows(
    pathway_nodes %>%
      select(name = Pathway, type, P_adjust, Enrich_Factor),
    
    metabolite_nodes %>%
      select(name = Metabolite, type)
  )
  
  g <- graph_from_data_frame(
    d = edges,
    vertices = nodes,
    directed = FALSE
  )
  
  set.seed(123)
  
  p <- ggraph(g, layout = "fr") +
    geom_edge_link(
      alpha = 0.35,
      linewidth = 0.4
    ) +
    geom_node_point(
      aes(
        shape = type,
        color = type,
        size = ifelse(type == "Pathway", 8, 3)
      )
    ) +
    geom_node_text(
      aes(label = name),
      repel = TRUE,
      size = 3
    ) +
    scale_shape_manual(
      values = c(
        Pathway = 16,
        Metabolite = 15
      )
    ) +
    scale_color_manual(
      values = c(
        Pathway = "orange",
        Metabolite = "green"
      )
    ) +
    scale_size_continuous(
      range = c(3, 10)
    ) +
    theme_void() +
    theme(
      legend.position = "right"
    )
  
  print(p)
  
  if (!is.null(output_file)) {
    ggsave(
      output_file,
      plot = p,
      width = 10,
      height = 8,
      dpi = 600
    )
  }
}
# AR-R3
create_network(
  "AR-R3_KEGG_enrichment.xlsx",
  "AR-R3_metabolite_pathway_network.pdf"
)

# AR-R
create_network(
  "AR-R_KEGG_enrichment.xlsx",
  "AR-R_metabolite_pathway_network.pdf"
)


