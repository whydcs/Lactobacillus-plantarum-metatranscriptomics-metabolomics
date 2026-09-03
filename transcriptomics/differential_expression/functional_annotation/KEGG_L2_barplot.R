library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)


KEGGL2 <- read_excel(
  "KEGG.bar.L2.percent.xlsx",
  sheet = "KEGG.bar.L2.percent"
)


KEGGL2$Pathway <- factor(
  KEGGL2$Pathway,
  levels = rev(KEGGL2$Pathway)
)


KEGGL21 <- KEGGL2 %>%
  pivot_longer(
    cols = L1:L28,
    names_to = "sample",
    values_to = "value"
  )


group <- read_excel(
  "KEGG.bar.L2.percent.xlsx",
  sheet = "Sheet1"
)


KEGGL21 <- cbind(KEGGL21, group)


# AR_R <- KEGGL21 %>% filter(group == "AR-R")
# AR_R3 <- KEGGL21 %>% filter(group == "AR-R3")

# KEGG Level 2 stacked bar plot
p <- ggplot(
  KEGGL21,
  aes(x = sample, y = value, fill = Pathway)
) +
  geom_bar(
    stat = "identity",
    position = "stack"
  ) +
  theme_bw() +
  theme(
    panel.background = element_rect(fill = "white"),
    plot.background = element_rect(fill = "white"),
    panel.grid.major = element_line(color = "grey90"),
    panel.grid.minor = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.text.y = element_blank(),
    axis.title.x = element_text(size = 15),
    axis.title.y = element_blank(),
    legend.text = element_text(size = 15),
    legend.title = element_text(size = 15)
  ) +
  scale_y_continuous(expand = c(0, 0)) +
  facet_wrap(~ group, scales = "free_x") +
  labs(x = "Group")

print(p)
