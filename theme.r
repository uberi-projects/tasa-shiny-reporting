## theme.R

# Define palettes ---------------------------
palette <- c(
    "#00A09A", "#2CA0D9", "#00AE46", "#FFA400", "#FF747E", "#003A68" # use for TASA branding
)
palette_extended <- c(
    "#00A09A", "#2CA0D9", "#00AE46", "#FFA400", "#FF747E", "#003A68", # use for visualizations
    "#7300a0", "#2cd9d0", "#edf04b", "#ff7b00", "#ff74e1", "#680000",
    "#b4e9e7", "#b8ddf0", "#b7eccc", "#f5efe3", "#ecbdc0", "#6c8396",
    "#e0c8e9", "#809e9d", "#eff0c2", "#e9bd93", "#b694af", "#b47e7e"
)

# Define ggplot2 theme ---------------------------
theme_custom <- theme(
    legend.position = "top",
    legend.box = "horizontal",
    axis.text.x = element_text(face = "bold", angle = 90, vjust = 0.5, hjust = 0.5, size = 10),
    axis.text.y = element_text(face = "bold", size = 12),
    axis.title.x = element_text(size = 14, margin = margin(5, 0, 0, 0)),
    axis.title.y = element_text(size = 14, margin = margin(0, 5, 0, 0)),
    plot.title = element_text(size = 18, margin = margin(0, 0, 5, 0)),
    strip.text = element_text(size = 18),
    legend.text = element_text(size = 10, vjust = 0.5),
    legend.title = element_text(face = "bold", size = 10, vjust = 0.5),
    legend.key.height = unit(0.5, "cm"),
    legend.key.width = unit(0.5, "cm"),
    legend.margin = margin(t = 5, b = 5),
    panel.background = element_rect(fill = "white", color = "white"),
    plot.background = element_rect(fill = "white", color = "white"),
    legend.background = element_rect(fill = "white", color = NA),
    legend.box.background = element_rect(fill = "white", color = NA),
    panel.grid.major.y = element_line(color = "grey85", linewidth = 0.5)
)
