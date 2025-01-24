## theme.R

# Load packages ---------------------------
library(fresh)

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

# Define shiny dashboard theme ---------------------------
theme_dashboard <- create_theme(
    adminlte_color(light_blue = "#00A09A", aqua = "#2CA0D9", green = "#00AE46", purple = "#FFA400"),
    adminlte_sidebar(
        width = "200px", dark_bg = "#003A68", dark_hover_bg = "#2CA0D9", dark_color = "white", dark_hover_color = "white",
        dark_submenu_bg = "#74bde2", dark_submenu_hover_color = "#003A68", dark_submenu_color = "black"
    ),
    adminlte_global(content_bg = "white", box_bg = "#00A09A", info_box_bg = "#2CA0D9")
)
