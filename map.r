## map.r

# Attach Packages ---------------------------
library(sf)
library(ggspatial)
library(ggnewscale)
library(ggrepel)

# Load Shapefiles ---------------------------
ecosystems_map <- st_read("Belize_Ecosystems_2015.shp") %>%
    st_transform(4326) %>%
    filter(ECOSYSTEM %in% c("Seagrass", "Open sea", "Mangrove and littoral forest", "Coral reef")) %>%
    mutate(ECOSYSTEM = recode(ECOSYSTEM,
        "Seagrass" = "Seagrass",
        "Open sea" = "Caribbean Sea",
        "Mangrove and littoral forest" = "Mangrove/Littoral Forest",
        "Coral reef" = "Coral Reef"
    ))

areas_map <- st_read("CA.shp") %>%
    st_transform(4326) %>%
    filter(name %in% c("Spawning Aggregation Site Reserve", "Conservation", "Preservation", "Special Management Area")) %>%
    mutate(name = recode(name,
        "Spawning Aggregation Site Reserve" = "Spawning Aggregation Site",
        "Conservation" = "Conservation",
        "Preservation" = "Preservation",
        "Special Management Area" = "Special Management"
    ))

# Create Function to Make Dynamic Map ---------------------------
generate_map <- function(df, lat, long, site_id) {
    df$Long <- -abs(df[[long]])
    df$Lat <- df[[lat]]
    df_sf <- st_as_sf(df, coords = c("Long", "Lat"), crs = 4326)
    ggplot() +
        geom_sf(data = ecosystems_map, aes(fill = ECOSYSTEM), linewidth = 0, color = "transparent") +
        scale_fill_manual(values = c(
            "Seagrass" = "#bbe9ffff",
            "Caribbean Sea" = "#ebfaffff",
            "Mangrove/Littoral Forest" = "#c9ffcbff",
            "Coral Reef" = "#bdb7e3ff"
        ), name = "Ecosystem") +
        new_scale_fill() +
        geom_sf(data = areas_map, aes(fill = name), alpha = 0.85, linewidth = 0, color = "transparent") +
        scale_fill_manual(values = c(
            "Spawning Aggregation Site" = "#ef9a95ff",
            "Conservation" = "#f6f89dff",
            "Preservation" = "#f7bf9aff",
            "Special Management" = "#f5c8d5ff"
        ), name = "Zone") +
        geom_sf(data = df_sf, size = 2, color = "#302f2f", fill = "white", shape = 21) +
        geom_text_repel(
            data = df, aes(x = Long, y = Lat, label = !!sym(site_id)),
            size = 2.5, color = "black", fontface = "bold", box.padding = 0.5,
            point.padding = 0.5, force = 5, max.overlaps = 50,
            segment.color = "#555555", segment.size = 0.5
        ) +
        coord_sf(xlim = c(-88.05, -87.66), ylim = c(17.117024, 17.664305)) +
        annotation_scale(location = "br", width_hint = 0.2, style = "ticks") +
        annotation_north_arrow(
            location = "tl", which_north = "true",
            style = north_arrow_fancy_orienteering
        ) +
        theme_minimal() +
        theme(
            legend.text = element_text(size = 8),
            legend.title = element_text(size = 9),
            legend.key.size = unit(0.5, "cm"),
            legend.spacing.y = unit(0.2, "cm")
        )
}
