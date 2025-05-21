## figures_fisher_1per.r

# Attach packages ---------------------------
library(ggplot2)

# Define figures ---------------------------
figure <- ggplot(iris, aes(x = Sepal.Width, y = Sepal.Length, color = Species)) +
    geom_point() +
    theme_classic()

# Download figures ---------------------------
output$figures_fisher_1per <- downloadHandler(
    filename = function() {
        paste("figure_fisher_1per_", Sys.Date(), ".png", sep = "")
    },
    content = function(file) {
        ggsave(file, plot = figure, device = "png", width = 6, height = 4)
    }
)
