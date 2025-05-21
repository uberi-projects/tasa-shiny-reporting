## figures_fisher_1per.r

# Attach packages ---------------------------
library(ggplot2)

# Download figures ---------------------------
output$figures_fisher_1per <- downloadHandler(
    filename = function() {
        datatype <- isolate(input$datatype_fisher_1per)
        paste0("figure_fisher_1per_", datatype, ".zip")
    },
    content = function(zipfile) {
        datatype <- isolate(input$datatype_fisher_1per)
        p <- switch(datatype,
            "Conch" = ggplot(iris, aes(Sepal.Width, Sepal.Length, color = Species)) +
                geom_point() +
                theme_classic(),
            "Lobster" = ggplot(iris, aes(Sepal.Width, Sepal.Length, color = Species)) +
                geom_point() +
                theme_dark(),
            "Finfish" = ggplot(iris, aes(Sepal.Width, Sepal.Length, color = Species)) +
                geom_point() +
                theme_void(),
            stop("Unknown datatype: ", datatype)
        )
        img_name <- paste0("figure_fisher_1per_", datatype, ".png")
        tmp_png <- file.path(tempdir(), img_name)
        ggsave(tmp_png, plot = p, device = "png", width = 6, height = 4)
        zip::zipr(
            zipfile,
            files = tmp_png,
            root = tempdir()
        )
    },
    contentType = "application/zip"
)
