## figures_fisher_multiper.r

# Download figures ---------------------------
output$figures_fisher_multiper <- downloadHandler(
    filename = function() {
        datatype <- isolate(input$datatype_fisher_multiper)
        paste0("figure_fisher_multiper_", tolower(datatype), ".zip")
    },
    content = function(zipfile) {
        datatype <- isolate(input$datatype_fisher_multiper)
        plot_list <- list()
        p1 <- switch(datatype,
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
        plot_list[[1]] <- list(
            plot = p1,
            name = paste0("figure_fisher_multiper_", tolower(datatype), "_1.png")
        )
        if (datatype == "Conch") {
            p2 <- ggplot(iris, aes(x = Species, fill = as.factor(Petal.Width))) +
                geom_bar() +
                theme_classic()
            plot_list[[2]] <- list(
                plot = p2,
                name = paste0("figure_fisher_multiper_", tolower(datatype), "_2.png")
            )
        }
        tmp_files <- lapply(plot_list, function(item) {
            tmp <- file.path(tempdir(), item$name)
            ggsave(tmp, plot = item$plot, device = "png", width = 6, height = 4)
            tmp
        })
        zip::zipr(zipfile, files = unlist(tmp_files), root = tempdir())
    },
    contentType = "application/zip"
)
