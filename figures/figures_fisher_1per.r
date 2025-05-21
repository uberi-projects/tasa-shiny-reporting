## figures_fisher_1per.r

# Download figures ---------------------------
output$figures_fisher_1per <- downloadHandler(
    filename = function() {
        datatype <- isolate(input$datatype_fisher_1per)
        paste0("figure_fisher_1per_", tolower(datatype), ".zip")
    },
    content = function(zipfile) {
        # Define datatype
        datatype <- isolate(input$datatype_fisher_1per)

        # Define dataframe
        datafile <- df_upload_fisher_1per()
        df <- datafile[[1]]
        if (datatype == "Conch") {
            timeframe <- input$timeframe_fisher_1per
            df$`Waypoint Date` <- as.Date(df$`Waypoint Date`)
            fisher_1per_year_selection <- if (fisher_1per_year_selection_flag()) input[["fisher_1per_year_selection"]] else "None"
            fisher_1per_conch_season_selection <- if (fisher_1per_conch_season_selection_flag()) input[["fisher_1per_conch_season_selection"]] else "None"
            if (timeframe == "Annual" && as.character(fisher_1per_year_selection) != "None") {
                df <- df |>
                    filter(format(`Waypoint Date`, "%Y") == fisher_1per_year_selection)
            } else if (timeframe == "Seasonal" && as.character(fisher_1per_conch_season_selection) != "None") {
                season_years <- unlist(strsplit(fisher_1per_conch_season_selection, "-"))
                start_date <- as.Date(paste0(season_years[1], "-07-01"))
                end_date <- as.Date(paste0(season_years[2], "-02-28"))
                df <- df |>
                    filter(`Waypoint Date` >= start_date, `Waypoint Date` <= end_date)
            }
            df$Month <- format(df$`Waypoint Date`, "%b %Y")
        }
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
            name = paste0("figure_fisher_1per_", tolower(datatype), "_1.png"),
            width = 6,
            height = 4
        )
        if (datatype == "Conch") {
            df_events <- df %>%
                mutate(
                    Month = format(`Waypoint Date`, "%b %Y"),
                    Month_sort = as.Date(format(`Waypoint Date`, "%Y-%m-01"))
                ) %>%
                select(Month, `Waypoint ID`, `Hours Fished`, `Total Lbs of Catch`) %>%
                distinct() %>%
                filter(!is.na(`Hours Fished`), !is.na(`Total Lbs of Catch`), `Hours Fished` > 0) %>%
                group_by(Month) %>%
                summarize(
                    Hours = sum(`Hours Fished`, na.rm = TRUE),
                    `Catch (lb)` = sum(`Total Lbs of Catch`, na.rm = TRUE)
                )
            df_fig2_A <- df %>%
                mutate(
                    Month = format(`Waypoint Date`, "%b %Y"),
                    Month_sort = as.Date(format(`Waypoint Date`, "%Y-%m-01"))
                ) %>%
                group_by(Month, Month_sort) %>%
                summarize(
                    Fishers = n_distinct(`Fishers Name`),
                    Conch = n(),
                    Events = n_distinct(`Waypoint ID`)
                ) %>%
                left_join(df_events, by = "Month") %>%
                arrange(Month_sort) %>%
                mutate(
                    `CPUE (lb/hr)` = round(`Catch (lb)` / Hours, 1),
                    `CPUE (num/hr)` = round(Conch / Hours, 1)
                )
            A <- ggplot(df_fig2_A, aes(x = Month_sort, y = Events, group = 1)) +
                geom_line(size = 0.5) +
                geom_point(size = 2.5) +
                scale_x_date(
                    date_breaks = "1 month",
                    date_labels = "%b '%y"
                ) +
                expand_limits(y = 0) +
                theme_custom +
                xlab("Month") +
                ylab("Fishing Events") +
                labs(title = "A")
            B <- ggplot(df_fig2_A, aes(x = Month_sort, y = Hours, group = 1)) +
                geom_line(size = 0.5) +
                geom_point(size = 2.5) +
                scale_x_date(
                    date_breaks = "1 month",
                    date_labels = "%b '%y"
                ) +
                expand_limits(y = 0) +
                theme_custom +
                xlab("Month") +
                ylab("Hours Fished") +
                labs(title = "B")
            C <- ggplot(df_fig2_A, aes(x = Month_sort, y = Conch, group = 1)) +
                geom_line(size = 0.5) +
                geom_point(size = 2.5) +
                scale_x_date(
                    date_breaks = "1 month",
                    date_labels = "%b '%y"
                ) +
                expand_limits(y = 0) +
                theme_custom +
                xlab("Month") +
                ylab("Number Conchs") +
                labs(title = "C")
            D <- ggplot(df_fig2_A, aes(x = Month_sort, y = `Catch (lb)`, group = 1)) +
                geom_line(size = 0.5) +
                geom_point(size = 2.5) +
                scale_x_date(
                    date_breaks = "1 month",
                    date_labels = "%b '%y"
                ) +
                expand_limits(y = 0) +
                theme_custom +
                xlab("Month") +
                ylab("Catch (lb)") +
                labs(title = "D")
            E <- ggplot(df_fig2_A, aes(x = Month_sort, group = 1)) +
                geom_line(aes(y = `CPUE (lb/hr)`), size = 0.5) +
                geom_point(aes(y = `CPUE (lb/hr)`, shape = "lb/hr"), size = 2.5) +
                geom_line(aes(y = `CPUE (num/hr)`), size = 0.5) +
                geom_point(aes(y = `CPUE (num/hr)`, shape = "num/hr"), size = 2.5) +
                scale_x_date(
                    date_breaks = "1 month",
                    date_labels = "%b '%y"
                ) +
                expand_limits(y = 0) +
                scale_shape_manual(
                    name = "CPUE Type",
                    values = c("lb/hr" = 4, "num/hr" = 19)
                ) +
                theme_custom +
                xlab("Month") +
                ylab("Catch per Unit Effort") +
                labs(title = "E")
            figure_2_top <- ggarrange(
                A, B, C, D,
                ncol = 2, nrow = 2
            )
            figure2 <- ggarrange(
                figure_2_top, E,
                ncol = 1,
                heights = c(2, 1)
            )
            p2 <- figure2
            plot_list[[2]] <- list(
                plot = p2,
                name = paste0("figure_fisher_1per_", tolower(datatype), "_fig2.png"),
                width = 6,
                height = 8
            )
        }
        tmp_files <- lapply(plot_list, function(item) {
            tmp <- file.path(tempdir(), item$name)
            ggsave(filename = tmp, plot = item$plot, device = "png", width = item$width, height = item$height, units = "in", dpi = 300)
            tmp
        })
        zip::zipr(zipfile, files = unlist(tmp_files), root = tempdir())
    },
    contentType = "application/zip"
)
