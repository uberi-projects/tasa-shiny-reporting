## figures_lamp_1per.r

# Download figures ---------------------------
output$figures_lamp_1per <- downloadHandler(
    filename = function() {
        datatype <- isolate(input$datatype_lamp_1per)
        paste0("figure_lamp_1per_", tolower(datatype), ".zip")
    },
    content = function(zipfile) {
        # Define datatype
        datatype <- isolate(input$datatype_lamp_1per)

        # Define dataframe
        datafile <- df_upload_lamp_1per()
        lamp_1per_year_selection <- if (lamp_1per_year_selection_flag()) input[["lamp_1per_year_selection"]] else "None"
        if (datatype == "Conch") {
            update_site_id <- function(df) {
                df |> mutate(`Site ID` = ifelse(is.na(`Site ID`) | `Site ID` == "", as.character(`Old Site ID`), as.character(`Site ID`)))
            }
            df_surveys <- update_site_id(datafile$Survey_Data)
            df_sites <- update_site_id(datafile$Sites)
            df_merged <- df_surveys |>
                left_join(df_sites, by = "Site ID") |>
                filter(Section != "NW") |>
                mutate(`Conch Count` = as.integer(`Conch Count`))
            if (as.character(lamp_1per_year_selection) != "None") {
                df_merged <- df_merged |>
                    filter(format(Date.x, "%Y") == lamp_1per_year_selection) |>
                    filter(format(Date.y, "%Y") == lamp_1per_year_selection)
                df_sites <- df_sites |>
                    filter(format(Date, "%Y") == lamp_1per_year_selection)
                df_surveys <- df_surveys |>
                    filter(format(Date, "%Y") == lamp_1per_year_selection)
            }
        }

        # Initialize plotlist
        plot_list <- list()

        # Add p1 plots to plotlist
        if (datatype == "Conch") {
            summary_transect <- df_merged %>%
                select(Date.x, `Site ID`, Transect, Section, `Conch Count`, `Shell Length (in)`, `Lip Thickness (mm)`) %>%
                group_by(Date.x, `Site ID`, Transect, Section) %>%
                summarize(Conch_Count_Transect = sum(`Conch Count`)) %>%
                ungroup()
            summary_site <- summary_transect %>%
                group_by(`Site ID`, Section) %>%
                summarize(
                    Num_Transects = n(),
                    Conch_Count_Site = sum(Conch_Count_Transect),
                    Mean_Conch_Per_Transect = mean(Conch_Count_Transect),
                    Median_Conch_Per_Transect = median(Conch_Count_Transect),
                    Min_Conch_Per_Transect = min(Conch_Count_Transect),
                    Max_Conch_Per_Transect = max(Conch_Count_Transect)
                )
            mean_per_transect <- summary_transect %>%
                group_by(Section) %>%
                summarize(Mean_Conch_Per_Transect = mean(Conch_Count_Transect))
            mean_per_site <- summary_site %>%
                group_by(Section) %>%
                summarize(Mean_Conch_Per_Site = mean(Conch_Count_Site))
            A <- ggplot(summary_transect, aes(x = Conch_Count_Transect)) +
                geom_histogram(
                    binwidth = 1, color = "black",
                    fill = "white", boundary = 0
                ) +
                geom_boxplot(aes(y = 175), width = 10, size = 1) +
                geom_vline(
                    xintercept = mean(summary_transect$Conch_Count_Transect),
                    size = 1, linetype = "dashed"
                ) +
                theme_custom +
                xlab("Conch per Transect") +
                ylab("Number of Transects") +
                labs(title = "A") +
                scale_x_continuous(
                    breaks = c(0, 20, 40, 60, 80, 100),
                    labels = c(0, 20, 40, 60, 80, 100)
                ) +
                coord_cartesian(xlim = c(1, 110))
            B <- ggplot(summary_site, aes(x = Conch_Count_Site)) +
                geom_histogram(
                    binwidth = 5, color = "black",
                    fill = "white", boundary = 0
                ) +
                geom_boxplot(aes(y = 45), width = 3.5, size = 1) +
                geom_vline(
                    xintercept = mean(summary_site$Conch_Count_Site),
                    size = 1, linetype = "dashed"
                ) +
                theme_custom +
                xlab("Conch per Site") +
                ylab("Number of Sites") +
                labs(title = "B") +
                scale_x_continuous(
                    breaks = c(0, 50, 100, 150, 200, 250),
                    labels = c(0, 50, 100, 150, 200, 250)
                ) +
                coord_cartesian(xlim = c(1, 275))
            C <- ggplot(
                summary_transect,
                aes(x = Section, y = Conch_Count_Transect, fill = Section)
            ) +
                geom_boxplot(fill = "white") +
                geom_point(
                    data = mean_per_transect,
                    aes(x = Section, y = Mean_Conch_Per_Transect),
                    size = 5, shape = 2, stroke = 1
                ) +
                theme_custom +
                ylab("Conch per Transect") +
                guides(fill = "none") +
                labs(title = "C")
            D <- ggplot(
                summary_site,
                aes(x = Section, y = Conch_Count_Site, fill = Section)
            ) +
                geom_boxplot(fill = "white") +
                geom_point(
                    data = mean_per_site,
                    aes(x = Section, y = Mean_Conch_Per_Site),
                    size = 5, shape = 2, stroke = 1
                ) +
                theme_custom +
                ylab("Conch per Site") +
                guides(fill = "none") +
                labs(title = "D")
            figure2_bottom <- ggarrange(C, D, ncol = 2, nrow = 1)
            figure2 <- ggarrange(A, B, figure2_bottom, ncol = 1, nrow = 3)
            p1 <- figure2
            plot_list[[1]] <- list(
                plot = p1,
                name = paste0("figure_fisher_1per_", tolower(datatype), "_fig2.png"),
                width = 6,
                height = 7.5
            )
        } else if (datatype == "General LAMP") {
            p1 <- ggplot(iris, aes(Sepal.Width, Sepal.Length, color = Species)) +
                geom_point() +
                theme_dark()
            plot_list[[1]] <- list(
                plot = p1,
                name = paste0("figure_fisher_1per_", tolower(datatype), "_fig2.png"),
                width = 6,
                height = 8
            )
        }

        # Add p2 plots to plotlist
        if (datatype == "Conch") {
            df_fig3 <- df_merged %>%
                mutate(
                    `Shell Length (cm)` = `Shell Length (in)` * 2.54,
                    Lipped = factor(
                        case_when(
                            is.na(`Lip Thickness (mm)`) ~ "Not Lipped",
                            `Lip Thickness (mm)` > 0 ~ "Lipped",
                            TRUE ~ "Not Lipped"
                        ),
                        levels = c("Not Lipped", "Lipped")
                    )
                )
            A <- ggplot(filter(df_fig3, Section == "NE"), aes(x = `Shell Length (cm)`, fill = Lipped)) +
                geom_histogram(color = "black", position = "identity", alpha = 0.5) +
                theme_custom +
                labs(title = "A", subtitle = "Northeast") +
                scale_fill_manual(values = c(palette[3], palette[4])) +
                geom_vline(xintercept = 17.8, linetype = "dashed") +
                scale_x_continuous(limits = c(0, 30), breaks = seq(0, 30, 5))
            B <- ggplot(filter(df_fig3, Section == "NO"), aes(x = `Shell Length (cm)`, fill = Lipped)) +
                geom_histogram(color = "black", position = "identity", alpha = 0.5) +
                theme_custom +
                labs(title = "B", subtitle = "North") +
                scale_fill_manual(values = c(palette[3], palette[4])) +
                geom_vline(xintercept = 17.8, linetype = "dashed") +
                scale_x_continuous(limits = c(0, 30), breaks = seq(0, 30, 5))
            C <- ggplot(filter(df_fig3, Section == "SE"), aes(x = `Shell Length (cm)`, fill = Lipped)) +
                geom_histogram(color = "black", position = "identity", alpha = 0.5) +
                theme_custom +
                labs(title = "C", subtitle = "Southeast") +
                scale_fill_manual(values = c(palette[3], palette[4])) +
                geom_vline(xintercept = 17.8, linetype = "dashed") +
                scale_x_continuous(limits = c(0, 30), breaks = seq(0, 30, 5))
            D <- ggplot(filter(df_fig3, Section == "SW"), aes(x = `Shell Length (cm)`, fill = Lipped)) +
                geom_histogram(color = "black", position = "identity", alpha = 0.5) +
                theme_custom +
                labs(title = "D", subtitle = "Southwest") +
                scale_fill_manual(values = c(palette[3], palette[4])) +
                geom_vline(xintercept = 17.8, linetype = "dashed") +
                scale_x_continuous(limits = c(0, 30), breaks = seq(0, 30, 5))
            E <- ggplot(df_fig3, aes(x = `Shell Length (cm)`, fill = Lipped)) +
                geom_histogram(color = "black", position = "identity", alpha = 0.5) +
                theme_custom +
                labs(title = "E", subtitle = "Overall") +
                scale_fill_manual(values = c(palette[3], palette[4])) +
                geom_vline(xintercept = 17.8, linetype = "dashed") +
                scale_x_continuous(limits = c(0, 30), breaks = seq(0, 30, 5))
            figure3 <- ggarrange(A, B, C, D, E, nrow = 3, ncol = 2)
            p2 <- figure3
            plot_list[[2]] <- list(
                plot = p2,
                name = paste0("figure_fisher_1per_", tolower(datatype), "_fig3.png"),
                width = 6,
                height = 8
            )
        }

        # Add files to directory and zip
        tmp_files <- lapply(plot_list, function(item) {
            tmp <- file.path(tempdir(), item$name)
            ggsave(filename = tmp, plot = item$plot, device = "png", width = item$width, height = item$height, units = "in", dpi = 300)
            tmp
        })
        zip::zipr(zipfile, files = unlist(tmp_files), root = tempdir())
    },
    contentType = "application/zip"
)
