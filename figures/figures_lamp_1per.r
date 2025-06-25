## figures_lamp_1per.r

# Download figures ---------------------------
output$figures_lamp_1per_hidden <- downloadHandler(
    filename = function() {
        datatype <- isolate(input$datatype_lamp_1per)
        paste0("figure_lamp_1per_", tolower(datatype), ".zip")
    },
    content = function(zipfile) {
        showLoaderBar("lamp_1per", session)
        # Define datatype
        datatype <- isolate(input$datatype_lamp_1per)

        # Define dataframe
        datafile <- df_upload_lamp_1per()
        lamp_1per_year_selection <- if (lamp_1per_year_selection_flag()) input[["lamp_1per_year_selection"]] else "None"
        lamp_1per_period_selection <- if (lamp_1per_period_selection_flag()) input[["lamp_1per_period_selection"]] else "None"
        if (datatype == "Conch") {
            study_conch_check <- FALSE
            study_lobster_check <- FALSE
            study_finfish_check <- FALSE
            study_urchin_crab_check <- FALSE
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
        } else if (datatype == "General LAMP") {
            study_conch_check <- "Conch" %in% names(datafile)
            study_lobster_check <- "Lobster" %in% names(datafile)
            study_finfish_check <- "Finfish" %in% names(datafile)
            study_urchin_crab_check <- "Diadema_Crab" %in% names(datafile)
            df_biomass <- if (study_finfish_check) datafile$Biomass
            if (as.character(lamp_1per_period_selection) != "None") {
                df_sites <- datafile$Sites |>
                    filter(format(Date, "%b %Y") == lamp_1per_period_selection)
            } else {
                df_sites <- datafile$Sites
            }
            df_species <- datafile$Species
            df_finfish <- if (study_finfish_check) {
                datafile$Finfish %>%
                    inner_join(df_sites, by = c("Site ID", "Period")) %>%
                    filter(!is.na(`Site ID`) & !is.null(`Site ID`))
            } else {
                data.frame(matrix(ncol = 0, nrow = 0))
            }
            df_conch <- if (study_conch_check) {
                datafile$Conch %>%
                    inner_join(df_sites, by = c("Site ID", "Period")) %>%
                    filter(!is.na(`Site ID`) & !is.null(`Site ID`))
            } else {
                data.frame(matrix(ncol = 0, nrow = 0))
            }
            df_lobster <- if (study_lobster_check) {
                datafile$Lobster %>%
                    inner_join(df_sites, by = c("Site ID", "Period")) %>%
                    filter(!is.na(`Site ID`) & !is.null(`Site ID`))
            } else {
                data.frame(matrix(ncol = 0, nrow = 0))
            }
            df_urchin_crab <- if (study_urchin_crab_check) {
                datafile$Diadema_Crab %>%
                    inner_join(df_sites, by = c("Site ID", "Period")) %>%
                    filter(!is.na(`Site ID`) & !is.null(`Site ID`)) %>%
                    mutate(across(everything(), ~ ifelse(. %in% c("N/E", "NE"), 0, .))) %>%
                    mutate(
                        `Adult Diadema antillarum` = as.numeric(`Adult Diadema antillarum`),
                        `Juvenile Diadema antillarum` = as.numeric(`Juvenile Diadema antillarum`),
                        `Mithrax spinosissumus` = as.numeric(`Mithrax spinosissumus`)
                    )
            } else {
                data.frame(matrix(ncol = 0, nrow = 0))
            }
            study_conch_check <- nrow(df_conch) > 3
            study_lobster_check <- nrow(df_lobster) > 3
            study_finfish_check <- nrow(df_finfish) > 3
            study_urchin_crab_check <- nrow(df_urchin_crab) > 3
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
        } else if (datatype == "General LAMP" & study_finfish_check) {
            df_table1f_transect_density <- df_finfish %>%
                left_join(df_species, by = "Species") %>%
                mutate(Total = rowSums(across(`0-5 cm`:`>40 cm`, as.numeric), na.rm = TRUE)) %>%
                filter(!is.na(`Site ID`)) %>%
                group_by(`Mgmt Zone`, `Site ID`, Transect) %>%
                summarise(
                    Transects = n_distinct(Transect, na.rm = TRUE),
                    `Area (m2)` = Transects * 100,
                    `Count` = sum(Total),
                    `Density / 100m2 (Transect)` = (`Count` / `Area (m2)`) * 100,
                    .groups = "drop"
                )
            df_table1f <- df_table1f_transect_density %>%
                group_by(`Mgmt Zone`) %>%
                summarise(
                    Sites = n_distinct(`Site ID`, na.rm = TRUE),
                    Transects = sum(Transects),
                    `Area (m2)` = sum(`Area (m2)`),
                    `Count` = sum(`Count`),
                    `Density / 100m2` = round(`Count` / `Area (m2)` * 100, 1),
                    `Median Density / 100m2` = round(median(`Density / 100m2 (Transect)`), 1),
                    .groups = "drop"
                )
            df_table1f_overall <- df_table1f_transect_density %>%
                summarise(
                    `Mgmt Zone` = "Overall",
                    Sites = n_distinct(`Site ID`, na.rm = TRUE),
                    Transects = sum(Transects),
                    `Area (m2)` = sum(`Area (m2)`),
                    `Count` = sum(`Count`),
                    `Density / 100m2` = round(`Count` / `Area (m2)` * 100, 1),
                    `Median Density / 100m2` = round(median(`Density / 100m2 (Transect)`), 1)
                )
            df_table1f <- bind_rows(df_table1f, df_table1f_overall)
            df_table2f <- df_finfish %>%
                left_join(df_species, by = "Species") %>%
                mutate(Total = as.numeric(`0-5 cm`) + as.numeric(`6-10 cm`) + as.numeric(`11-20 cm`) + as.numeric(`21-30 cm`) + as.numeric(`31-40 cm`) + as.numeric(`>40 cm`)) %>%
                filter(!is.na(`Site ID`)) %>%
                group_by(`Mgmt Zone`, Grouping) %>%
                left_join(df_table1f, by = "Mgmt Zone") %>%
                summarise(
                    Count = sum(Total),
                    `Density / 100m2` = round(Count / `Area (m2)` * 100, 1)
                ) %>%
                distinct() %>%
                na.omit() %>%
                arrange(Grouping)
            figure1f <- ggplot(df_table2f, aes(x = `Mgmt Zone`, y = `Density / 100m2`, fill = Grouping)) +
                geom_col(color = "black", position = "dodge") +
                theme_custom +
                xlab("Management Zone") +
                ylab("Fish Density per 100m2") +
                theme(axis.text.x = element_text(angle = 0)) +
                scale_fill_manual(values = palette[2:8])
            p1 <- figure1f
            plot_list[[length(plot_list) + 1]] <- list(
                plot = p1,
                name = paste0("figure_fisher_1per_", tolower(datatype), "_fig1f.png"),
                width = 6,
                height = 3
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
        } else if (datatype == "General LAMP" & study_finfish_check) {
            df_table3f_prelim <- df_finfish %>%
                left_join(df_species, by = "Species") %>%
                left_join(df_biomass, by = c("Family", "Scientific Name")) %>%
                mutate(
                    `0-5cm BM` = as.numeric(`0-5 cm`) * (LWRa * ((LWRconv * 2.5)^LWRb)),
                    `6-10cm BM` = as.numeric(`6-10 cm`) * (LWRa * ((LWRconv * 8)^LWRb)),
                    `11-20cm BM` = as.numeric(`11-20 cm`) * (LWRa * ((LWRconv * 15.5)^LWRb)),
                    `21-30cm BM` = as.numeric(`21-30 cm`) * (LWRa * ((LWRconv * 25.5)^LWRb)),
                    `31-40cm BM` = as.numeric(`31-40 cm`) * (LWRa * ((LWRconv * 35.5)^LWRb)),
                    `>40cm BM` = as.numeric(`>40 cm`) * (LWRa * ((LWRconv * 40)^LWRb)),
                    Biomass = `0-5cm BM` + `6-10cm BM` + `11-20cm BM` + `21-30cm BM` + `31-40cm BM` + `>40cm BM`
                ) %>%
                group_by(`Mgmt Zone`, Grouping, `Site ID`) %>%
                summarize(
                    Transects = n_distinct(Transect, na.rm = TRUE),
                    `Site Biomass` = sum(Biomass, na.rm = TRUE),
                    `Site Biomass Density (g/100m2)` = `Site Biomass` / Transects
                )
            figure2f <- ggplot(filter(df_table3f_prelim, Grouping %in% c("Commercial", "Herbivorous")), aes(x = `Mgmt Zone`, y = `Site Biomass Density (g/100m2)`, fill = Grouping)) +
                geom_boxplot(color = "black", position = "dodge") +
                stat_summary(fun = mean, geom = "point", shape = 4, size = 4, color = "black", position = position_dodge(width = 0.75)) +
                theme_custom +
                xlab("Management Zone") +
                ylab("Fish Biomass (g per 100m2)") +
                theme(axis.text.x = element_text(angle = 0)) +
                scale_fill_manual(values = palette[2:8])
            p2 <- figure2f
            plot_list[[length(plot_list) + 1]] <- list(
                plot = p2,
                name = paste0("figure_fisher_1per_", tolower(datatype), "_fig2f.png"),
                width = 6,
                height = 4
            )
        }

        # Add p3 plots to plotlist
        if (datatype == "General LAMP" & study_finfish_check) {
            df_table4f <- df_finfish %>%
                left_join(df_species, by = "Species") %>%
                left_join(df_biomass, by = c("Family", "Scientific Name")) %>%
                rename("40+ cm" = `>40 cm`) %>%
                rename("06-10 cm" = `6-10 cm`) %>%
                rename("0-05 cm" = `0-5 cm`) %>%
                pivot_longer(
                    cols = c("0-05 cm", "06-10 cm", "11-20 cm", "21-30 cm", "31-40 cm", "40+ cm"),
                    names_to = "Size Class", values_to = "Count"
                ) %>%
                group_by(`Mgmt Zone`, Grouping, `Size Class`) %>%
                summarize(Frequency = sum(as.numeric(Count))) %>%
                filter(Grouping %in% c("Commercial", "Herbivorous"))
            A <- ggplot(filter(df_table4f, Grouping == "Commercial"), aes(x = `Mgmt Zone`, y = Frequency, fill = `Size Class`)) +
                geom_col(color = "black", position = "dodge") +
                theme_custom +
                xlab("Management Zone") +
                theme(axis.text.x = element_text(angle = 0)) +
                scale_fill_manual(values = palette[2:8]) +
                labs(title = "A", subtitle = "Commercial Fish") +
                guides(fill = "none")
            B <- ggplot(filter(df_table4f, Grouping == "Herbivorous"), aes(x = `Mgmt Zone`, y = Frequency, fill = `Size Class`)) +
                geom_col(color = "black", position = "dodge") +
                theme_custom +
                xlab("Management Zone") +
                theme(axis.text.x = element_text(angle = 0)) +
                scale_fill_manual(values = palette[2:8]) +
                labs(title = "B", subtitle = "Herbivorous Fish") +
                theme(legend.position = "bottom")
            figure3f <- ggarrange(A, B, ncol = 1, nrow = 2, heights = c(1, 1.2))
            p3 <- figure3f
            plot_list[[length(plot_list) + 1]] <- list(
                plot = p3,
                name = paste0("figure_fisher_1per_", tolower(datatype), "_fig3f.png"),
                width = 6,
                height = 6
            )
        }

        # Add p4 plots to plotlist
        if (datatype == "General LAMP" & study_conch_check) {
            df_table1co_zones <- df_conch %>%
                filter(!is.na(`Site ID`)) %>%
                group_by(`Mgmt Zone`, `Site ID`) %>%
                summarise(
                    Transects = n_distinct(Transect, na.rm = TRUE),
                    `Area (m2)` = Transects * 200,
                    `Milk Conch Count` = sum(Species == "Sc" | Species == "Strombus costatus" | Species == "Milk Conch"),
                    `Queen Conch Count` = sum(Species == "Ag" | Species == "Aliger gigas" | Species == "Queen Conch")
                ) %>%
                group_by(`Mgmt Zone`) %>%
                summarize(
                    Sites = n_distinct(`Site ID`, na.rm = TRUE),
                    Transects = sum(Transects),
                    `Area (m2)` = sum(`Area (m2)`),
                    `Area (Hec)` = `Area (m2)` / 10000,
                    `Queen Conch` = sum(`Queen Conch Count`),
                    `Milk Conch` = sum(`Milk Conch Count`),
                    `Queen Conch / Hec` = round(`Queen Conch` / `Area (Hec)`, 1),
                    `Milk Conch / Hec` = round(`Milk Conch` / `Area (Hec)`, 1)
                ) %>%
                select(-Transects, -`Area (m2)`)
            df_figure1co <- df_table1co_zones %>%
                select(`Mgmt Zone`, `Queen Conch` = `Queen Conch / Hec`, `Milk Conch` = `Milk Conch / Hec`) %>%
                pivot_longer(cols = c(`Queen Conch`, `Milk Conch`), names_to = "Species", values_to = "Density (Conch/Hectare)")
            figure1co <- ggplot(df_figure1co, aes(x = `Mgmt Zone`, y = `Density (Conch/Hectare)`, fill = Species)) +
                geom_col(color = "black", position = "dodge") +
                theme_custom +
                xlab("Management Zone") +
                theme(axis.text.x = element_text(angle = 0)) +
                scale_fill_manual(values = palette[2:8])
            p4 <- figure1co
            plot_list[[length(plot_list) + 1]] <- list(
                plot = p4,
                name = paste0("figure_fisher_1per_", tolower(datatype), "_fig1co.png"),
                width = 6,
                height = 2
            )
        }

        # Add p5 plots to plotlist
        if (datatype == "General LAMP" & study_conch_check) {
            df_figure2co <- df_conch %>%
                filter(Species == "Ag" | Species == "Aliger gigas" | Species == "Queen Conch") %>%
                mutate(
                    `Shell Length (cm)` = `Shell Length (mm)` / 10,
                    Lipped = factor(
                        case_when(
                            is.na(`Lip Thickness (mm)`) ~ "Not Lipped",
                            `Lip Thickness (mm)` > 0 ~ "Lipped",
                            TRUE ~ "Not Lipped"
                        ),
                        levels = c("Not Lipped", "Lipped")
                    )
                )
            A <- ggplot(filter(df_figure2co, `Mgmt Zone` == "Conservation Zone"), aes(x = `Shell Length (cm)`, fill = Lipped)) +
                geom_histogram(color = "black", position = "identity", alpha = 0.5) +
                theme_custom +
                labs(title = "A", subtitle = "Conservation Zone") +
                scale_fill_manual(values = c(palette[3], palette[4])) +
                geom_vline(xintercept = 17.8, linetype = "dashed") +
                scale_x_continuous(limits = c(0, 25), breaks = seq(0, 25, 5))
            B <- ggplot(filter(df_figure2co, `Mgmt Zone` == "General Use Zone"), aes(x = `Shell Length (cm)`, fill = Lipped)) +
                geom_histogram(color = "black", position = "identity", alpha = 0.5) +
                theme_custom +
                labs(title = "B", subtitle = "General Use Zone") +
                scale_fill_manual(values = c(palette[3], palette[4])) +
                geom_vline(xintercept = 17.8, linetype = "dashed") +
                scale_x_continuous(limits = c(0, 25), breaks = seq(0, 25, 5))
            C <- ggplot(filter(df_figure2co, `Mgmt Zone` == "Special Management Zone"), aes(x = `Shell Length (cm)`, fill = Lipped)) +
                geom_histogram(color = "black", position = "identity", alpha = 0.5) +
                theme_custom +
                labs(title = "C", subtitle = "Special Management Zone") +
                scale_fill_manual(values = c(palette[3], palette[4])) +
                geom_vline(xintercept = 17.8, linetype = "dashed") +
                scale_x_continuous(limits = c(0, 25), breaks = seq(0, 25, 5))
            D <- ggplot(df_figure2co, aes(x = `Shell Length (cm)`, fill = Lipped)) +
                geom_histogram(color = "black", position = "identity", alpha = 0.5) +
                theme_custom +
                labs(title = "D", subtitle = "Overall") +
                scale_fill_manual(values = c(palette[3], palette[4])) +
                geom_vline(xintercept = 17.8, linetype = "dashed") +
                scale_x_continuous(limits = c(0, 25), breaks = seq(0, 25, 5))
            figure2co <- ggarrange(A, B, C, D, nrow = 2, ncol = 2)
            p5 <- figure2co
            plot_list[[length(plot_list) + 1]] <- list(
                plot = p5,
                name = paste0("figure_fisher_1per_", tolower(datatype), "_fig2co.png"),
                width = 6,
                height = 5.5
            )
        }

        # Add p6 plots to plotlist
        if (datatype == "General LAMP" & study_lobster_check) {
            df_table1l_overall <- df_lobster %>%
                filter(!is.na(`Site ID`)) %>%
                summarise(
                    Sites = n_distinct(`Site ID`, na.rm = TRUE),
                    `Hec Sampled` = n_distinct(paste0(`Site ID`, Transect), na.rm = TRUE) * 200 / 10000,
                    `Caribbean Spiny` = sum(Species == "Pa" | Species == "Panulirus argus" | Species == "Caribbean Spiny Lobster"),
                    `Caribbean Spiny / Hec` = round(`Caribbean Spiny` / `Hec Sampled`, 1),
                    `Spotted Spiny` = sum(Species == "Pg" | Species == "Panulirus guttatus" | Species == "Spotted Spiny Lobster"),
                    Males = sum(Sex %in% c("M", "m", "Male", "male")),
                    Females = sum(Sex %in% c("F", "f", "Female", "female")),
                    Total = `Caribbean Spiny` + `Spotted Spiny`,
                    Unsexed = Total - Males - Females
                ) %>%
                select(-`Hec Sampled`) %>%
                mutate(`Mgmt Zone` = "Overall")
            df_table1l_zones <- df_lobster %>%
                filter(!is.na(`Site ID`)) %>%
                group_by(`Mgmt Zone`) %>%
                summarise(
                    Sites = n_distinct(`Site ID`, na.rm = TRUE),
                    `Hec Sampled` = n_distinct(paste0(`Site ID`, Transect), na.rm = TRUE) * 200 / 10000,
                    `Caribbean Spiny` = sum(Species == "Pa" | Species == "Panulirus argus" | Species == "Caribbean Spiny Lobster"),
                    `Caribbean Spiny / Hec` = round(`Caribbean Spiny` / `Hec Sampled`, 1),
                    `Spotted Spiny` = sum(Species == "Pg" | Species == "Panulirus guttatus" | Species == "Spotted Spiny Lobster"),
                    Males = sum(Sex %in% c("M", "m", "Male", "male")),
                    Females = sum(Sex %in% c("F", "f", "Female", "female"))
                ) %>%
                select(-`Hec Sampled`)
            df_figure1lA <- df_table1l_overall %>%
                select(Males, Females, Unsexed) %>%
                pivot_longer(c(Males, Females, Unsexed), names_to = "Sex", values_to = "Value")
            A <- ggplot(df_figure1lA, aes(x = "", y = Value, fill = Sex)) +
                geom_col(color = "black") +
                geom_label(aes(label = Value), position = position_stack(vjust = 0.5), show.legend = FALSE) +
                coord_polar(theta = "y") +
                theme_custom +
                xlab("") +
                labs(title = "A") +
                theme(legend.box = "vertical", legend.direction = "vertical") +
                scale_fill_manual(values = palette[2:4])
            B <- ggplot(df_table1l_zones, aes(x = `Mgmt Zone`, y = `Caribbean Spiny / Hec`, fill = `Mgmt Zone`)) +
                geom_col(color = "black") +
                theme_custom +
                xlab("Management Zone") +
                labs(title = "B") +
                theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(), legend.box = "vertical", legend.direction = "vertical") +
                scale_fill_manual(values = c(palette[1], palette[5], palette[6], palette[7], palette[8]))
            figure1l <- ggarrange(A, B, ncol = 2, nrow = 1)
            p6 <- figure1l
            plot_list[[length(plot_list) + 1]] <- list(
                plot = p6,
                name = paste0("figure_fisher_1per_", tolower(datatype), "_fig1l.png"),
                width = 6,
                height = 4
            )
        }

        # Add p7 plots to plotlist
        if (datatype == "General LAMP" & study_lobster_check) {
            df_figure2l <- df_lobster %>%
                filter(Species == "Pa" | Species == "Panulirus argus" | Species == "Caribbean Spiny Lobster") %>%
                filter(
                    `Carapace Length (mm)` != 0 & `Carapace Length (mm)` != "0" & `Carapace Length (mm)` != "0.0" &
                        `Carapace Length (mm)` != "NE" & `Carapace Length (mm)` != "N/E" & `Carapace Length (mm)` != "NA" &
                        `Carapace Length (mm)` != "N/A"
                )
            A <- ggplot(filter(df_figure2l, `Mgmt Zone` == "Conservation Zone"), aes(x = `Carapace Length (mm)`)) +
                geom_histogram(color = "black", position = "identity", alpha = 0.5, fill = palette[1]) +
                theme_custom +
                labs(title = "A", subtitle = "Conservation Zone") +
                geom_vline(xintercept = 76, linetype = "dashed") +
                scale_x_continuous(limits = c(0, 150), breaks = seq(0, 150, 25))
            B <- ggplot(filter(df_figure2l, `Mgmt Zone` == "General Use Zone"), aes(x = `Carapace Length (mm)`)) +
                geom_histogram(color = "black", position = "identity", alpha = 0.5, fill = palette[1]) +
                theme_custom +
                labs(title = "B", subtitle = "General Use Zone") +
                geom_vline(xintercept = 76, linetype = "dashed") +
                scale_x_continuous(limits = c(0, 150), breaks = seq(0, 150, 25))
            C <- ggplot(filter(df_figure2l, `Mgmt Zone` == "Special Management Zone"), aes(x = `Carapace Length (mm)`)) +
                geom_histogram(color = "black", position = "identity", alpha = 0.5, fill = palette[1]) +
                theme_custom +
                labs(title = "C", subtitle = "Special Management Zone") +
                geom_vline(xintercept = 76, linetype = "dashed") +
                scale_x_continuous(limits = c(0, 150), breaks = seq(0, 150, 25))
            D <- ggplot(df_figure2l, aes(x = `Carapace Length (mm)`)) +
                geom_histogram(color = "black", position = "identity", alpha = 0.5, fill = palette[1]) +
                theme_custom +
                labs(title = "D", subtitle = "Overall") +
                geom_vline(xintercept = 76, linetype = "dashed") +
                scale_x_continuous(limits = c(0, 150), breaks = seq(0, 150, 25))
            figure2l <- ggarrange(A, B, C, D, ncol = 2, nrow = 2)
            p7 <- figure2l
            plot_list[[length(plot_list) + 1]] <- list(
                plot = p7,
                name = paste0("figure_fisher_1per_", tolower(datatype), "_fig2l.png"),
                width = 6,
                height = 5
            )
        }

        # Add p8 plots to plotlist
        if (datatype == "General LAMP" & study_urchin_crab_check) {
            df_figure1dm <- df_urchin_crab %>%
                pivot_longer(
                    cols = c(`Adult Diadema antillarum`, `Juvenile Diadema antillarum`, `Mithrax spinosissumus`),
                    names_to = "Species", values_to = "Count"
                ) %>%
                group_by(`Site ID`, Species) %>%
                summarize(Count = sum(Count, na.rm = TRUE))
            figure1dm <- ggplot(df_figure1dm, aes(x = Species, y = Count)) +
                geom_boxplot(color = "black", fill = palette[1]) +
                stat_summary(fun = mean, geom = "point", shape = 4, size = 4, color = "black") +
                theme_custom +
                theme(axis.text.x = element_text(angle = 0, size = 8))
            p8 <- figure1dm
            plot_list[[length(plot_list) + 1]] <- list(
                plot = p8,
                name = paste0("figure_fisher_1per_", tolower(datatype), "_fig1dm.png"),
                width = 6,
                height = 2
            )
        }

        # Add files to directory and zip
        tmp_files <- lapply(plot_list, function(item) {
            tmp <- file.path(tempdir(), item$name)
            ggsave(filename = tmp, plot = item$plot, device = "png", width = item$width, height = item$height, units = "in", dpi = 300)
            tmp
        })
        hideLoaderBar("lamp_1per", session)
        zip::zipr(zipfile, files = unlist(tmp_files), root = tempdir())
    },
    contentType = "application/zip"
)
