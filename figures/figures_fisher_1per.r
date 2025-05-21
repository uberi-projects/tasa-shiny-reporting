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
        df$`Waypoint Date` <- as.Date(df$`Waypoint Date`)
        fisher_1per_year_selection <- if (fisher_1per_year_selection_flag()) input[["fisher_1per_year_selection"]] else "None"
        if (datatype == "Conch") {
            timeframe <- input$timeframe_fisher_1per
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
        } else if (datatype == "Lobster") {
            timeframe <- input$timeframe_fisher_1per
            fisher_1per_lobster_season_selection <- if (fisher_1per_lobster_season_selection_flag()) input[["fisher_1per_lobster_season_selection"]] else "None"
            if (timeframe == "Annual" && as.character(fisher_1per_year_selection) != "None") {
                df <- df |>
                    filter(format(`Waypoint Date`, "%Y") == fisher_1per_year_selection)
            } else if (timeframe == "Seasonal" && as.character(fisher_1per_lobster_season_selection) != "None") {
                season_years <- unlist(strsplit(fisher_1per_lobster_season_selection, "-"))
                start_date <- as.Date(paste0(season_years[1], "-07-01"))
                end_date <- as.Date(paste0(season_years[2], "-02-28"))
                df <- df |>
                    filter(`Waypoint Date` >= start_date, `Waypoint Date` <= end_date)
            }
            df$Month <- format(df$`Waypoint Date`, "%b %Y")
        } else if (datatype == "Finfish") {
            timeframe <- "Annual"
            if (timeframe == "Annual" && as.character(fisher_1per_year_selection) != "None") {
                df <- df |>
                    filter(format(`Waypoint Date`, "%Y") == fisher_1per_year_selection)
            }
            df$Month <- format(df$`Waypoint Date`, "%b %Y")
        }

        # Initialize plotlist
        plot_list <- list()

        # Add p1 plots to plotlist
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
            p1 <- figure2
            plot_list[[1]] <- list(
                plot = p1,
                name = paste0("figure_fisher_1per_", tolower(datatype), "_fig2.png"),
                width = 6,
                height = 8
            )
        } else if (datatype == "Lobster") {
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
                    Lobster = n(),
                    Events = n_distinct(`Waypoint ID`)
                ) %>%
                left_join(df_events, by = "Month") %>%
                arrange(Month_sort) %>%
                mutate(
                    `CPUE (lb/hr)` = round(`Catch (lb)` / Hours, 1),
                    `CPUE (num/hr)` = round(Lobster / Hours, 1)
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
            C <- ggplot(df_fig2_A, aes(x = Month_sort, y = Lobster, group = 1)) +
                geom_line(size = 0.5) +
                geom_point(size = 2.5) +
                scale_x_date(
                    date_breaks = "1 month",
                    date_labels = "%b '%y"
                ) +
                expand_limits(y = 0) +
                theme_custom +
                xlab("Month") +
                ylab("Number Lobsters") +
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
            p1 <- figure2
            plot_list[[1]] <- list(
                plot = p1,
                name = paste0("figure_fisher_1per_", tolower(datatype), "_fig2.png"),
                width = 6,
                height = 8
            )
        } else if (datatype == "Finfish") {
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
                    Finfish = n(),
                    Events = n_distinct(`Waypoint ID`),
                    .groups = "drop"
                ) %>%
                left_join(df_events, by = "Month") %>%
                arrange(Month_sort) %>%
                mutate(
                    `CPUE (lb/hr)` = ifelse(Hours > 0, round(`Catch (lb)` / Hours, 1), NA),
                    `CPUE (num/hr)` = ifelse(Hours > 0, round(Finfish / Hours, 1), NA)
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
            C <- ggplot(df_fig2_A, aes(x = Month_sort, y = Finfish, group = 1)) +
                geom_line(size = 0.5) +
                geom_point(size = 2.5) +
                scale_x_date(
                    date_breaks = "1 month",
                    date_labels = "%b '%y"
                ) +
                expand_limits(y = 0) +
                theme_custom +
                xlab("Month") +
                ylab("Number Finfishs") +
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
            p1 <- figure2
            plot_list[[1]] <- list(
                plot = p1,
                name = paste0("figure_fisher_1per_", tolower(datatype), "_fig2.png"),
                width = 6,
                height = 8
            )
        }

        # Add p2 plots to plotlist
        if (datatype == "Conch") {
            df_fig3_unsummarized <- df %>%
                filter(`Weight Type` == "Market Clean") %>%
                mutate(
                    Month = format(`Waypoint Date`, "%b %Y"),
                    Month_sort = as.Date(format(`Waypoint Date`, "%Y-%m-01"))
                ) %>%
                arrange(Month_sort)
            df_fig3 <- df %>%
                filter(`Weight Type` == "Market Clean") %>%
                mutate(
                    Month = format(`Waypoint Date`, "%b %Y"),
                    Month_sort = as.Date(format(`Waypoint Date`, "%Y-%m-01"))
                ) %>%
                group_by(Month, Month_sort) %>%
                summarize(
                    `Mean Weight (oz)` = round(mean(`Weight (oz)`, na.rm = TRUE), 1),
                    `Median Weight (oz)` = round(median(`Weight (oz)`, na.rm = TRUE), 1)
                ) %>%
                arrange(Month_sort)
            df_fig3_weight <- df_fig3 %>%
                filter(!is.nan(`Mean Weight (oz)`))
            A <- ggplot(df_fig3_unsummarized, aes(x = `Weight (oz)`)) +
                geom_histogram(binwidth = 1, color = "black") +
                theme_custom +
                expand_limits(x = 0) +
                xlab("Weight (oz)") +
                ylab("Count") +
                labs(title = "A") +
                theme(legend.position = "none")
            B <- ggplot(df_fig3_weight, aes(x = Month_sort, y = `Mean Weight (oz)`)) +
                geom_line(size = 0.5) +
                geom_point(size = 2.5) +
                theme_custom +
                scale_x_date(
                    date_breaks = "1 month",
                    date_labels = "%b '%y"
                ) +
                ylab("Weight (oz)") +
                xlab("Month") +
                labs(title = "B") +
                theme(legend.position = "none")
            figure3 <- ggarrange(
                A, B,
                ncol = 1, nrow = 2
            )
            p2 <- figure3
            plot_list[[2]] <- list(
                plot = p2,
                name = paste0("figure_fisher_1per_", tolower(datatype), "_fig3.png"),
                width = 6,
                height = 5
            )
        } else if (datatype == "Lobster") {
            gear_levels <- df %>%
                pull(`Type of Fishing Gears`) %>%
                unique() %>%
                sort()
            gear_colors <- setNames(palette[2:(1 + length(gear_levels))], gear_levels)
            df_fig3_unsummarized <- df %>%
                mutate(
                    Month = format(`Waypoint Date`, "%b %Y"),
                    Month_sort = as.Date(format(`Waypoint Date`, "%Y-%m-01")),
                    weight_for_headed = ifelse(`Weight type` == "headed", `Weight (oz)`, NA_real_)
                ) %>%
                arrange(Month_sort)
            df_fig3 <- df %>%
                mutate(
                    Month = format(`Waypoint Date`, "%b %Y"),
                    Month_sort = as.Date(format(`Waypoint Date`, "%Y-%m-01")),
                    weight_for_headed = ifelse(`Weight type` == "headed", `Weight (oz)`, NA_real_)
                ) %>%
                group_by(Month, Month_sort) %>%
                summarize(
                    `Mean Carapace Length (In)` = round(mean(`Carapace Length (Inches)`, na.rm = TRUE), 1),
                    `Median Carapace Length (In)` = round(median(`Carapace Length (Inches)`, na.rm = TRUE), 1),
                    `Mean Weight (oz)` = round(mean(weight_for_headed, na.rm = TRUE), 1),
                    `Median Weight (oz)` = round(median(weight_for_headed, na.rm = TRUE), 1),
                    `Female (%)` = round(100 * sum(Sex == "Female", na.rm = TRUE) / n(), 1),
                    `Male (%)` = round(100 * sum(Sex == "Male", na.rm = TRUE) / n(), 1)
                ) %>%
                arrange(Month_sort)
            df_fig3_carapace <- df_fig3 %>%
                filter(!is.nan(`Mean Carapace Length (In)`))
            df_fig3_weight <- df_fig3 %>%
                filter(!is.nan(`Mean Weight (oz)`))
            A <- ggplot(df_fig3_unsummarized, aes(x = `Type of Fishing Gears`, fill = `Type of Fishing Gears`)) +
                geom_bar(color = "black") +
                scale_fill_manual(values = gear_colors, drop = FALSE) +
                theme_custom +
                expand_limits(x = 0) +
                xlab("") +
                ylab("Count") +
                labs(title = "A")
            B <- ggplot(df_fig3_unsummarized, aes(x = `Carapace Length (Inches)`, fill = `Type of Fishing Gears`)) +
                geom_histogram(binwidth = 1, color = "black") +
                scale_fill_manual(values = gear_colors, drop = FALSE) +
                theme_custom +
                expand_limits(x = 0) +
                xlab("Carapace Length (Inches)") +
                ylab("Count") +
                labs(title = "B") +
                theme(legend.position = "none")
            C <- ggplot(df_fig3_unsummarized, aes(x = weight_for_headed, fill = `Type of Fishing Gears`)) +
                geom_histogram(binwidth = 1, color = "black") +
                scale_fill_manual(values = gear_colors, drop = FALSE) +
                theme_custom +
                expand_limits(x = 0) +
                xlab("Weight (oz)") +
                ylab("Count") +
                labs(title = "C") +
                theme(legend.position = "none")
            D <- ggplot(df_fig3_carapace, aes(x = Month_sort, y = `Mean Carapace Length (In)`)) +
                geom_line(size = 0.5) +
                geom_point(size = 2.5) +
                theme_custom +
                scale_x_date(
                    date_breaks = "1 month",
                    date_labels = "%b '%y"
                ) +
                ylab("Mean Carapace Length (In)") +
                xlab("Month") +
                scale_y_continuous(n.breaks = 4) +
                labs(title = "D") +
                theme(legend.position = "none")
            E <- ggplot(df_fig3_weight, aes(x = Month_sort, y = `Mean Weight (oz)`)) +
                geom_line(size = 0.5) +
                geom_point(size = 2.5) +
                theme_custom +
                scale_x_date(
                    date_breaks = "1 month",
                    date_labels = "%b '%y"
                ) +
                ylab("Weight (oz)") +
                xlab("Month") +
                labs(title = "E") +
                theme(legend.position = "none")
            figure_3_bottom <- ggarrange(
                B, C, D, E,
                ncol = 2, nrow = 2
            )
            figure3 <- ggarrange(
                A, figure_3_bottom,
                ncol = 1,
                heights = c(1, 2)
            )
            p2 <- figure3
            plot_list[[2]] <- list(
                plot = p2,
                name = paste0("figure_fisher_1per_", tolower(datatype), "_fig3.png"),
                width = 6,
                height = 8
            )
        } else if (datatype == "Finfish") {
            gear_levels <- df %>%
                pull(`Type of Gear`) %>%
                unique() %>%
                sort()
            gear_colors <- setNames(palette[2:(1 + length(gear_levels))], gear_levels)
            df_fig3_unsummarized <- df %>%
                mutate(
                    Month = format(`Waypoint Date`, "%b %Y"),
                    Month_sort = as.Date(format(`Waypoint Date`, "%Y-%m-01")),
                    weight_for_gutted = ifelse(`Weight Type` == "Gutted", `Weight (lbs)`, NA_real_),
                    weight_for_whole = ifelse(`Weight Type` == "Whole", `Weight (lbs)`, NA_real_)
                ) %>%
                arrange(Month_sort)
            df_fig3 <- df %>%
                mutate(
                    Month = format(`Waypoint Date`, "%b %Y"),
                    Month_sort = as.Date(format(`Waypoint Date`, "%Y-%m-01")),
                    weight_for_gutted = ifelse(`Weight Type` == "Gutted", `Weight (lbs)`, NA_real_),
                    weight_for_whole = ifelse(`Weight Type` == "Whole", `Weight (lbs)`, NA_real_)
                ) %>%
                group_by(Month, Month_sort) %>%
                summarize(
                    `Mean Length (In)` = round(mean(as.numeric(`Total Length (Inches)`), na.rm = TRUE), 1),
                    `Mean Fork Length (In)` = round(mean(as.numeric(`Fork Length (Inches)`), na.rm = TRUE), 1),
                    `Mean Weight Gutted (lbs)` = ifelse(any(`Weight Type` == "Gutted"),
                        round(mean(as.numeric(`Weight (lbs)`)[`Weight Type` == "Gutted"], na.rm = TRUE), 1),
                        NA
                    ),
                    `Mean Weight Whole (lbs)` = ifelse(any(`Weight Type` == "Whole"),
                        round(mean(as.numeric(`Weight (lbs)`)[`Weight Type` == "Whole"], na.rm = TRUE), 1),
                        NA
                    )
                ) %>%
                arrange(Month_sort)
            df_fig3_gutted_weight <- df_fig3 %>%
                filter(!is.nan(`Mean Weight Gutted (lbs)`))
            df_fig3_whole_weight <- df_fig3 %>%
                filter(!is.nan(`Mean Weight Whole (lbs)`))
            df_fig3_total_length <- df_fig3 %>%
                filter(!is.nan(`Mean Length (In)`))
            df_fig3_fork_length <- df_fig3 %>%
                filter(!is.nan(`Mean Fork Length (In)`))
            A <- ggplot(df_fig3_unsummarized, aes(x = `Type of Gear`, fill = `Type of Gear`)) +
                geom_bar(color = "black") +
                scale_fill_manual(values = gear_colors, drop = FALSE) +
                theme_custom +
                expand_limits(x = 0) +
                xlab("") +
                ylab("Count") +
                labs(title = "A") +
                theme(axis.text.x = element_text(angle = 0))
            B <- ggplot(df_fig3_unsummarized, aes(x = `Total Length (Inches)`, fill = `Type of Gear`)) +
                geom_histogram(binwidth = 1, color = "black") +
                scale_fill_manual(values = gear_colors, drop = FALSE) +
                theme_custom +
                expand_limits(x = 0) +
                xlab("Total Length (Inches)") +
                ylab("Count") +
                labs(title = "B") +
                theme(legend.position = "none")
            C <- ggplot() +
                geom_line(
                    data = df_fig3_total_length,
                    aes(x = Month_sort, y = `Mean Length (In)`, linetype = "Total Length"),
                    size = 0.5
                ) +
                geom_point(
                    data = df_fig3_total_length,
                    aes(x = Month_sort, y = `Mean Length (In)`, shape = "Total Length"),
                    size = 2.5
                ) +
                geom_line(
                    data = df_fig3_fork_length,
                    aes(x = Month_sort, y = `Mean Fork Length (In)`, linetype = "Fork Length"),
                    size = 0.5
                ) +
                geom_point(
                    data = df_fig3_fork_length,
                    aes(x = Month_sort, y = `Mean Fork Length (In)`, shape = "Fork Length"),
                    size = 2.5
                ) +
                theme_custom +
                scale_x_date(
                    date_breaks = "1 month",
                    date_labels = "%b '%y"
                ) +
                ylab("Mean Total Length (Inches)") +
                xlab("Month") +
                labs(
                    title = "C",
                    linetype = "",
                    shape = ""
                ) +
                scale_linetype_manual(
                    values = c("Total Length" = "solid", "Fork Length" = "dotted")
                ) +
                scale_shape_manual(
                    values = c("Total Length" = 16, "Fork Length" = 4)
                )
            D <- ggplot(df_fig3_gutted_weight, aes(x = Month_sort, y = `Mean Weight Gutted (lbs)`)) +
                geom_line(size = 0.5) +
                geom_point(size = 2.5) +
                theme_custom +
                scale_x_date(
                    date_breaks = "1 month",
                    date_labels = "%b '%y"
                ) +
                ylab("Gutted Weight (lb)") +
                xlab("Month") +
                labs(title = "D")
            E <- ggplot(df_fig3_whole_weight, aes(x = Month_sort, y = `Mean Weight Whole (lbs)`)) +
                geom_line(size = 0.5) +
                geom_point(size = 2.5) +
                theme_custom +
                scale_x_date(
                    date_breaks = "1 month",
                    date_labels = "%b '%y"
                ) +
                ylab("Whole Weight (lb)") +
                xlab("Month") +
                labs(title = "E")
            figure_3_bottom <- ggarrange(
                B, C, D, E,
                ncol = 2, nrow = 2
            )
            figure3 <- ggarrange(
                A, figure_3_bottom,
                ncol = 1,
                heights = c(1, 2.4)
            )
            p2 <- figure3
            plot_list[[2]] <- list(
                plot = p2,
                name = paste0("figure_fisher_1per_", tolower(datatype), "_fig3.png"),
                width = 6,
                height = 8
            )
        }

        # Add p3 plots to plotlist
        if (datatype == "Finfish") {
            df_figure4_totals <- df %>%
                group_by(`Fish Species`) %>%
                summarise(`Total Count` = n(), ..groups = "drop")
            df_figure4 <- df %>%
                mutate(
                    Month = format(`Waypoint Date`, "%b %Y"),
                    Month_sort = as.Date(format(`Waypoint Date`, "%Y-%m-01"))
                ) %>%
                group_by(`Fish Species`, Month, Month_sort) %>%
                summarise(Count = n()) %>%
                left_join(df_figure4_totals, by = "Fish Species") %>%
                mutate(`Fish Species` = as.factor(`Fish Species`))
            figure4 <- ggplot(df_figure4, aes(
                x = fct_reorder(`Fish Species`, desc(`Total Count`)),
                y = Count,
                fill = fct_reorder(Month, Month_sort)
            )) +
                geom_col(color = "black") +
                scale_fill_manual(values = palette_extended) +
                theme_custom +
                expand_limits(y = 0) +
                xlab("Fish Species") +
                ylab("Count") +
                labs(fill = "") +
                theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
            p3 <- figure4
            plot_list[[3]] <- list(
                plot = p3,
                name = paste0("figure_fisher_1per_", tolower(datatype), "_fig4.png"),
                width = 6,
                height = 6
            )
        } else if (datatype == "Lobster") {
            df_fig4 <- df %>%
                mutate(
                    Month = format(`Waypoint Date`, "%b %Y"),
                    Month_sort = as.Date(format(`Waypoint Date`, "%Y-%m-01"))
                ) %>%
                group_by(Month, Month_sort) %>%
                summarize(
                    Female = round(100 * sum(Sex == "Female", na.rm = TRUE) / n(), 1),
                    Male = round(100 * sum(Sex == "Male", na.rm = TRUE) / n(), 1)
                ) %>%
                arrange(Month_sort) %>%
                pivot_longer(cols = c(Female, Male), names_to = "Sex", values_to = "Percent")
            figure4 <- ggplot(df_fig4, aes(x = Month_sort, fill = Sex, y = Percent)) +
                geom_col(color = "black") +
                scale_fill_manual(values = palette[2:3]) +
                theme_custom +
                scale_x_date(
                    date_breaks = "1 month",
                    date_labels = "%b '%y"
                ) +
                xlab("Month") +
                ylab("Percent")
            p3 <- figure4
            plot_list[[3]] <- list(
                plot = p3,
                name = paste0("figure_fisher_1per_", tolower(datatype), "_fig4.png"),
                width = 6,
                height = 2.5
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
