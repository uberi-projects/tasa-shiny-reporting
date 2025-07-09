## figures_fisheries_1per.r

# Download figures ---------------------------
output$figures_fisheries_1per_hidden <- downloadHandler(
    filename = function() {
        datatype <- isolate(input$datatype_fisheries_1per)
        paste0("figure_fisheries_1per_", tolower(datatype), ".zip")
    },
    content = function(zipfile) {
        showLoaderBar("fisheries_1per", session)
        # Define datatype
        datatype <- isolate(input$datatype_fisheries_1per)

        # Define dataframe
        datafile <- df_upload_fisheries_1per()
        df <- datafile[[1]]

        # Initialize plotlist
        plot_list <- list()

        # Add p1 plots to plotlist
        if (datatype == "Conch") {
            df_events <- df %>%
                mutate(
                    Month = format(`Date of Encounter`, "%b %Y"),
                    Month_sort = as.Date(format(`Date of Encounter`, "%Y-%m-01"))
                ) %>%
                select(Month, `Mission Id`, `Hours Fished`) %>%
                distinct() %>%
                mutate(`Hours Fished` = as.numeric(`Hours Fished`)) %>%
                filter(!is.na(`Hours Fished`), `Hours Fished` > 0) %>%
                group_by(Month) %>%
                summarize(
                    Hours = sum(`Hours Fished`, na.rm = TRUE)
                )
            df_fig1_A <- df %>%
                mutate(
                    Month = format(`Date of Encounter`, "%b %Y"),
                    Month_sort = as.Date(format(`Date of Encounter`, "%Y-%m-01"))
                ) %>%
                group_by(Month, Month_sort) %>%
                summarize(
                    Vessels = n_distinct(`Name of Vessel`),
                    Conch = n(),
                    Missions = n_distinct(`Mission Id`)
                ) %>%
                left_join(df_events, by = "Month") %>%
                arrange(Month_sort) %>%
                mutate(
                    `CPUE (num/hr)` = round(Conch / Hours, 1)
                )
            A <- ggplot(df_fig1_A, aes(x = Month_sort, y = Hours, group = 1)) +
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
                labs(title = "A")
            B <- ggplot(df_fig1_A, aes(x = Month_sort, y = Conch, group = 1)) +
                geom_line(size = 0.5) +
                geom_point(size = 2.5) +
                scale_x_date(
                    date_breaks = "1 month",
                    date_labels = "%b '%y"
                ) +
                expand_limits(y = 0) +
                theme_custom +
                xlab("Month") +
                ylab("Number Conch") +
                labs(title = "B")
            C <- ggplot(df_fig1_A, aes(x = Month_sort, y = `CPUE (num/hr)`, group = 1)) +
                geom_line(size = 0.5) +
                geom_point(size = 2.5) +
                scale_x_date(
                    date_breaks = "1 month",
                    date_labels = "%b '%y"
                ) +
                expand_limits(y = 0) +
                theme_custom +
                xlab("Month") +
                ylab("Catch per Unit Effort") +
                labs(title = "C")
            figure1 <- ggarrange(
                A, B, C,
                nrow = 3, ncol = 1
            )
            p1 <- figure1
            plot_list[[1]] <- list(
                plot = p1,
                name = paste0("figure_fisheries_1per_", tolower(datatype), "_fig1.png"),
                width = 6,
                height = 8
            )
        } else if (datatype == "Lobster") {
            df_events <- df %>%
                mutate(
                    Month = format(`Date of Encounter`, "%b %Y"),
                    Month_sort = as.Date(format(`Date of Encounter`, "%Y-%m-01"))
                ) %>%
                select(Month, `Mission Id`, `Hours Fished`) %>%
                distinct() %>%
                mutate(`Hours Fished` = as.numeric(`Hours Fished`)) %>%
                filter(!is.na(`Hours Fished`), `Hours Fished` > 0) %>%
                group_by(Month) %>%
                summarize(
                    Hours = sum(`Hours Fished`, na.rm = TRUE)
                )
            df_fig1_A <- df %>%
                mutate(
                    Month = format(`Date of Encounter`, "%b %Y"),
                    Month_sort = as.Date(format(`Date of Encounter`, "%Y-%m-01"))
                ) %>%
                group_by(Month, Month_sort) %>%
                summarize(
                    Vessels = n_distinct(`Name of Vessel`),
                    Lobster = n(),
                    Missions = n_distinct(`Mission Id`)
                ) %>%
                left_join(df_events, by = "Month") %>%
                arrange(Month_sort) %>%
                mutate(
                    `CPUE (num/hr)` = round(Lobster / Hours, 1)
                )
            A <- ggplot(df_fig1_A, aes(x = Month_sort, y = Hours, group = 1)) +
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
                labs(title = "A")
            B <- ggplot(df_fig1_A, aes(x = Month_sort, y = Lobster, group = 1)) +
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
                labs(title = "B")
            C <- ggplot(df_fig1_A, aes(x = Month_sort, y = `CPUE (num/hr)`, group = 1)) +
                geom_line(size = 0.5) +
                geom_point(size = 2.5) +
                scale_x_date(
                    date_breaks = "1 month",
                    date_labels = "%b '%y"
                ) +
                expand_limits(y = 0) +
                theme_custom +
                xlab("Month") +
                ylab("Catch per Unit Effort") +
                labs(title = "C")
            figure1 <- ggarrange(
                A, B, C,
                nrow = 3, ncol = 1
            )
            p1 <- figure1
            plot_list[[1]] <- list(
                plot = p1,
                name = paste0("figure_fisheries_1per_", tolower(datatype), "_fig1.png"),
                width = 6,
                height = 8
            )
        } else if (datatype == "Finfish") {
            df_events <- df %>%
                mutate(
                    Month = format(`Date of Encounter`, "%b %Y"),
                    Month_sort = as.Date(format(`Date of Encounter`, "%Y-%m-01"))
                ) %>%
                select(Month, `Mission Id`, `Hours Fished`) %>%
                distinct() %>%
                mutate(`Hours Fished` = as.numeric(`Hours Fished`)) %>%
                filter(!is.na(`Hours Fished`), `Hours Fished` > 0) %>%
                group_by(Month) %>%
                summarize(
                    Hours = sum(`Hours Fished`, na.rm = TRUE)
                )
            df_fig1_A <- df %>%
                mutate(
                    Month = format(`Date of Encounter`, "%b %Y"),
                    Month_sort = as.Date(format(`Date of Encounter`, "%Y-%m-01"))
                ) %>%
                group_by(Month, Month_sort) %>%
                summarize(
                    Vessels = n_distinct(`Name of vessel used`),
                    Finfish = n(),
                    Missions = n_distinct(`Mission Id`)
                ) %>%
                left_join(df_events, by = "Month") %>%
                arrange(Month_sort) %>%
                mutate(
                    `CPUE (num/hr)` = round(Finfish / Hours, 1)
                )
            A <- ggplot(df_fig1_A, aes(x = Month_sort, y = Hours, group = 1)) +
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
                labs(title = "A")
            B <- ggplot(df_fig1_A, aes(x = Month_sort, y = Finfish, group = 1)) +
                geom_line(size = 0.5) +
                geom_point(size = 2.5) +
                scale_x_date(
                    date_breaks = "1 month",
                    date_labels = "%b '%y"
                ) +
                expand_limits(y = 0) +
                theme_custom +
                xlab("Month") +
                ylab("Number Finfish") +
                labs(title = "B")
            C <- ggplot(df_fig1_A, aes(x = Month_sort, y = `CPUE (num/hr)`, group = 1)) +
                geom_line(size = 0.5) +
                geom_point(size = 2.5) +
                scale_x_date(
                    date_breaks = "1 month",
                    date_labels = "%b '%y"
                ) +
                expand_limits(y = 0) +
                theme_custom +
                xlab("Month") +
                ylab("Catch per Unit Effort") +
                labs(title = "C")
            figure1 <- ggarrange(
                A, B, C,
                nrow = 3, ncol = 1
            )
            p1 <- figure1
            plot_list[[1]] <- list(
                plot = p1,
                name = paste0("figure_fisheries_1per_", tolower(datatype), "_fig1.png"),
                width = 6,
                height = 8
            )
        }

        # Add p2 plots to plotlist
        if (datatype == "Conch") {
            df_fig2_unsummarized <- df %>%
                mutate(
                    weight_for_mk = ifelse(`Weight Type` == "Market Clean", `Weight (g)`, NA_real_),
                    weight_for_whole = ifelse(`Weight Type` == "Whole", `Weight (g)`, NA_real_),
                    weight_for_up = ifelse(`Weight Type` == "Unprocessed", `Weight (g)`, NA_real_)
                ) %>%
                mutate(
                    Month = format(`Date of Encounter`, "%b %Y"),
                    Month_sort = as.Date(format(`Date of Encounter`, "%Y-%m-01")),
                    `Shell Length (mm)` = as.numeric(`Shell Length (mm)`),
                    `Lip Thickness (mm)` = as.numeric(`Lip Thickness (mm)`),
                    weight_for_mk = as.numeric(weight_for_mk),
                    weight_for_whole = as.numeric(weight_for_whole),
                    weight_for_up = as.numeric(weight_for_up)
                ) %>%
                arrange(Month_sort)
            df_fig2 <- df %>%
                mutate(
                    weight_for_mk = ifelse(`Weight Type` == "Market Clean", `Weight (g)`, NA_real_),
                    weight_for_whole = ifelse(`Weight Type` == "Whole", `Weight (g)`, NA_real_),
                    weight_for_up = ifelse(`Weight Type` == "Unprocessed", `Weight (g)`, NA_real_),
                    Month = format(`Date of Encounter`, "%b %Y"),
                    Month_sort = as.Date(format(`Date of Encounter`, "%Y-%m-01")),
                    `Shell Length (mm)` = as.numeric(`Shell Length (mm)`),
                    `Lip Thickness (mm)` = as.numeric(`Lip Thickness (mm)`)
                ) %>%
                mutate(
                    weight_for_mk = as.numeric(weight_for_mk),
                    weight_for_whole = as.numeric(weight_for_whole),
                    weight_for_up = as.numeric(weight_for_up)
                ) %>%
                group_by(Month, Month_sort) %>%
                summarize(
                    weight_for_mk = round(mean(weight_for_mk, na.rm = TRUE), 1),
                    weight_for_whole = round(mean(weight_for_whole, na.rm = TRUE), 1),
                    weight_for_up = round(mean(weight_for_up, na.rm = TRUE), 1),
                    .groups = "drop"
                ) %>%
                arrange(Month_sort)
            A <- ggplot(df_fig2_unsummarized, aes(x = weight_for_mk)) +
                geom_histogram(binwidth = 10, color = "black") +
                scale_fill_manual(values = palette[2:3], drop = FALSE) +
                theme_custom +
                expand_limits(x = 0) +
                xlab("Market Clean Weight (g)") +
                ylab("Count") +
                labs(title = "A")
            B <- ggplot(df_fig2_unsummarized, aes(x = weight_for_whole)) +
                geom_histogram(binwidth = 100, color = "black") +
                scale_fill_manual(values = palette[2:3], drop = FALSE) +
                theme_custom +
                expand_limits(x = 0) +
                xlab("Whole Weight (g)") +
                ylab("Count") +
                labs(title = "B")
            C <- ggplot(df_fig2, aes(x = Month_sort)) +
                theme_custom +
                scale_x_date(
                    date_breaks = "1 month",
                    date_labels = "%b '%y"
                ) +
                ylab("Weight (g)") +
                xlab("Month") +
                scale_shape_manual(
                    name = "Weight Type",
                    values = c("Market Clean" = 4, "Whole" = 19, "Unprocessed" = 6)
                ) +
                scale_color_manual(
                    name = "Weight Type",
                    values = c("Market Clean" = "black", "Whole" = "black", "Unprocessed" = "black")
                ) +
                expand_limits(y = 0) +
                labs(title = "C") +
                theme(legend.position = "bottom")
            if (sum(!is.na(df_fig2$weight_for_mk)) >= 2) {
                C <- C + geom_line(aes(y = weight_for_mk, group = 1), size = 0.5)
            }
            if (sum(!is.na(df_fig2$weight_for_mk)) >= 1) {
                C <- C + geom_point(aes(y = weight_for_mk, shape = "Market Clean"), size = 2.5)
            }
            if (sum(!is.na(df_fig2$weight_for_whole)) >= 2) {
                C <- C + geom_line(aes(y = weight_for_whole, group = 1), size = 0.5)
            }
            if (sum(!is.na(df_fig2$weight_for_whole)) >= 1) {
                C <- C + geom_point(aes(y = weight_for_whole, shape = "Whole"), size = 2.5)
            }
            if (sum(!is.na(df_fig2$weight_for_up)) >= 2) {
                C <- C + geom_line(aes(y = weight_for_up, group = 1), size = 0.5)
            }
            if (sum(!is.na(df_fig2$weight_for_up)) >= 1) {
                C <- C + geom_point(aes(y = weight_for_up, shape = "Unprocessed"), size = 2.5)
            }
            figure_3_top <- ggarrange(
                A, B,
                ncol = 2, nrow = 1
            )
            figure2 <- ggarrange(
                figure_3_top, C,
                ncol = 1, nrow = 2
            )
            p2 <- figure2
            plot_list[[2]] <- list(
                plot = p2,
                name = paste0("figure_fisheries_1per_", tolower(datatype), "_fig2.png"),
                width = 6,
                height = 8
            )
        } else if (datatype == "Lobster") {
            df_fig2_unsummarized <- df %>%
                filter(!is.na(Sex)) %>%
                mutate(
                    Month = format(`Date of Encounter`, "%b %Y"),
                    Month_sort = as.Date(format(`Date of Encounter`, "%Y-%m-01")),
                    weight_for_headed = ifelse(`Weight Type` == "headed", `Total Weight (g)`, NA_real_),
                    `Pleopod Stage` = as.character(`Pleopod Stage`),
                    `Carapace Length (mm)` = as.numeric(`Carapace Length (mm)`),
                    weight_for_headed = as.numeric(weight_for_headed)
                ) %>%
                arrange(Month_sort)
            df_fig2 <- df %>%
                filter(!is.na(Sex)) %>%
                mutate(
                    Month = format(`Date of Encounter`, "%b %Y"),
                    Month_sort = as.Date(format(`Date of Encounter`, "%Y-%m-01")),
                    weight_for_headed = ifelse(`Weight Type` == "headed", `Total Weight (g)`, NA_real_),
                    `Pleopod Stage` = as.character(`Pleopod Stage`),
                    `Carapace Length (mm)` = as.numeric(`Carapace Length (mm)`),
                    weight_for_headed = as.numeric(weight_for_headed)
                ) %>%
                group_by(Month, Month_sort) %>%
                summarize(
                    `Mean Carapace Length (mm)` = round(mean(`Carapace Length (mm)`, na.rm = TRUE), 1),
                    `Median Carapace Length (mm)` = round(median(`Carapace Length (mm)`, na.rm = TRUE), 1),
                    `Mean Weight (g)` = round(mean(weight_for_headed, na.rm = TRUE), 1),
                    `Median Weight (g)` = round(median(weight_for_headed, na.rm = TRUE), 1),
                    `Female (%)` = round(100 * sum(Sex == "Female", na.rm = TRUE) / n(), 1),
                    `Male (%)` = round(100 * sum(Sex == "Male", na.rm = TRUE) / n(), 1)
                ) %>%
                arrange(Month_sort)
            df_fig2_carapace <- df_fig2 %>%
                filter(!is.nan(`Mean Carapace Length (mm)`))
            df_fig2_weight <- df_fig2 %>%
                filter(!is.nan(`Mean Weight (g)`))
            A <- ggplot(df_fig2_unsummarized, aes(x = `Carapace Length (mm)`, fill = `Sex`)) +
                geom_histogram(binwidth = 10, color = "black") +
                scale_fill_manual(values = palette[2:3], drop = FALSE) +
                theme_custom +
                expand_limits(x = 0) +
                xlab("Carapace Length (mm)") +
                ylab("Count") +
                labs(title = "A")
            B <- ggplot(df_fig2_unsummarized, aes(x = weight_for_headed, fill = `Sex`)) +
                geom_histogram(binwidth = 50, color = "black") +
                scale_fill_manual(values = palette[2:3], drop = FALSE) +
                theme_custom +
                expand_limits(x = 0) +
                xlab("Weight (g)") +
                ylab("Count") +
                labs(title = "B")
            C <- ggplot(df_fig2_carapace, aes(x = Month_sort, y = `Mean Carapace Length (mm)`)) +
                geom_line(size = 0.5) +
                geom_point(size = 2.5) +
                theme_custom +
                scale_x_date(
                    date_breaks = "1 month",
                    date_labels = "%b '%y"
                ) +
                ylab("Mean Carapace Length (mm)") +
                xlab("Month") +
                scale_y_continuous(n.breaks = 4) +
                labs(title = "C") +
                theme(legend.position = "none")
            D <- ggplot(df_fig2_weight, aes(x = Month_sort, y = `Mean Weight (g)`)) +
                geom_line(size = 0.5) +
                geom_point(size = 2.5) +
                theme_custom +
                scale_x_date(
                    date_breaks = "1 month",
                    date_labels = "%b '%y"
                ) +
                ylab("Weight (g)") +
                xlab("Month") +
                labs(title = "D") +
                theme(legend.position = "none")
            figure_3_top <- ggarrange(
                A, B,
                ncol = 2, nrow = 1
            )
            figure2 <- ggarrange(
                figure_3_top, C, D,
                ncol = 1, nrow = 3
            )
            p2 <- figure2
            plot_list[[2]] <- list(
                plot = p2,
                name = paste0("figure_fisheries_1per_", tolower(datatype), "_fig2.png"),
                width = 6,
                height = 8
            )
        } else if (datatype == "Finfish") {
            df_fig2_unsummarized <- df %>%
                mutate(
                    Month = format(`Date of Encounter`, "%b %Y"),
                    Month_sort = as.Date(format(`Date of Encounter`, "%Y-%m-01")),
                    `Total Length (cm)` = as.numeric(`Total Length (cm)`),
                    `Fork Length (cm)` = as.numeric(`Fork Length (cm)`),
                    `Weight (g)` = as.numeric(`Weight (g)`)
                ) %>%
                arrange(Month_sort)
            df_fig2 <- df %>%
                mutate(
                    Month = format(`Date of Encounter`, "%b %Y"),
                    Month_sort = as.Date(format(`Date of Encounter`, "%Y-%m-01")),
                    `Total Length (cm)` = as.numeric(`Total Length (cm)`),
                    `Fork Length (cm)` = as.numeric(`Fork Length (cm)`),
                    `Weight (g)` = as.numeric(`Weight (g)`)
                ) %>%
                group_by(Month, Month_sort) %>%
                summarize(
                    `Mean Total Length (cm)` = round(mean(`Total Length (cm)`, na.rm = TRUE), 1),
                    `Median Total Length (cm)` = round(median(`Total Length (cm)`, na.rm = TRUE), 1),
                    `Mean Fork Length (cm)` = round(mean(`Fork Length (cm)`, na.rm = TRUE), 1),
                    `Median Fork Length (cm)` = round(median(`Fork Length (cm)`, na.rm = TRUE), 1),
                    `Mean Weight (g)` = round(mean(`Weight (g)`, na.rm = TRUE), 1),
                    `Median Weight (g)` = round(median(`Weight (g)`, na.rm = TRUE), 1)
                ) %>%
                arrange(Month_sort)
            df_fig2_long <- df_fig2 %>%
                pivot_longer(
                    cols = c(`Mean Total Length (cm)`, `Mean Fork Length (cm)`),
                    names_to = "Metric",
                    values_to = "Length"
                )
            A <- ggplot(df_fig2_unsummarized, aes(x = `Total Length (cm)`)) +
                geom_histogram(binwidth = 10, color = "black") +
                theme_custom +
                expand_limits(x = 0) +
                xlab("Total Length (cm)") +
                ylab("Count") +
                labs(title = "A")
            B <- ggplot(df_fig2_unsummarized, aes(x = `Fork Length (cm)`)) +
                geom_histogram(binwidth = 10, color = "black") +
                theme_custom +
                expand_limits(x = 0) +
                xlab("Fork Length (cm)") +
                ylab("Count") +
                labs(title = "B")
            C <- ggplot(df_fig2_long, aes(x = Month_sort, group = Metric)) +
                geom_line(aes(y = Length), size = 0.5) +
                geom_point(aes(y = Length, shape = Metric), size = 2.5) +
                scale_shape_manual(name = "Metric", values = c("Mean Total Length (cm)" = 4, "Mean Fork Length (cm)" = 19)) +
                theme_custom +
                scale_x_date(
                    date_breaks = "1 month",
                    date_labels = "%b '%y"
                ) +
                ylab("Length (cm)") +
                xlab("Month") +
                labs(title = "C") +
                scale_y_continuous(n.breaks = 4)
            figure_2_top <- ggarrange(
                A, B,
                ncol = 2, nrow = 1
            )
            figure2 <- ggarrange(
                figure_2_top, C,
                ncol = 1, nrow = 2
            )
            p2 <- figure2
            plot_list[[2]] <- list(
                plot = p2,
                name = paste0("figure_fisheries_1per_", tolower(datatype), "_fig2.png"),
                width = 6,
                height = 6
            )
        }

        # Add p3 plots to plotlist
        if (datatype == "Conch") {
            df_fig3 <- df %>%
                mutate(
                    `Shell Length (cm)` = as.numeric(`Shell Length (mm)`) / 10,
                    `Lip Thickness (mm)` = as.numeric(`Lip Thickness (mm)`)
                )
            A <- ggplot(filter(df_fig3, is.na(`Lip Thickness (mm)`)), aes(x = `Shell Length (cm)`)) +
                geom_histogram(color = "black", position = "identity", alpha = 0.5) +
                theme_custom +
                scale_fill_manual(values = c(palette[3], palette[4])) +
                geom_vline(xintercept = 17.8, linetype = "dashed") +
                scale_x_continuous(limits = c(0, 30), breaks = seq(0, 30, 5)) +
                labs(title = "A", subtitle = "No Data for Lip Thickness")
            B <- ggplot(filter(df_fig3, `Lip Thickness (mm)` <= 5 & `Lip Thickness (mm)` >= 0), aes(x = `Shell Length (cm)`)) +
                geom_histogram(color = "black", position = "identity", alpha = 0.5) +
                theme_custom +
                scale_fill_manual(values = c(palette[3], palette[4])) +
                geom_vline(xintercept = 17.8, linetype = "dashed") +
                scale_x_continuous(limits = c(0, 30), breaks = seq(0, 30, 5)) +
                labs(title = "B", subtitle = "Lip Thickness 0-5mm")
            C <- ggplot(filter(df_fig3, `Lip Thickness (mm)` > 5), aes(x = `Shell Length (cm)`)) +
                geom_histogram(color = "black", position = "identity", alpha = 0.5) +
                theme_custom +
                scale_fill_manual(values = c(palette[3], palette[4])) +
                geom_vline(xintercept = 17.8, linetype = "dashed") +
                scale_x_continuous(limits = c(0, 30), breaks = seq(0, 30, 5)) +
                labs(title = "C", subtitle = "Lip Thickness >5mm")
            D <- ggplot(df_fig3, aes(x = `Shell Length (cm)`)) +
                geom_histogram(color = "black", position = "identity", alpha = 0.5) +
                theme_custom +
                scale_fill_manual(values = c(palette[3], palette[4])) +
                geom_vline(xintercept = 17.8, linetype = "dashed") +
                scale_x_continuous(limits = c(0, 30), breaks = seq(0, 30, 5)) +
                labs(title = "D", subtitle = "Overall")
            figure3 <- ggarrange(A, B, C, D, nrow = 2, ncol = 2)
            p3 <- figure3
            plot_list[[3]] <- list(
                plot = p3,
                name = paste0("figure_fisheries_1per_", tolower(datatype), "_fig3.png"),
                width = 6,
                height = 5
            )
        } else if (datatype == "Lobster") {
            df_fig3 <- df %>%
                mutate(
                    Month = format(`Date of Encounter`, "%b %Y"),
                    Month_sort = as.Date(format(`Date of Encounter`, "%Y-%m-01"))
                ) %>%
                group_by(Month, Month_sort) %>%
                summarize(
                    Female = round(100 * sum(Sex == "Female", na.rm = TRUE) / n(), 1),
                    Male = round(100 * sum(Sex == "Male", na.rm = TRUE) / n(), 1)
                ) %>%
                arrange(Month_sort) %>%
                pivot_longer(cols = c(Female, Male), names_to = "Sex", values_to = "Percent")
            A <- ggplot(df_fig3, aes(x = Month_sort, fill = Sex, y = Percent)) +
                geom_col(color = "black") +
                scale_fill_manual(values = palette[2:3]) +
                theme_custom +
                scale_x_date(
                    date_breaks = "1 month",
                    date_labels = "%b '%y"
                ) +
                xlab("Month") +
                ylab("Percent")
            B <- ggplot(filter(df_fig2_unsummarized, !is.na(`Pleopod Stage`)), aes(x = `Pleopod Stage`)) +
                geom_bar(color = "black", fill = palette[2]) +
                theme_custom +
                expand_limits(x = 0) +
                xlab("Pleopod Stage") +
                ylab("Count") +
                labs(title = "B")
            figure3 <- ggarrange(A, B, nrow = 1, ncol = 2)
            p3 <- figure3
            plot_list[[3]] <- list(
                plot = p3,
                name = paste0("figure_fisheries_1per_", tolower(datatype), "_fig3.png"),
                width = 6,
                height = 8
            )
        } else if (datatype == "Finfish") {
            df_fig3_long <- df_fig2 %>%
                pivot_longer(
                    cols = c(`Mean Weight (g)`, `Median Weight (g)`),
                    names_to = "Metric",
                    values_to = "Weight"
                )
            A <- ggplot(df_fig2_unsummarized, aes(x = `Weight (g)`)) +
                geom_histogram(binwidth = 100, color = "black") +
                theme_custom +
                expand_limits(x = 0) +
                xlab("Mean Weight (g)") +
                ylab("Count") +
                labs(title = "A")
            B <- ggplot(df_fig3_long, aes(x = Month_sort, group = Metric)) +
                geom_line(aes(y = Weight), size = 0.5) +
                geom_point(aes(y = Weight, shape = Metric), size = 2.5) +
                scale_shape_manual(name = "Metric", values = c("Mean Weight (g)" = 4, "Median Weight (g)" = 19)) +
                theme_custom +
                scale_x_date(
                    date_breaks = "1 month",
                    date_labels = "%b '%y"
                ) +
                ylab("Weight (g)") +
                xlab("Month") +
                labs(title = "C") +
                scale_y_continuous(n.breaks = 4)
            figure3 <- ggarrange(
                A, B,
                ncol = 1, nrow = 2
            )
            p3 <- figure3
            plot_list[[3]] <- list(
                plot = p3,
                name = paste0("figure_fisheries_1per_", tolower(datatype), "_fig3.png"),
                width = 6,
                height = 6
            )
        }

        # Add p4 plots to plotlist
        if (datatype == "Finfish") {
            figure4 <- ggplot(df, aes(x = fct_infreq(`Fish Species`))) +
                geom_bar() +
                theme_custom +
                xlab("Fish Species") +
                ylab("Count")
            p4 <- figure4
            plot_list[[4]] <- list(
                plot = p4,
                name = paste0("figure_fisheries_1per_", tolower(datatype), "_fig4.png"),
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
        hideLoaderBar("fisheries_1per", session)
        zip::zipr(zipfile, files = unlist(tmp_files), root = tempdir())
    },
    contentType = "application/zip"
)
