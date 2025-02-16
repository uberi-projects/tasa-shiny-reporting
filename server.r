## server.R

# Load packages ---------------------------
library(readxl)

# Define server ---------------------------
server <- function(input, output, session) {
    # Define dataframes from uploads
    nas <- c("NA", "N/A", "Unknown", "Missing", "None", "")
    df_upload_fisheries <- reactive({
        req(input$upload_fisheries)
        read.csv(input$upload_fisheries$datapath)
    })
    df_upload_fisher <- reactive({
        req(input$upload_fisher)
        read.csv(input$upload_fisher$datapath)
    })
    df_upload_lamp <- reactive({
        req(input$upload_lamp)
        if (input$datatype_lamp == "Conch" & input$period_lamp == "One Period") {
            list(
                Survey_Data = read_excel(input$upload_lamp$datapath, sheet = "Survey Data", na = nas),
                Sites = read_excel(input$upload_lamp$datapath, sheet = "Sites", na = nas),
                Habitat_Types = read_excel(input$upload_lamp$datapath, sheet = "Habitat Types", na = nas)
            )
        } else if (input$datatype_lamp == "General LAMP" & input$period_lamp == "One Period") {
            list(
                Species = read_excel(input$upload_lamp$datapath, sheet = "Species", na = nas),
                Sites = read_excel(input$upload_lamp$datapath, sheet = "Sites", na = nas),
                Finfish = read_excel(input$upload_lamp$datapath, sheet = "Finfish", na = nas),
                Conch = read_excel(input$upload_lamp$datapath, sheet = "Conch", na = nas),
                Lobster = read_excel(input$upload_lamp$datapath, sheet = "Lobster", na = nas),
                Diadema_Crab = read_excel(input$upload_lamp$datapath, sheet = "Diadema and Crab", na = nas)
            )
        } else {
            pressure # putting this as placeholder until multi-period is supported
        }
    })

    df_upload_spag <- reactive({
        req(input$upload_spag)
        read.csv(input$upload_spag$datapath)
    })

    # Validate dataframes
    observeEvent(input$validate_fisheries, {
        shinyalert("Success!", "Validation Successful!")
    })
    observeEvent(input$validate_fisher, {
        shinyalert("Success!", "Validation Successful!")
    })
    observeEvent(input$validate_lamp, {
        source("validation/validate_lampconch_1per.r")
        completeness_passed <- func_validate_lampconch_1per_completeness_check(df_upload_lamp()$Survey_Data, df_upload_lamp()$Sites, df_upload_lamp()$Habitat_Types)
        validation_message_completeness <- func_validate_lampconch_1per_completeness(df_upload_lamp()$Survey_Data, df_upload_lamp()$Sites, df_upload_lamp()$Habitat_Types)
        if (!completeness_passed) {
            shinyalert("Attention!",
                text = paste(validation_message_completeness, "Please ensure all required columns are present prior to validation."),
                confirmButtonText = "I Understand", confirmButtonCol = "#FFA400", type = "warning", size = "m", html = TRUE
            )
        } else {
            validation_passed <- func_validate_lampconch_1per_check(df_upload_lamp()$Survey_Data, df_upload_lamp()$Sites, df_upload_lamp()$Habitat_Types)
            validation_message_surveydata <- func_validate_lampconch_1per_surveydata(df_upload_lamp()$Survey_Data, df_upload_lamp()$Sites)
            validation_message_sites <- func_validate_lampconch_1per_sites(df_upload_lamp()$Survey_Data, df_upload_lamp()$Sites, df_upload_lamp()$Habitat_Types)
            validation_message <- c(
                if (length(validation_message_surveydata) > 0 & length(validation_message_sites) > 0) {
                    paste0(
                        "Survey Data Sheet:", "<br><br>", validation_message_surveydata, "<br><br>",
                        "Sites Sheet:", "<br><br>", validation_message_sites, "<br><br>"
                    )
                } else if (length(validation_message_surveydata) > 0) {
                    paste0("Survey Data Sheet:", "<br><br>", validation_message_surveydata, "<br><br>")
                } else if (length(validation_message_sites) > 0) {
                    paste0("Sites Sheet:", "<br><br>", validation_message_sites, "<br><br>")
                }
            )
            if (validation_passed | length(validation_message) == 0) {
                shinyalert("Success!", "Validation Successful!",
                    confirmButtonText = "Great!", confirmButtonCol = "#00AE46", type = "success", size = "s"
                )
            } else {
                shinyalert("Attention!",
                    text = validation_message,
                    confirmButtonText = "I Understand", confirmButtonCol = "#FFA400", type = "warning", size = "m", html = TRUE
                )
            }
        }
    })
    observeEvent(input$validate_spag, {
        shinyalert("Success!", "Validation Successful!")
    })

    # Create reports
    output$report_fisheries <- downloadHandler(
        filename = function() {
            report_file <- switch(input$period_fisheries,
                "One Season" = "report_fisheries_1per.Rmd",
                "Multiple Seasons" = "report_fisheries_multiper.Rmd",
                "Multiple Years" = "report_fisheries_multiyear.Rmd"
            )
            gsub(".Rmd", ".docx", report_file)
        },
        content = function(file) {
            report_file <- switch(input$period_fisheries,
                "One Season" = "report_fisheries_1per.Rmd",
                "Multiple Seasons" = "report_fisheries_multiper.Rmd",
                "Multiple Years" = "report_fisheries_multiyear.Rmd"
            )
            src <- normalizePath(c(
                paste0("reports/", report_file),
                "reports/report_template.docx",
                "www/images/TASA_logo_full_color.png"
            ))
            owd <- setwd(tempdir())
            on.exit(setwd(owd))
            file.copy(src, c(report_file, "report_template.docx", "TASA_logo_full_color.png"), overwrite = TRUE)
            out <- render(
                report_file,
                params = list(user_name = input$name, datafile = df_upload_fisheries()),
                envir = new.env(parent = globalenv())
            )
            file.rename(out, file)
        }
    )

    output$report_fisher <- downloadHandler(
        filename = function() {
            report_file <- switch(input$period_fisher,
                "One Season" = "report_fisher_1per.Rmd",
                "Multiple Seasons" = "report_fisher_multiper.Rmd",
                "Multiple Years" = "report_fisher_multiyear.Rmd"
            )
            gsub(".Rmd", ".docx", report_file)
        },
        content = function(file) {
            report_file <- switch(input$period_fisher,
                "One Season" = "report_fisher_1per.Rmd",
                "Multiple Seasons" = "report_fisher_multiper.Rmd",
                "Multiple Years" = "report_fisher_multiyear.Rmd"
            )
            src <- normalizePath(c(
                paste0("reports/", report_file),
                "reports/report_template.docx",
                "www/images/TASA_logo_full_color.png"
            ))
            owd <- setwd(tempdir())
            on.exit(setwd(owd))
            file.copy(src, c(report_file, "report_template.docx", "TASA_logo_full_color.png"), overwrite = TRUE)
            out <- render(
                report_file,
                params = list(user_name = input$name, datafile = df_upload_fisher()),
                envir = new.env(parent = globalenv())
            )
            file.rename(out, file)
        }
    )

    output$report_lamp <- downloadHandler(
        filename = function() {
            report_file <- switch(paste(input$datatype_lamp, input$period_lamp, sep = "_"),
                "Conch_One Period" = "report_lampconch_1per.Rmd",
                "Conch_Multiple Periods" = "report_lampconch_multiper.Rmd",
                "General LAMP_One Period" = "report_lampgen_1per.Rmd",
                "General LAMP_Multiple Periods" = "report_lampgen_multiper.Rmd"
            )
            gsub(".Rmd", ".docx", report_file)
        },
        content = function(file) {
            report_file <- switch(paste(input$datatype_lamp, input$period_lamp, sep = "_"),
                "Conch_One Period" = "report_lampconch_1per.Rmd",
                "Conch_Multiple Periods" = "report_lampconch_multiper.Rmd",
                "General LAMP_One Period" = "report_lampgen_1per.Rmd",
                "General LAMP_Multiple Periods" = "report_lampgen_multiper.Rmd"
            )
            src <- normalizePath(c(paste0("reports/", report_file), "reports/report_template.docx", "www/images/TAMR_map.jpg", "theme.r"))
            owd <- setwd(tempdir())
            on.exit(setwd(owd))
            file.copy(src, c(report_file, "report_template.docx", "TAMR_map.jpg", "theme.r"), overwrite = TRUE)
            out <- render(
                report_file,
                params = list(user_name = input$name, datafile_name = input$upload_lamp$name, datafile = df_upload_lamp()),
                envir = new.env(parent = globalenv())
            )
            file.rename(out, file)
        }
    )


    output$report_spag <- downloadHandler(
        filename = function() {
            report_file <- switch(paste(input$datatype_spag, input$period_spag, sep = "_"),
                "Visual Census_One Year" = "report_spagvis_1per.Rmd",
                "Visual Census_Multiple Years" = "report_spagvis_multiper.Rmd",
                "Laser_One Year" = "report_spaglaser_1per.Rmd",
                "Laser_Multiple Years" = "report_spaglaser_multiper.Rmd"
            )
            gsub(".Rmd", ".docx", report_file)
        },
        content = function(file) {
            report_file <- switch(paste(input$datatype_spag, input$period_spag, sep = "_"),
                "Visual Census_One Year" = "report_spagvis_1per.Rmd",
                "Visual Census_Multiple Years" = "report_spagvis_multiper.Rmd",
                "Laser_One Year" = "report_spaglaser_1per.Rmd",
                "Laser_Multiple Years" = "report_spaglaser_multiper.Rmd"
            )
            src <- normalizePath(c(
                paste0("reports/", report_file),
                "reports/report_template.docx",
                "www/images/TASA_logo_full_color.png"
            ))
            owd <- setwd(tempdir())
            on.exit(setwd(owd))
            file.copy(src, c(report_file, "report_template.docx", "TASA_logo_full_color.png"), overwrite = TRUE)
            out <- render(
                report_file,
                params = list(user_name = input$name, datafile = df_upload_spag()),
                envir = new.env(parent = globalenv())
            )
            file.rename(out, file)
        }
    )


    # Observe action button "Manual" on home page
    observeEvent(input$go_to_manual_tab, {
        updateTabsetPanel(session = session, "navbar_page", selected = "Manual")
    })

    output$template_list_table <- renderUI({
        link_text <- paste(readLines("text/links.txt"))
        tags$table(
            class = "templates-table",
            tags$thead(
                tags$tr(
                    tags$th("Datatype"),
                    tags$th("Subtype"),
                    tags$th("Link to Template")
                )
            ),
            tags$tbody(
                # Iterates through lists to create table info
                lapply(1:6, function(i) {
                    tags$tr(
                        tags$td(c("Fisheries Catch", "Fisher Catch", "LAMP", "LAMP", "SPAG", "SPAG")[i]),
                        tags$td(c("-", "-", "Conch", "General", "Visual Census", "Laser Data")[i]),
                        tags$td(
                            tags$a(href = link_text[i + 1], "View Template", target = "_blank")
                        )
                    )
                })
            )
        )
    })
}
