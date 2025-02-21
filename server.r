## server.R

# Attach packages ---------------------------
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
        sheets_available <- excel_sheets(input$upload_lamp$datapath)
        data_list <- list()
        if (input$datatype_lamp == "Conch") {
            if ("Survey Data" %in% sheets_available) {
                data_list$Survey_Data <- read_excel(input$upload_lamp$datapath, sheet = "Survey Data", na = nas)
            }
            if ("Sites" %in% sheets_available) {
                data_list$Sites <- read_excel(input$upload_lamp$datapath, sheet = "Sites", na = nas)
            }
            if ("Habitat Types" %in% sheets_available) {
                data_list$Habitat_Types <- read_excel(input$upload_lamp$datapath, sheet = "Habitat Types", na = nas)
            }
            return(data_list)
        } else {
            data_list <- list()
            if ("Species" %in% sheets_available) {
                data_list$Species <- read_excel(input$upload_lamp$datapath, sheet = "Species", na = nas)
            }
            if ("Sites" %in% sheets_available) {
                data_list$Sites <- read_excel(input$upload_lamp$datapath, sheet = "Sites", na = nas)
            }
            if ("Finfish" %in% sheets_available) {
                data_list$Finfish <- read_excel(input$upload_lamp$datapath, sheet = "Finfish", na = nas)
            }
            if ("Conch" %in% sheets_available) {
                data_list$Conch <- read_excel(input$upload_lamp$datapath, sheet = "Conch", na = nas)
            }
            if ("Lobster" %in% sheets_available) {
                data_list$Lobster <- read_excel(input$upload_lamp$datapath, sheet = "Lobster", na = nas)
            }
            if ("Diadema and Crab" %in% sheets_available) {
                data_list$Diadema_Crab <- read_excel(input$upload_lamp$datapath, sheet = "Diadema and Crab", na = nas)
            }
            return(data_list)
        }
    })
    df_upload_spag <- reactive({
        req(input$upload_spag)
        read.csv(input$upload_spag$datapath)
    })

    # Validate dataframes
    observeEvent(input$validate_fisheries, {
        shinyalert("Success!", "Validation Successful!",
            confirmButtonText = "Great!", confirmButtonCol = "#00AE46", type = "success", size = "s"
        )
        # If Validation Passes "no function yet"
        shinyjs::enable("fisheries_name")
        shinyjs::hide("fisheries_input_box_cover")
    })
    observeEvent(input$validate_fisher, {
        shinyalert("Success!", "Validation Successful!",
            confirmButtonText = "Great!", confirmButtonCol = "#00AE46", type = "success", size = "s"
        )
        # If Validation Passes
        shinyjs::enable("fisher_name")
        shinyjs::hide("fisher_input_box_cover")
    })
    observeEvent(input$validate_lamp, {
        source("validation/validate_lampconch_1per.r")
        sheets_passed <- func_validate_lampconch_1per_sheets_check(df_upload_lamp())
        validation_message_sheets <- func_validate_lampconch_1per_sheets(df_upload_lamp())
        if (!sheets_passed) {
            shinyalert("Alert!",
                text = paste(validation_message_sheets, "Please ensure all required sheets are present prior to validation."),
                confirmButtonText = "I Understand", confirmButtonCol = "#FF747E", type = "error", size = "m", html = TRUE
            )
        } else {
            completeness_passed <- func_validate_lampconch_1per_completeness_check(df_upload_lamp()$Survey_Data, df_upload_lamp()$Sites, df_upload_lamp()$Habitat_Types)
            validation_message_completeness <- func_validate_lampconch_1per_completeness(df_upload_lamp()$Survey_Data, df_upload_lamp()$Sites, df_upload_lamp()$Habitat_Types)
            if (!completeness_passed) {
                shinyalert("Alert!",
                    text = paste(validation_message_completeness, "Please ensure all required columns are present prior to validation."),
                    confirmButtonText = "I Understand", confirmButtonCol = "#FF747E", type = "error", size = "m", html = TRUE
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
                        confirmButtonText = "Great!", confirmButtonCol = "#00AE46", type = "success", size = "s",
                        shinyjs::enable("lamp_name"),
                        shinyjs::hide("lamp_input_box_cover")
                    )
                } else {
                    shinyalert("Attention!",
                        text = validation_message,
                        confirmButtonText = "I Understand", confirmButtonCol = "#FFA400", type = "warning", size = "m", html = TRUE
                    )
                }
            }
        }
    })
    observeEvent(input$validate_spag, {
        shinyalert("Success!", "Validation Successful!",
            confirmButtonText = "Great!", confirmButtonCol = "#00AE46", type = "success", size = "s"
        )
        # If Validation Passes
        shinyjs::enable("spag_name")
        shinyjs::hide("spag_input_box_cover")
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
                params = list(user_name = input$fisheries_name, datafile = df_upload_fisheries()),
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
                params = list(user_name = input$fisher_name, datafile = df_upload_fisher()),
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
            shapefiles <- list.files("shapefiles", full.names = TRUE)
            normalized_shapefiles <- normalizePath(shapefiles)
            src <- normalizePath(c(
                paste0("reports/", report_file),
                "reports/report_template.docx",
                "www/images/TAMR_map.jpg",
                "theme.r", "map.r",
                normalized_shapefiles
            ))
            owd <- setwd(tempdir())
            on.exit(setwd(owd))
            file.copy(src, c(report_file, "report_template.docx", "TAMR_map.jpg", "theme.r", "map.r", basename(shapefiles)), overwrite = TRUE)
            out <- render(
                report_file,
                params = list(user_name = input$lamp_name, datafile_name = input$upload_lamp$name, datafile = df_upload_lamp()),
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
                params = list(user_name = input$spag_name, datafile = df_upload_spag()),
                envir = new.env(parent = globalenv())
            )
            file.rename(out, file)
        }
    )

    # Enable/disable input boxes
    observeEvent(input$upload_fisheries, {
        if (!is.null(input$upload_fisheries)) {
            shinyjs::enable("validate_fisheries")
            shinyjs::hide("fisheries_validation_box_cover")
        }
    })
    observeEvent(input$upload_fisher, {
        if (!is.null(input$upload_fisher)) {
            shinyjs::enable("validate_fisher")
            shinyjs::hide("fisher_validation_box_cover")
        }
    })
    observeEvent(input$upload_lamp, {
        if (!is.null(input$upload_lamp)) {
            shinyjs::enable("validate_lamp")
            shinyjs::hide("lamp_validation_box_cover")
        }
    })
    observeEvent(input$upload_spag, {
        if (!is.null(input$upload_spag)) {
            shinyjs::enable("validate_spag")
            shinyjs::hide("spag_validation_box_cover")
        }
    })

    # Observe action button "Manual" on home page
    observeEvent(input$go_to_manual_tab, {
        updateTabsetPanel(session = session, "navbar_page", selected = "Manual")
    })

    # Render list of data templates
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
