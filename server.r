## server.R

# Attach packages ---------------------------
library(readxl)

# Define server ---------------------------
server <- function(input, output, session) {
    shinyjs::hide("feedback-content-box")

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
            if ("Biomass" %in% sheets_available) {
                data_list$Biomass <- read_excel(input$upload_lamp$datapath, sheet = "Biomass", na = nas)
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
        shinyalert("Notice!", "Validation has not been implemented for this dataset as of yet!",
            confirmButtonText = "I Understand", confirmButtonCol = "#cde9f0", type = "info", size = "s"
        )
        enableCustomization("fisheries")
    })

    observeEvent(input$validate_fisher, {
        shinyalert("Notice!", "Validation has not been implemented for this dataset as of yet!",
            confirmButtonText = "I Understand", confirmButtonCol = "#cde9f0", type = "info", size = "s"
        )
        enableCustomization("fisher")
    })

    observeEvent(input$validate_lamp, {
        # Validate LAMP Conch 1per
        if (input$datatype_lamp == "Conch") {
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
                    validation_message <- ""
                    if (length(validation_message_surveydata) > 0 && validation_message_surveydata != "") {
                        validation_message <- paste0("Survey Data Sheet:", "<br><br>", validation_message_surveydata, "<br><br>")
                    }
                    if (length(validation_message_sites) > 0 && validation_message_sites != "") {
                        validation_message <- paste0(validation_message, "Sites Sheet:", "<br><br>", validation_message_sites, "<br><br>")
                    }
                    if (validation_passed || nchar(validation_message) == 0) {
                        shinyalert("Success!", "Validation Successful!",
                            confirmButtonText = "Great!", confirmButtonCol = "#00AE46", type = "success", size = "s",
                            enableCustomization("lamp")
                        )
                    } else {
                        shinyalert("Attention!",
                            text = validation_message,
                            confirmButtonText = "I Understand", confirmButtonCol = "#FFA400", type = "warning", size = "m", html = TRUE,
                            enableCustomization("lamp")
                        )
                    }
                }
            }

            # Validate LAMP General 1per
        } else {
            source("validation/validate_lampgeneral_1per.r")
            sheets_passed <- func_validate_lampgeneral_1per_sheets_check(df_upload_lamp())
            validation_message_sheets <- func_validate_lampgeneral_1per_sheets(df_upload_lamp())
            if (!sheets_passed) {
                shinyalert("Alert!",
                    text = paste(validation_message_sheets, "Please ensure all required sheets are present prior to validation."),
                    confirmButtonText = "I Understand", confirmButtonCol = "#FF747E", type = "error", size = "m", html = TRUE
                )
            } else {
                completeness_passed <- func_validate_lampgeneral_1per_completeness_check(df_upload_lamp()$Sites)
                validation_message_completeness <- func_validate_lampgeneral_1per_completeness(df_upload_lamp()$Sites)
                if (!completeness_passed) {
                    shinyalert("Alert!",
                        text = paste(validation_message_completeness, "Please ensure all required columns are present prior to validation."),
                        confirmButtonText = "I Understand", confirmButtonCol = "#FF747E", type = "error", size = "m", html = TRUE
                    )
                } else {
                    validation_passed <- func_validate_lampgeneral_1per_check(
                        df_upload_lamp(), df_upload_lamp()$Species, df_upload_lamp()$Sites, df_upload_lamp()$Finfish, df_upload_lamp()$Conch, df_upload_lamp()$Lobster, df_upload_lamp()$Diadema_Crab
                    )
                    validation_message_species <- func_validate_lampgeneral_1per_species(df_upload_lamp()$Species)
                    validation_message_sites <- func_validate_lampgeneral_1per_sites(df_upload_lamp()$Sites)
                    validation_message_finfish <- if ("Finfish" %in% names(df_upload_lamp())) {
                        func_validate_lampgeneral_1per_finfish(df_upload_lamp()$Finfish, df_upload_lamp()$Sites, df_upload_lamp()$Species)
                    } else {
                        NULL
                    }
                    validation_message_conch <- if ("Conch" %in% names(df_upload_lamp())) {
                        func_validate_lampgeneral_1per_conch(df_upload_lamp()$Conch, df_upload_lamp()$Sites)
                    } else {
                        NULL
                    }
                    validation_message_lobster <- if ("Lobster" %in% names(df_upload_lamp())) {
                        func_validate_lampgeneral_1per_lobster(df_upload_lamp()$Lobster, df_upload_lamp()$Sites)
                    } else {
                        NULL
                    }
                    validation_message_diadema_crab <- if ("Diadema_Crab" %in% names(df_upload_lamp())) {
                        func_validate_lampgeneral_1per_diadema_crab(df_upload_lamp()$Diadema_Crab, df_upload_lamp()$Sites)
                    } else {
                        NULL
                    }
                    validation_message <- ""
                    if (length(validation_message_species) > 0 && validation_message_species != "") {
                        validation_message <- paste0("Species Sheet:", "<br><br>", validation_message_species, "<br><br>")
                    }
                    if (length(validation_message_sites) > 0 && validation_message_sites != "") {
                        validation_message <- paste0(validation_message, "Sites Sheet:", "<br><br>", validation_message_sites, "<br><br>")
                    }
                    if (!is.null(validation_message_finfish) && length(validation_message_finfish) > 0 && validation_message_finfish != "") {
                        validation_message <- paste0(validation_message, "Finfish Sheet:", "<br><br>", validation_message_finfish, "<br><br>")
                    }
                    if (!is.null(validation_message_conch) && length(validation_message_conch) > 0 && validation_message_conch != "") {
                        validation_message <- paste0(validation_message, "Conch Sheet:", "<br><br>", validation_message_conch, "<br><br>")
                    }
                    if (!is.null(validation_message_lobster) && length(validation_message_lobster) > 0 && validation_message_lobster != "") {
                        validation_message <- paste0(validation_message, "Lobster Sheet:", "<br><br>", validation_message_lobster, "<br><br>")
                    }
                    if (!is.null(validation_message_diadema_crab) && length(validation_message_diadema_crab) > 0 && validation_message_diadema_crab != "") {
                        validation_message <- paste0(validation_message, "Diadema and Crab Sheet:", "<br><br>", validation_message_diadema_crab, "<br><br>")
                    }
                    if (validation_passed || nchar(validation_message) == 0) {
                        shinyalert("Success!", "Validation Successful!",
                            confirmButtonText = "Great!", confirmButtonCol = "#00AE46", type = "success", size = "s",
                            enableCustomization("lamp")
                        )
                    } else {
                        shinyalert("Attention!",
                            text = validation_message,
                            confirmButtonText = "I Understand", confirmButtonCol = "#FFA400", type = "warning", size = "m", html = TRUE,
                            enableCustomization("lamp")
                        )
                    }
                }
            }
        }
    })

    observeEvent(input$validate_spag, {
        shinyalert("Notice!", "Validation has not been implemented for this dataset as of yet!",
            confirmButtonText = "I Understand", confirmButtonCol = "#cde9f0", type = "info", size = "s"
        )
        enableCustomization("spag")
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

    # Observe Periods
    observeEvent(input$period_fisheries, {
        disableCustomization("fisheries")
    })
    observeEvent(input$period_fisher, {
        disableCustomization("fisher")
    })
    observeEvent(input$period_lamp, {
        disableCustomization("lamp")
    })
    observeEvent(input$period_spag, {
        disableCustomization("spag")
    })

    # Observe Datatype
    observeEvent(input$datatype_lamp, {
        disableCustomization("lamp")
    })
    observeEvent(input$datatype_spag, {
        disableCustomization("spag")
    })

    # Observe Upload
    observeEvent(input$upload_fisheries, {
        if (!is.null(input$upload_fisheries)) {
            enableValidate("fisheries")
            disableCustomization("fisheries")
        }
    })
    observeEvent(input$upload_fisher, {
        if (!is.null(input$upload_fisher)) {
            enableValidate("fisher")
            disableCustomization("fisher")
        }
    })
    observeEvent(input$upload_lamp, {
        if (!is.null(input$upload_lamp)) {
            enableValidate("lamp")
            disableCustomization("lamp")
        }
    })
    observeEvent(input$upload_spag, {
        if (!is.null(input$upload_spag)) {
            enableValidate("spag")
            disableCustomization("spag")
        }
    })


    observeEvent(input$feedback_opn_bttn, {
        shinyjs::show("feedback-content-box")
        shinyjs::hide("feedback_opn_bttn")
    })

    observeEvent(input$feedback_bttn, {
        if (is.null(input$feedback_text) || trimws(input$feedback_text) == "") {
            shinyalert("Error!", "Please enter feedback before submitting.",
                confirmButtonText = "Okay!", confirmButtonCol = "#E90C0C", type = "", size = "s"
            )
        } else {
            shinyjs::hide("feedback-content-box")
            shinyjs::show("feedback_opn_bttn")
            updateTextInput(session, "feedback_text", value = "")
            shinyalert("Success!", "Feedback Submitted!",
                confirmButtonText = "Okay!", confirmButtonCol = "#00AE46", type = "", size = "s"
            )
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

    # Helper Functions
    disableCustomization <- function(reportType) {
        shinyjs::disable(paste0(reportType, "_name"))
        shinyjs::disable(paste0("report_", reportType))
        shinyjs::show(paste0(reportType, "_input_box_cover"))
    }
    enableCustomization <- function(reportType) {
        shinyjs::enable(paste0(reportType, "_name"))
        shinyjs::enable(paste0("report_", reportType))
        shinyjs::hide(paste0(reportType, "_input_box_cover"))
    }
    enableValidate <- function(reportType) {
        shinyjs::enable(paste0("validate_", reportType))
        shinyjs::hide(paste0(reportType, "_validation_box_cover"))
    }
}
