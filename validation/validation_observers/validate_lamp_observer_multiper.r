## validate_lamp_observer_multiper.r

observeEvent(input$validate_lamp_multiper, {
    # Validate LAMP Conch multiper
    if (input$datatype_lamp_multiper == "Conch") {
        source("validation/validate_lampconch_1per.r")
        validation_status <- c()

        # Validate LAMP Conch multiper file 1
        file1_valid <- FALSE
        sheets_passed <- func_validate_lampconch_1per_sheets_check(df_upload_lamp_multiper1())
        validation_message_sheets <- func_validate_lampconch_1per_sheets(df_upload_lamp_multiper1())
        if (!sheets_passed) {
            shinyalert("Alert! File 1!",
                text = paste(validation_message_sheets, "Please ensure all required sheets are present prior to validation."),
                confirmButtonText = "I Understand", confirmButtonCol = "#FF747E", type = "error", size = "m", html = TRUE
            )
        } else {
            completeness_passed <- func_validate_lampconch_1per_completeness_check(df_upload_lamp_multiper1()$Survey_Data, df_upload_lamp_multiper1()$Sites, df_upload_lamp_multiper1()$Habitat_Types)
            validation_message_completeness <- func_validate_lampconch_1per_completeness(df_upload_lamp_multiper1()$Survey_Data, df_upload_lamp_multiper1()$Sites, df_upload_lamp_multiper1()$Habitat_Types)
            if (!completeness_passed) {
                shinyalert("Alert! File 1!",
                    text = paste(validation_message_completeness, "Please ensure all required columns are present prior to validation."),
                    confirmButtonText = "I Understand", confirmButtonCol = "#FF747E", type = "error", size = "m", html = TRUE
                )
            } else {
                validation_passed <- func_validate_lampconch_1per_check(df_upload_lamp_multiper1()$Survey_Data, df_upload_lamp_multiper1()$Sites, df_upload_lamp_multiper1()$Habitat_Types)
                validation_message_surveydata <- func_validate_lampconch_1per_surveydata(df_upload_lamp_multiper1()$Survey_Data, df_upload_lamp_multiper1()$Sites)
                validation_message_sites <- func_validate_lampconch_1per_sites(df_upload_lamp_multiper1()$Survey_Data, df_upload_lamp_multiper1()$Sites, df_upload_lamp_multiper1()$Habitat_Types)
                validation_message <- ""
                if (length(validation_message_surveydata) > 0 && validation_message_surveydata != "") {
                    validation_message <- paste0("Survey Data Sheet:", "<br><br>", validation_message_surveydata, "<br><br>")
                }
                if (length(validation_message_sites) > 0 && validation_message_sites != "") {
                    validation_message <- paste0(validation_message, "Sites Sheet:", "<br><br>", validation_message_sites, "<br><br>")
                }
                if (validation_passed || nchar(validation_message) == 0) {
                    shinyalert("Success! File 1!", "Validation Successful!",
                        confirmButtonText = "Great!", confirmButtonCol = "#00AE46", type = "success", size = "s"
                    )
                    file1_valid <- TRUE
                } else {
                    shinyalert("Attention! File 1!",
                        text = validation_message,
                        confirmButtonText = "I Understand", confirmButtonCol = "#FFA400", type = "warning", size = "m", html = TRUE
                    )
                    file1_valid <- TRUE
                }
            }
        }
        validation_status <- c(validation_status, file1_valid)

        # Validate LAMP Conch multiper file 2
        file2_valid <- FALSE
        sheets_passed <- func_validate_lampconch_1per_sheets_check(df_upload_lamp_multiper2())
        validation_message_sheets <- func_validate_lampconch_1per_sheets(df_upload_lamp_multiper2())
        if (!sheets_passed) {
            shinyalert("Alert! File 2!",
                text = paste(validation_message_sheets, "Please ensure all required sheets are present prior to validation."),
                confirmButtonText = "I Understand", confirmButtonCol = "#FF747E", type = "error", size = "m", html = TRUE
            )
        } else {
            completeness_passed <- func_validate_lampconch_1per_completeness_check(df_upload_lamp_multiper2()$Survey_Data, df_upload_lamp_multiper2()$Sites, df_upload_lamp_multiper2()$Habitat_Types)
            validation_message_completeness <- func_validate_lampconch_1per_completeness(df_upload_lamp_multiper2()$Survey_Data, df_upload_lamp_multiper2()$Sites, df_upload_lamp_multiper2()$Habitat_Types)
            if (!completeness_passed) {
                shinyalert("Alert! File 2!",
                    text = paste(validation_message_completeness, "Please ensure all required columns are present prior to validation."),
                    confirmButtonText = "I Understand", confirmButtonCol = "#FF747E", type = "error", size = "m", html = TRUE
                )
            } else {
                validation_passed <- func_validate_lampconch_1per_check(df_upload_lamp_multiper2()$Survey_Data, df_upload_lamp_multiper2()$Sites, df_upload_lamp_multiper2()$Habitat_Types)
                validation_message_surveydata <- func_validate_lampconch_1per_surveydata(df_upload_lamp_multiper2()$Survey_Data, df_upload_lamp_multiper2()$Sites)
                validation_message_sites <- func_validate_lampconch_1per_sites(df_upload_lamp_multiper2()$Survey_Data, df_upload_lamp_multiper2()$Sites, df_upload_lamp_multiper2()$Habitat_Types)
                validation_message <- ""
                if (length(validation_message_surveydata) > 0 && validation_message_surveydata != "") {
                    validation_message <- paste0("Survey Data Sheet:", "<br><br>", validation_message_surveydata, "<br><br>")
                }
                if (length(validation_message_sites) > 0 && validation_message_sites != "") {
                    validation_message <- paste0(validation_message, "Sites Sheet:", "<br><br>", validation_message_sites, "<br><br>")
                }
                if (validation_passed || nchar(validation_message) == 0) {
                    shinyalert("Success! File 2!", "Validation Successful!",
                        confirmButtonText = "Great!", confirmButtonCol = "#00AE46", type = "success", size = "s"
                    )
                    file2_valid <- TRUE
                } else {
                    shinyalert("Attention! File 2!",
                        text = validation_message,
                        confirmButtonText = "I Understand", confirmButtonCol = "#FFA400", type = "warning", size = "m", html = TRUE
                    )
                    file2_valid <- TRUE
                }
            }
        }
        validation_status <- c(validation_status, file2_valid)

        # Validate LAMP Conch multiper file 3
        if (lamp_multiper3_flag()) {
            file3_valid <- FALSE
            sheets_passed <- func_validate_lampconch_1per_sheets_check(df_upload_lamp_multiper3())
            validation_message_sheets <- func_validate_lampconch_1per_sheets(df_upload_lamp_multiper3())
            if (!sheets_passed) {
                shinyalert("Alert! File 3!",
                    text = paste(validation_message_sheets, "Please ensure all required sheets are present prior to validation."),
                    confirmButtonText = "I Understand", confirmButtonCol = "#FF747E", type = "error", size = "m", html = TRUE
                )
            } else {
                completeness_passed <- func_validate_lampconch_1per_completeness_check(df_upload_lamp_multiper3()$Survey_Data, df_upload_lamp_multiper3()$Sites, df_upload_lamp_multiper3()$Habitat_Types)
                validation_message_completeness <- func_validate_lampconch_1per_completeness(df_upload_lamp_multiper3()$Survey_Data, df_upload_lamp_multiper3()$Sites, df_upload_lamp_multiper3()$Habitat_Types)
                if (!completeness_passed) {
                    shinyalert("Alert! File 3!",
                        text = paste(validation_message_completeness, "Please ensure all required columns are present prior to validation."),
                        confirmButtonText = "I Understand", confirmButtonCol = "#FF747E", type = "error", size = "m", html = TRUE
                    )
                } else {
                    validation_passed <- func_validate_lampconch_1per_check(df_upload_lamp_multiper3()$Survey_Data, df_upload_lamp_multiper3()$Sites, df_upload_lamp_multiper3()$Habitat_Types)
                    validation_message_surveydata <- func_validate_lampconch_1per_surveydata(df_upload_lamp_multiper3()$Survey_Data, df_upload_lamp_multiper3()$Sites)
                    validation_message_sites <- func_validate_lampconch_1per_sites(df_upload_lamp_multiper3()$Survey_Data, df_upload_lamp_multiper3()$Sites, df_upload_lamp_multiper3()$Habitat_Types)
                    validation_message <- ""
                    if (length(validation_message_surveydata) > 0 && validation_message_surveydata != "") {
                        validation_message <- paste0("Survey Data Sheet:", "<br><br>", validation_message_surveydata, "<br><br>")
                    }
                    if (length(validation_message_sites) > 0 && validation_message_sites != "") {
                        validation_message <- paste0(validation_message, "Sites Sheet:", "<br><br>", validation_message_sites, "<br><br>")
                    }
                    if (validation_passed || nchar(validation_message) == 0) {
                        shinyalert("Success! File 3!", "Validation Successful!",
                            confirmButtonText = "Great!", confirmButtonCol = "#00AE46", type = "success", size = "s"
                        )
                        file3_valid <- TRUE
                    } else {
                        shinyalert("Attention! File 3!",
                            text = validation_message,
                            confirmButtonText = "I Understand", confirmButtonCol = "#FFA400", type = "warning", size = "m", html = TRUE
                        )
                        file3_valid <- TRUE
                    }
                }
            }
            validation_status <- c(validation_status, file3_valid)
        }

        # Validate LAMP Conch multiper file 4
        if (lamp_multiper4_flag()) {
            file4_valid <- FALSE
            sheets_passed <- func_validate_lampconch_1per_sheets_check(df_upload_lamp_multiper4())
            validation_message_sheets <- func_validate_lampconch_1per_sheets(df_upload_lamp_multiper4())
            if (!sheets_passed) {
                shinyalert("Alert! File 4!",
                    text = paste(validation_message_sheets, "Please ensure all required sheets are present prior to validation."),
                    confirmButtonText = "I Understand", confirmButtonCol = "#FF747E", type = "error", size = "m", html = TRUE
                )
            } else {
                completeness_passed <- func_validate_lampconch_1per_completeness_check(df_upload_lamp_multiper4()$Survey_Data, df_upload_lamp_multiper4()$Sites, df_upload_lamp_multiper4()$Habitat_Types)
                validation_message_completeness <- func_validate_lampconch_1per_completeness(df_upload_lamp_multiper4()$Survey_Data, df_upload_lamp_multiper4()$Sites, df_upload_lamp_multiper4()$Habitat_Types)
                if (!completeness_passed) {
                    shinyalert("Alert! File 4!",
                        text = paste(validation_message_completeness, "Please ensure all required columns are present prior to validation."),
                        confirmButtonText = "I Understand", confirmButtonCol = "#FF747E", type = "error", size = "m", html = TRUE
                    )
                } else {
                    validation_passed <- func_validate_lampconch_1per_check(df_upload_lamp_multiper4()$Survey_Data, df_upload_lamp_multiper4()$Sites, df_upload_lamp_multiper4()$Habitat_Types)
                    validation_message_surveydata <- func_validate_lampconch_1per_surveydata(df_upload_lamp_multiper4()$Survey_Data, df_upload_lamp_multiper4()$Sites)
                    validation_message_sites <- func_validate_lampconch_1per_sites(df_upload_lamp_multiper4()$Survey_Data, df_upload_lamp_multiper4()$Sites, df_upload_lamp_multiper4()$Habitat_Types)
                    validation_message <- ""
                    if (length(validation_message_surveydata) > 0 && validation_message_surveydata != "") {
                        validation_message <- paste0("Survey Data Sheet:", "<br><br>", validation_message_surveydata, "<br><br>")
                    }
                    if (length(validation_message_sites) > 0 && validation_message_sites != "") {
                        validation_message <- paste0(validation_message, "Sites Sheet:", "<br><br>", validation_message_sites, "<br><br>")
                    }
                    if (validation_passed || nchar(validation_message) == 0) {
                        shinyalert("Success! File 4!", "Validation Successful!",
                            confirmButtonText = "Great!", confirmButtonCol = "#00AE46", type = "success", size = "s"
                        )
                        file4_valid <- TRUE
                    } else {
                        shinyalert("Attention! File 4!",
                            text = validation_message,
                            confirmButtonText = "I Understand", confirmButtonCol = "#FFA400", type = "warning", size = "m", html = TRUE
                        )
                        file4_valid <- TRUE
                    }
                }
            }
            validation_status <- c(validation_status, file4_valid)
        }

        if (all(validation_status)) {
            enableCustomization("lamp_multiper")
        }

        # Validate LAMP General multiper
    } else {
        source("validation/validate_lampgeneral_1per.r")
        validation_status <- c()

        # Validate LAMP General multiper file 1
        file1_valid <- FALSE
        sheets_passed <- func_validate_lampgeneral_1per_sheets_check(df_upload_lamp_multiper1())
        validation_message_sheets <- func_validate_lampgeneral_1per_sheets(df_upload_lamp_multiper1())
        if (!sheets_passed) {
            shinyalert("Alert! File 1!",
                text = paste(validation_message_sheets, "Please ensure all required sheets are present prior to validation."),
                confirmButtonText = "I Understand", confirmButtonCol = "#FF747E", type = "error", size = "m", html = TRUE
            )
        } else {
            completeness_passed <- func_validate_lampgeneral_1per_completeness_check(df_upload_lamp_multiper1())
            validation_message_completeness <- func_validate_lampgeneral_1per_completeness(df_upload_lamp_multiper1())
            if (!completeness_passed) {
                shinyalert("Alert! File 1!",
                    text = paste(validation_message_completeness, "Please ensure all required columns are present prior to validation."),
                    confirmButtonText = "I Understand", confirmButtonCol = "#FF747E", type = "error", size = "m", html = TRUE
                )
            } else {
                validation_passed <- func_validate_lampgeneral_1per_check(df_upload_lamp_multiper1())
                validation_message_species <- func_validate_lampgeneral_1per_species(df_upload_lamp_multiper1()$Species)
                validation_message_sites <- func_validate_lampgeneral_1per_sites(df_upload_lamp_multiper1()$Sites)
                validation_message_finfish <- if ("Finfish" %in% names(df_upload_lamp_multiper1())) {
                    func_validate_lampgeneral_1per_finfish(df_upload_lamp_multiper1()$Finfish, df_upload_lamp_multiper1()$Sites, df_upload_lamp_multiper1()$Species)
                } else {
                    NULL
                }
                validation_message_conch <- if ("Conch" %in% names(df_upload_lamp_multiper1())) {
                    func_validate_lampgeneral_1per_conch(df_upload_lamp_multiper1()$Conch, df_upload_lamp_multiper1()$Sites)
                } else {
                    NULL
                }
                validation_message_lobster <- if ("Lobster" %in% names(df_upload_lamp_multiper1())) {
                    func_validate_lampgeneral_1per_lobster(df_upload_lamp_multiper1()$Lobster, df_upload_lamp_multiper1()$Sites)
                } else {
                    NULL
                }
                validation_message_diadema_crab <- if ("Diadema_Crab" %in% names(df_upload_lamp_multiper1())) {
                    func_validate_lampgeneral_1per_diadema_crab(df_upload_lamp_multiper1()$Diadema_Crab, df_upload_lamp_multiper1()$Sites)
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
                    shinyalert("Success! File 1!", "Validation Successful!",
                        confirmButtonText = "Great!", confirmButtonCol = "#00AE46", type = "success", size = "s"
                    )
                    file1_valid <- TRUE
                } else {
                    shinyalert("Attention! File 1!",
                        text = validation_message,
                        confirmButtonText = "I Understand", confirmButtonCol = "#FFA400", type = "warning", size = "m", html = TRUE
                    )
                    file1_valid <- TRUE
                }
            }
        }
        validation_status <- c(validation_status, file1_valid)

        # Validate LAMP General multiper file 2
        file2_valid <- FALSE
        sheets_passed <- func_validate_lampgeneral_1per_sheets_check(df_upload_lamp_multiper2())
        validation_message_sheets <- func_validate_lampgeneral_1per_sheets(df_upload_lamp_multiper2())
        if (!sheets_passed) {
            shinyalert("Alert! File 2!",
                text = paste(validation_message_sheets, "Please ensure all required sheets are present prior to validation."),
                confirmButtonText = "I Understand", confirmButtonCol = "#FF747E", type = "error", size = "m", html = TRUE
            )
        } else {
            completeness_passed <- func_validate_lampgeneral_1per_completeness_check(df_upload_lamp_multiper2())
            validation_message_completeness <- func_validate_lampgeneral_1per_completeness(df_upload_lamp_multiper2())
            if (!completeness_passed) {
                shinyalert("Alert! File 2!",
                    text = paste(validation_message_completeness, "Please ensure all required columns are present prior to validation."),
                    confirmButtonText = "I Understand", confirmButtonCol = "#FF747E", type = "error", size = "m", html = TRUE
                )
            } else {
                validation_passed <- func_validate_lampgeneral_1per_check(df_upload_lamp_multiper2())
                validation_message_species <- func_validate_lampgeneral_1per_species(df_upload_lamp_multiper2()$Species)
                validation_message_sites <- func_validate_lampgeneral_1per_sites(df_upload_lamp_multiper2()$Sites)
                validation_message_finfish <- if ("Finfish" %in% names(df_upload_lamp_multiper2())) {
                    func_validate_lampgeneral_1per_finfish(df_upload_lamp_multiper2()$Finfish, df_upload_lamp_multiper2()$Sites, df_upload_lamp_multiper2()$Species)
                } else {
                    NULL
                }
                validation_message_conch <- if ("Conch" %in% names(df_upload_lamp_multiper2())) {
                    func_validate_lampgeneral_1per_conch(df_upload_lamp_multiper2()$Conch, df_upload_lamp_multiper2()$Sites)
                } else {
                    NULL
                }
                validation_message_lobster <- if ("Lobster" %in% names(df_upload_lamp_multiper2())) {
                    func_validate_lampgeneral_1per_lobster(df_upload_lamp_multiper2()$Lobster, df_upload_lamp_multiper2()$Sites)
                } else {
                    NULL
                }
                validation_message_diadema_crab <- if ("Diadema_Crab" %in% names(df_upload_lamp_multiper2())) {
                    func_validate_lampgeneral_1per_diadema_crab(df_upload_lamp_multiper2()$Diadema_Crab, df_upload_lamp_multiper2()$Sites)
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
                    shinyalert("Success! File 2!", "Validation Successful!",
                        confirmButtonText = "Great!", confirmButtonCol = "#00AE46", type = "success", size = "s"
                    )
                    file2_valid <- TRUE
                } else {
                    shinyalert("Attention! File 2!",
                        text = validation_message,
                        confirmButtonText = "I Understand", confirmButtonCol = "#FFA400", type = "warning", size = "m", html = TRUE
                    )
                    file2_valid <- TRUE
                }
            }
        }
        validation_status <- c(validation_status, file2_valid)

        # Validate LAMP General multiper file 3
        if (lamp_multiper3_flag()) {
            file3_valid <- FALSE
            sheets_passed <- func_validate_lampgeneral_1per_sheets_check(df_upload_lamp_multiper3())
            validation_message_sheets <- func_validate_lampgeneral_1per_sheets(df_upload_lamp_multiper3())
            if (!sheets_passed) {
                shinyalert("Alert! File 3!",
                    text = paste(validation_message_sheets, "Please ensure all required sheets are present prior to validation."),
                    confirmButtonText = "I Understand", confirmButtonCol = "#FF747E", type = "error", size = "m", html = TRUE
                )
            } else {
                completeness_passed <- func_validate_lampgeneral_1per_completeness_check(df_upload_lamp_multiper3())
                validation_message_completeness <- func_validate_lampgeneral_1per_completeness(df_upload_lamp_multiper3())
                if (!completeness_passed) {
                    shinyalert("Alert! File 3!",
                        text = paste(validation_message_completeness, "Please ensure all required columns are present prior to validation."),
                        confirmButtonText = "I Understand", confirmButtonCol = "#FF747E", type = "error", size = "m", html = TRUE
                    )
                } else {
                    validation_passed <- func_validate_lampgeneral_1per_check(df_upload_lamp_multiper3())
                    validation_message_species <- func_validate_lampgeneral_1per_species(df_upload_lamp_multiper3()$Species)
                    validation_message_sites <- func_validate_lampgeneral_1per_sites(df_upload_lamp_multiper3()$Sites)
                    validation_message_finfish <- if ("Finfish" %in% names(df_upload_lamp_multiper3())) {
                        func_validate_lampgeneral_1per_finfish(df_upload_lamp_multiper3()$Finfish, df_upload_lamp_multiper3()$Sites, df_upload_lamp_multiper3()$Species)
                    } else {
                        NULL
                    }
                    validation_message_conch <- if ("Conch" %in% names(df_upload_lamp_multiper3())) {
                        func_validate_lampgeneral_1per_conch(df_upload_lamp_multiper3()$Conch, df_upload_lamp_multiper3()$Sites)
                    } else {
                        NULL
                    }
                    validation_message_lobster <- if ("Lobster" %in% names(df_upload_lamp_multiper3())) {
                        func_validate_lampgeneral_1per_lobster(df_upload_lamp_multiper3()$Lobster, df_upload_lamp_multiper3()$Sites)
                    } else {
                        NULL
                    }
                    validation_message_diadema_crab <- if ("Diadema_Crab" %in% names(df_upload_lamp_multiper3())) {
                        func_validate_lampgeneral_1per_diadema_crab(df_upload_lamp_multiper3()$Diadema_Crab, df_upload_lamp_multiper3()$Sites)
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
                        shinyalert("Success! File 3!", "Validation Successful!",
                            confirmButtonText = "Great!", confirmButtonCol = "#00AE46", type = "success", size = "s"
                        )
                        file3_valid <- TRUE
                    } else {
                        shinyalert("Attention! File 3!",
                            text = validation_message,
                            confirmButtonText = "I Understand", confirmButtonCol = "#FFA400", type = "warning", size = "m", html = TRUE
                        )
                        file3_valid <- TRUE
                    }
                }
            }
            validation_status <- c(validation_status, file3_valid)
        }

        # Validate LAMP General multiper file 4
        if (lamp_multiper4_flag()) {
            file3_valid <- FALSE
            sheets_passed <- func_validate_lampgeneral_1per_sheets_check(df_upload_lamp_multiper4())
            validation_message_sheets <- func_validate_lampgeneral_1per_sheets(df_upload_lamp_multiper4())
            if (!sheets_passed) {
                shinyalert("Alert! File 4!",
                    text = paste(validation_message_sheets, "Please ensure all required sheets are present prior to validation."),
                    confirmButtonText = "I Understand", confirmButtonCol = "#FF747E", type = "error", size = "m", html = TRUE
                )
            } else {
                completeness_passed <- func_validate_lampgeneral_1per_completeness_check(df_upload_lamp_multiper4())
                validation_message_completeness <- func_validate_lampgeneral_1per_completeness(df_upload_lamp_multiper4())
                if (!completeness_passed) {
                    shinyalert("Alert! File 4!",
                        text = paste(validation_message_completeness, "Please ensure all required columns are present prior to validation."),
                        confirmButtonText = "I Understand", confirmButtonCol = "#FF747E", type = "error", size = "m", html = TRUE
                    )
                } else {
                    validation_passed <- func_validate_lampgeneral_1per_check(df_upload_lamp_multiper4())
                    validation_message_species <- func_validate_lampgeneral_1per_species(df_upload_lamp_multiper4()$Species)
                    validation_message_sites <- func_validate_lampgeneral_1per_sites(df_upload_lamp_multiper4()$Sites)
                    validation_message_finfish <- if ("Finfish" %in% names(df_upload_lamp_multiper4())) {
                        func_validate_lampgeneral_1per_finfish(df_upload_lamp_multiper4()$Finfish, df_upload_lamp_multiper4()$Sites, df_upload_lamp_multiper4()$Species)
                    } else {
                        NULL
                    }
                    validation_message_conch <- if ("Conch" %in% names(df_upload_lamp_multiper4())) {
                        func_validate_lampgeneral_1per_conch(df_upload_lamp_multiper4()$Conch, df_upload_lamp_multiper4()$Sites)
                    } else {
                        NULL
                    }
                    validation_message_lobster <- if ("Lobster" %in% names(df_upload_lamp_multiper4())) {
                        func_validate_lampgeneral_1per_lobster(df_upload_lamp_multiper4()$Lobster, df_upload_lamp_multiper4()$Sites)
                    } else {
                        NULL
                    }
                    validation_message_diadema_crab <- if ("Diadema_Crab" %in% names(df_upload_lamp_multiper4())) {
                        func_validate_lampgeneral_1per_diadema_crab(df_upload_lamp_multiper4()$Diadema_Crab, df_upload_lamp_multiper4()$Sites)
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
                        shinyalert("Success! File 4!", "Validation Successful!",
                            confirmButtonText = "Great!", confirmButtonCol = "#00AE46", type = "success", size = "s"
                        )
                        file4_valid <- TRUE
                    } else {
                        shinyalert("Attention! File 4!",
                            text = validation_message,
                            confirmButtonText = "I Understand", confirmButtonCol = "#FFA400", type = "warning", size = "m", html = TRUE
                        )
                        file4_valid <- TRUE
                    }
                }
            }
            validation_status <- c(validation_status, file4_valid)
        }
        if (all(validation_status)) {
            enableCustomization("lamp_multiper")
        }
    }
})
