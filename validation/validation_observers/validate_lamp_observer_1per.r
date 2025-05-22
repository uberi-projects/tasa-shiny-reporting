## validate_lamp_observer_1per.r

observeEvent(input$validate_lamp_1per, {
    # Validate LAMP Conch 1per
    if (input$datatype_lamp_1per == "Conch") {
        source("validation/validate_lampconch_1per.r")
        sheets_passed <- func_validate_lampconch_1per_sheets_check(df_upload_lamp_1per())
        validation_message_sheets <- func_validate_lampconch_1per_sheets(df_upload_lamp_1per())
        if (!sheets_passed) {
            shinyalert("Alert!",
                text = paste(validation_message_sheets, "Please ensure all required sheets are present prior to validation."),
                confirmButtonText = "I Understand", confirmButtonCol = "#FF747E", type = "error", size = "m", html = TRUE
            )
        } else {
            completeness_passed <- func_validate_lampconch_1per_completeness_check(df_upload_lamp_1per()$Survey_Data, df_upload_lamp_1per()$Sites, df_upload_lamp_1per()$Habitat_Types)
            validation_message_completeness <- func_validate_lampconch_1per_completeness(df_upload_lamp_1per()$Survey_Data, df_upload_lamp_1per()$Sites, df_upload_lamp_1per()$Habitat_Types)
            if (!completeness_passed) {
                shinyalert("Alert!",
                    text = paste(validation_message_completeness, "Please ensure all required columns are present prior to validation."),
                    confirmButtonText = "I Understand", confirmButtonCol = "#FF747E", type = "error", size = "m", html = TRUE
                )
            } else {
                validation_passed <- func_validate_lampconch_1per_check(df_upload_lamp_1per()$Survey_Data, df_upload_lamp_1per()$Sites, df_upload_lamp_1per()$Habitat_Types)
                validation_message_surveydata <- func_validate_lampconch_1per_surveydata(df_upload_lamp_1per()$Survey_Data, df_upload_lamp_1per()$Sites)
                validation_message_sites <- func_validate_lampconch_1per_sites(df_upload_lamp_1per()$Survey_Data, df_upload_lamp_1per()$Sites, df_upload_lamp_1per()$Habitat_Types)
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
                        enableCustomization("lamp_1per")
                    )
                } else {
                    shinyalert("Attention!",
                        text = validation_message,
                        confirmButtonText = "I Understand", confirmButtonCol = "#FFA400", type = "warning", size = "m", html = TRUE,
                        enableCustomization("lamp_1per")
                    )
                }
            }
        }

        # Validate LAMP General 1per
    } else {
        source("validation/validate_lampgeneral_1per.r")
        sheets_passed <- func_validate_lampgeneral_1per_sheets_check(df_upload_lamp_1per())
        validation_message_sheets <- func_validate_lampgeneral_1per_sheets(df_upload_lamp_1per())
        if (!sheets_passed) {
            shinyalert("Alert!",
                text = paste(validation_message_sheets, "Please ensure all required sheets are present prior to validation."),
                confirmButtonText = "I Understand", confirmButtonCol = "#FF747E", type = "error", size = "m", html = TRUE
            )
        } else {
            completeness_passed <- func_validate_lampgeneral_1per_completeness_check(df_upload_lamp_1per())
            validation_message_completeness <- func_validate_lampgeneral_1per_completeness(df_upload_lamp_1per())
            if (!completeness_passed) {
                shinyalert("Alert!",
                    text = paste(validation_message_completeness, "Please ensure all required columns are present prior to validation."),
                    confirmButtonText = "I Understand", confirmButtonCol = "#FF747E", type = "error", size = "m", html = TRUE
                )
            } else {
                validation_passed <- func_validate_lampgeneral_1per_check(df_upload_lamp_1per())
                validation_message_species <- func_validate_lampgeneral_1per_species(df_upload_lamp_1per()$Species)
                validation_message_sites <- func_validate_lampgeneral_1per_sites(df_upload_lamp_1per()$Sites)
                validation_message_finfish <- if ("Finfish" %in% names(df_upload_lamp_1per())) {
                    func_validate_lampgeneral_1per_finfish(df_upload_lamp_1per()$Finfish, df_upload_lamp_1per()$Sites, df_upload_lamp_1per()$Species)
                } else {
                    NULL
                }
                validation_message_conch <- if ("Conch" %in% names(df_upload_lamp_1per())) {
                    func_validate_lampgeneral_1per_conch(df_upload_lamp_1per()$Conch, df_upload_lamp_1per()$Sites)
                } else {
                    NULL
                }
                validation_message_lobster <- if ("Lobster" %in% names(df_upload_lamp_1per())) {
                    func_validate_lampgeneral_1per_lobster(df_upload_lamp_1per()$Lobster, df_upload_lamp_1per()$Sites)
                } else {
                    NULL
                }
                validation_message_diadema_crab <- if ("Diadema_Crab" %in% names(df_upload_lamp_1per())) {
                    func_validate_lampgeneral_1per_diadema_crab(df_upload_lamp_1per()$Diadema_Crab, df_upload_lamp_1per()$Sites)
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
                        enableCustomization("lamp_1per"),
                        nameLengthCheck(input$lamp_1per_name, "lamp_1per")
                    )
                } else {
                    shinyalert("Attention!",
                        text = validation_message,
                        confirmButtonText = "I Understand", confirmButtonCol = "#FFA400", type = "warning", size = "m", html = TRUE,
                        enableCustomization("lamp_1per"),
                        nameLengthCheck(input$fisher_1per_name, "fisher_1per")
                    )
                }
            }
        }
    }
})
