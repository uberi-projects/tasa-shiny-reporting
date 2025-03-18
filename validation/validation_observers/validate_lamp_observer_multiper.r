## validate_lamp_observer_multiper.r

# datafiles:
# df_upload_lamp_multiper1()
# df_upload_lamp_multiper2()
# df_upload_lamp_multiper3() (optional)
# df_upload_lamp_multiper4() (optional)

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
        if (!is.null(input$upload_lamp_multiper3)) {
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
        if (!is.null(input$upload_lamp_multiper4)) {
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
        shinyalert("Notice!", "Validation has not been implemented for LAMP General Multi-Year as of yet!",
            confirmButtonText = "I Understand", confirmButtonCol = "#cde9f0", type = "info", size = "s"
        )
        enableCustomization("lamp_multiper")
    }
})
