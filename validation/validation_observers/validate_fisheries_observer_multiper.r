## validate_fisheries_observer_multiper.r

observeEvent(input$validate_fisheries_multiper, {
    # Validate Fisheries Catch multiper
    source("validation/validate_fisheries_1per.r")
    validation_status <- c()
    datatype <- input$datatype_fisheries_multiper

    # Validate Fisheries Catch multiper file 1
    file1_valid <- FALSE
    df_list <- df_upload_fisheries_multiper1()
    data_sheet <- df_list[[datatype]]
    sheets_passed <- func_validate_fisheries_1per_sheets_check(df_list, datatype)
    validation_message_sheets <- func_validate_fisheries_1per_sheets(df_list, datatype)
    if (!sheets_passed) {
        shinyalert("Alert! File 1!",
            text = paste(validation_message_sheets, "Please ensure all required sheets are present prior to validation."),
            confirmButtonText = "I Understand", confirmButtonCol = "#FF747E", type = "error", size = "m", html = TRUE
        )
    } else {
        completeness_passed <- func_validate_fisheries_1per_completeness_check(data_sheet, datatype)
        validation_message_completeness <- func_validate_fisheries_1per_completeness(data_sheet, datatype)
        if (!completeness_passed) {
            shinyalert("Alert! File 1!",
                text = paste(validation_message_completeness, "Please ensure all required columns are present prior to validation."),
                confirmButtonText = "I Understand", confirmButtonCol = "#FF747E", type = "error", size = "m", html = TRUE
            )
        } else {
            if (input$datatype_fisheries_multiper == "Lobster") {
                validation_passed <- func_validate_fisheries_1per_lobster_check(df_list$Lobster)
                validation_message_lobster <- func_validate_fisheries_1per_lobster(df_list$Lobster)
                validation_message <- ""
                if (length(validation_message_lobster) > 0 && validation_message_lobster != "") {
                    validation_message <- paste0("Lobster Sheet:", "<br><br>", validation_message_lobster, "<br><br>")
                }
            }
            if (input$datatype_fisheries_multiper == "Conch") {
                validation_passed <- func_validate_fisheries_1per_conch_check(df_list$Conch)
                validation_message_conch <- func_validate_fisheries_1per_conch(df_list$Conch)
                validation_message <- ""
                if (length(validation_message_conch) > 0 && validation_message_conch != "") {
                    validation_message <- paste0("Conch Sheet:", "<br><br>", validation_message_conch, "<br><br>")
                }
            }
            if (input$datatype_fisheries_multiper == "Finfish") {
                validation_passed <- func_validate_fisheries_1per_finfish_check(df_list$Finfish)
                validation_message_finfish <- func_validate_fisheries_1per_finfish(df_list$Finfish)
                validation_message <- ""
                if (length(validation_message_finfish) > 0 && validation_message_finfish != "") {
                    validation_message <- paste0("Finfish Sheet:", "<br><br>", validation_message_finfish, "<br><br>")
                }
            }
            if (validation_passed || nchar(validation_message) == 0) {
                shinyalert("Success! File 1!", "Validation Successful!",
                    confirmButtonText = "Great!", confirmButtonCol = "#00AE46", type = "success", size = "s",
                    enableCustomization("fisheries_1per"),
                    nameLengthCheck(input$fisheries_1per_name, "fisheries_1per")
                )
                file1_valid <- TRUE
            } else {
                shinyalert("Attention! File 1!",
                    text = validation_message,
                    confirmButtonText = "I Understand", confirmButtonCol = "#FFA400", type = "warning", size = "m", html = TRUE,
                    enableCustomization("fisheries_1per"),
                    nameLengthCheck(input$fisheries_1per_name, "fisheries_1per")
                )
                file1_valid <- TRUE
            }
        }
    }
    validation_status <- c(validation_status, file1_valid)

    # Validate Fisheries Catch multiper file 2
    file2_valid <- FALSE
    df_list <- df_upload_fisheries_multiper2()
    data_sheet <- df_list[[datatype]]
    sheets_passed <- func_validate_fisheries_1per_sheets_check(df_list, datatype)
    validation_message_sheets <- func_validate_fisheries_1per_sheets(df_list, datatype)
    if (!sheets_passed) {
        shinyalert("Alert! File 2!",
            text = paste(validation_message_sheets, "Please ensure all required sheets are present prior to validation."),
            confirmButtonText = "I Understand", confirmButtonCol = "#FF747E", type = "error", size = "m", html = TRUE
        )
    } else {
        completeness_passed <- func_validate_fisheries_1per_completeness_check(data_sheet, datatype)
        validation_message_completeness <- func_validate_fisheries_1per_completeness(data_sheet, datatype)
        if (!completeness_passed) {
            shinyalert("Alert! File 2!",
                text = paste(validation_message_completeness, "Please ensure all required columns are present prior to validation."),
                confirmButtonText = "I Understand", confirmButtonCol = "#FF747E", type = "error", size = "m", html = TRUE
            )
        } else {
            if (input$datatype_fisheries_multiper == "Lobster") {
                validation_passed <- func_validate_fisheries_1per_lobster_check(df_list$Lobster)
                validation_message_lobster <- func_validate_fisheries_1per_lobster(df_list$Lobster)
                validation_message <- ""
                if (length(validation_message_lobster) > 0 && validation_message_lobster != "") {
                    validation_message <- paste0("Lobster Sheet:", "<br><br>", validation_message_lobster, "<br><br>")
                }
            }
            if (input$datatype_fisheries_multiper == "Conch") {
                validation_passed <- func_validate_fisheries_1per_conch_check(df_list$Conch)
                validation_message_conch <- func_validate_fisheries_1per_conch(df_list$Conch)
                validation_message <- ""
                if (length(validation_message_conch) > 0 && validation_message_conch != "") {
                    validation_message <- paste0("Conch Sheet:", "<br><br>", validation_message_conch, "<br><br>")
                }
            }
            if (input$datatype_fisheries_multiper == "Finfish") {
                validation_passed <- func_validate_fisheries_1per_finfish_check(df_list$Finfish)
                validation_message_finfish <- func_validate_fisheries_1per_finfish(df_list$Finfish)
                validation_message <- ""
                if (length(validation_message_finfish) > 0 && validation_message_finfish != "") {
                    validation_message <- paste0("Finfish Sheet:", "<br><br>", validation_message_finfish, "<br><br>")
                }
            }
            if (validation_passed || nchar(validation_message) == 0) {
                shinyalert("Success! File 2!", "Validation Successful!",
                    confirmButtonText = "Great!", confirmButtonCol = "#00AE46", type = "success", size = "s",
                    enableCustomization("fisheries_1per"),
                    nameLengthCheck(input$fisheries_1per_name, "fisheries_1per")
                )
                file2_valid <- TRUE
            } else {
                shinyalert("Attention! File 2!",
                    text = validation_message,
                    confirmButtonText = "I Understand", confirmButtonCol = "#FFA400", type = "warning", size = "m", html = TRUE,
                    enableCustomization("fisheries_1per"),
                    nameLengthCheck(input$fisheries_1per_name, "fisheries_1per")
                )
                file2_valid <- TRUE
            }
        }
    }
    validation_status <- c(validation_status, file2_valid)

    # Validate Fisheries Catch multiper file 3
    if (fisheries_multiper3_flag()) {
        file3_valid <- FALSE
        df_list <- df_upload_fisheries_multiper3()
        data_sheet <- df_list[[datatype]]
        sheets_passed <- func_validate_fisheries_1per_sheets_check(df_list, datatype)
        validation_message_sheets <- func_validate_fisheries_1per_sheets(df_list, datatype)
        if (!sheets_passed) {
            shinyalert("Alert! File 3!",
                text = paste(validation_message_sheets, "Please ensure all required sheets are present prior to validation."),
                confirmButtonText = "I Understand", confirmButtonCol = "#FF747E", type = "error", size = "m", html = TRUE
            )
        } else {
            completeness_passed <- func_validate_fisheries_1per_completeness_check(data_sheet, datatype)
            validation_message_completeness <- func_validate_fisheries_1per_completeness(data_sheet, datatype)
            if (!completeness_passed) {
                shinyalert("Alert! File 3!",
                    text = paste(validation_message_completeness, "Please ensure all required columns are present prior to validation."),
                    confirmButtonText = "I Understand", confirmButtonCol = "#FF747E", type = "error", size = "m", html = TRUE
                )
            } else {
                if (input$datatype_fisheries_multiper == "Lobster") {
                    validation_passed <- func_validate_fisheries_1per_lobster_check(df_list$Lobster)
                    validation_message_lobster <- func_validate_fisheries_1per_lobster(df_list$Lobster)
                    validation_message <- ""
                    if (length(validation_message_lobster) > 0 && validation_message_lobster != "") {
                        validation_message <- paste0("Lobster Sheet:", "<br><br>", validation_message_lobster, "<br><br>")
                    }
                }
                if (input$datatype_fisheries_multiper == "Conch") {
                    validation_passed <- func_validate_fisheries_1per_conch_check(df_list$Conch)
                    validation_message_conch <- func_validate_fisheries_1per_conch(df_list$Conch)
                    validation_message <- ""
                    if (length(validation_message_conch) > 0 && validation_message_conch != "") {
                        validation_message <- paste0("Conch Sheet:", "<br><br>", validation_message_conch, "<br><br>")
                    }
                }
                if (input$datatype_fisheries_multiper == "Finfish") {
                    validation_passed <- func_validate_fisheries_1per_finfish_check(df_list$Finfish)
                    validation_message_finfish <- func_validate_fisheries_1per_finfish(df_list$Finfish)
                    validation_message <- ""
                    if (length(validation_message_finfish) > 0 && validation_message_finfish != "") {
                        validation_message <- paste0("Finfish Sheet:", "<br><br>", validation_message_finfish, "<br><br>")
                    }
                }
                if (validation_passed || nchar(validation_message) == 0) {
                    shinyalert("Success! File 3!", "Validation Successful!",
                        confirmButtonText = "Great!", confirmButtonCol = "#00AE46", type = "success", size = "s",
                        enableCustomization("fisheries_1per"),
                        nameLengthCheck(input$fisheries_1per_name, "fisheries_1per")
                    )
                    file3_valid <- TRUE
                } else {
                    shinyalert("Attention! File 3!",
                        text = validation_message,
                        confirmButtonText = "I Understand", confirmButtonCol = "#FFA400", type = "warning", size = "m", html = TRUE,
                        enableCustomization("fisheries_1per"),
                        nameLengthCheck(input$fisheries_1per_name, "fisheries_1per")
                    )
                    file3_valid <- TRUE
                }
            }
        }
        validation_status <- c(validation_status, file3_valid)
    }

    # Validate Fisheries Catch multiper file 4
    if (fisheries_multiper4_flag()) {
        file4_valid <- FALSE
        df_list <- df_upload_fisheries_multiper4()
        data_sheet <- df_list[[datatype]]
        sheets_passed <- func_validate_fisheries_1per_sheets_check(df_list, datatype)
        validation_message_sheets <- func_validate_fisheries_1per_sheets(df_list, datatype)
        if (!sheets_passed) {
            shinyalert("Alert! File 4!",
                text = paste(validation_message_sheets, "Please ensure all required sheets are present prior to validation."),
                confirmButtonText = "I Understand", confirmButtonCol = "#FF747E", type = "error", size = "m", html = TRUE
            )
        } else {
            completeness_passed <- func_validate_fisheries_1per_completeness_check(data_sheet, datatype)
            validation_message_completeness <- func_validate_fisheries_1per_completeness(data_sheet, datatype)
            if (!completeness_passed) {
                shinyalert("Alert! File 4!",
                    text = paste(validation_message_completeness, "Please ensure all required columns are present prior to validation."),
                    confirmButtonText = "I Understand", confirmButtonCol = "#FF747E", type = "error", size = "m", html = TRUE
                )
            } else {
                if (input$datatype_fisheries_multiper == "Lobster") {
                    validation_passed <- func_validate_fisheries_1per_lobster_check(df_list$Lobster)
                    validation_message_lobster <- func_validate_fisheries_1per_lobster(df_list$Lobster)
                    validation_message <- ""
                    if (length(validation_message_lobster) > 0 && validation_message_lobster != "") {
                        validation_message <- paste0("Lobster Sheet:", "<br><br>", validation_message_lobster, "<br><br>")
                    }
                }
                if (input$datatype_fisheries_multiper == "Conch") {
                    validation_passed <- func_validate_fisheries_1per_conch_check(df_list$Conch)
                    validation_message_conch <- func_validate_fisheries_1per_conch(df_list$Conch)
                    validation_message <- ""
                    if (length(validation_message_conch) > 0 && validation_message_conch != "") {
                        validation_message <- paste0("Conch Sheet:", "<br><br>", validation_message_conch, "<br><br>")
                    }
                }
                if (input$datatype_fisheries_multiper == "Finfish") {
                    validation_passed <- func_validate_fisheries_1per_finfish_check(df_list$Finfish)
                    validation_message_finfish <- func_validate_fisheries_1per_finfish(df_list$Finfish)
                    validation_message <- ""
                    if (length(validation_message_finfish) > 0 && validation_message_finfish != "") {
                        validation_message <- paste0("Finfish Sheet:", "<br><br>", validation_message_finfish, "<br><br>")
                    }
                }
                if (validation_passed || nchar(validation_message) == 0) {
                    shinyalert("Success! File 4!", "Validation Successful!",
                        confirmButtonText = "Great!", confirmButtonCol = "#00AE46", type = "success", size = "s",
                        enableCustomization("fisheries_1per"),
                        nameLengthCheck(input$fisheries_1per_name, "fisheries_1per")
                    )
                    file4_valid <- TRUE
                } else {
                    shinyalert("Attention! File 4!",
                        text = validation_message,
                        confirmButtonText = "I Understand", confirmButtonCol = "#FFA400", type = "warning", size = "m", html = TRUE,
                        enableCustomization("fisheries_1per"),
                        nameLengthCheck(input$fisheries_1per_name, "fisheries_1per")
                    )
                    file4_valid <- TRUE
                }
            }
        }
        validation_status <- c(validation_status, file4_valid)
    }
    if (all(validation_status)) {
        enableCustomization("fisheries_multiper")
        nameLengthCheck(input$fisheries_multiper_name, "fisheries_multiper")
    }
})
