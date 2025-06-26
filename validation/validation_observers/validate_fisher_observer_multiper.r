## validate_fisher_observer_multiper.r

observeEvent(input$validate_fisher_multiper, {
    # Validate Fisher Catch multiper
    source("validation/validate_fisher_1per.r")
    validation_status <- c()
    datatype <- input$datatype_fisher_multiper

    # Validate Fisher Catch multiper file 1
    file1_valid <- FALSE
    df_list <- df_upload_fisher_multiper1()
    data_sheet <- df_list[[datatype]]
    sheets_passed <- func_validate_fisher_1per_sheets_check(df_list, datatype)
    validation_message_sheets <- func_validate_fisher_1per_sheets(df_list, datatype)
    if (!sheets_passed) {
        shinyalert("Alert! File 1!",
            text = paste(validation_message_sheets, "Please ensure all required sheets are present prior to validation."),
            confirmButtonText = "I Understand", confirmButtonCol = "#FF747E", type = "error", size = "m", html = TRUE
        )
    } else {
        completeness_passed <- func_validate_fisher_1per_completeness_check(data_sheet, datatype)
        validation_message_completeness <- func_validate_fisher_1per_completeness(data_sheet, datatype)
        if (!completeness_passed) {
            shinyalert("Alert! File 1!",
                text = paste(validation_message_completeness, "Please ensure all required columns are present prior to validation."),
                confirmButtonText = "I Understand", confirmButtonCol = "#FF747E", type = "error", size = "m", html = TRUE
            )
        } else {
            if (input$datatype_fisher_multiper == "Lobster") {
                validation_passed <- func_validate_fisher_1per_lobster_check(df_list$Lobster)
                validation_message_lobster <- func_validate_fisher_1per_lobster(df_list$Lobster)
                validation_message <- ""
                if (length(validation_message_lobster) > 0 && validation_message_lobster != "") {
                    validation_message <- paste0("Lobster Sheet:", "<br><br>", validation_message_lobster, "<br><br>")
                }
            }
            if (input$datatype_fisher_multiper == "Conch") {
                validation_passed <- func_validate_fisher_1per_conch_check(df_list$Conch)
                validation_message_conch <- func_validate_fisher_1per_conch(df_list$Conch)
                validation_message <- ""
                if (length(validation_message_conch) > 0 && validation_message_conch != "") {
                    validation_message <- paste0("Conch Sheet:", "<br><br>", validation_message_conch, "<br><br>")
                }
            }
            if (input$datatype_fisher_multiper == "Finfish") {
                validation_passed <- func_validate_fisher_1per_finfish_check(df_list$Finfish)
                validation_message_finfish <- func_validate_fisher_1per_finfish(df_list$Finfish)
                validation_message <- ""
                if (length(validation_message_finfish) > 0 && validation_message_finfish != "") {
                    validation_message <- paste0("Finfish Sheet:", "<br><br>", validation_message_finfish, "<br><br>")
                }
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
    # Validate Fisher Catch multiper file 2
    file2_valid <- FALSE
    df_list <- df_upload_fisher_multiper2()
    data_sheet <- df_list[[datatype]]
    sheets_passed <- func_validate_fisher_1per_sheets_check(df_list, datatype)
    validation_message_sheets <- func_validate_fisher_1per_sheets(df_list, datatype)
    if (!sheets_passed) {
        shinyalert("Alert! File 2!",
            text = paste(validation_message_sheets, "Please ensure all required sheets are present prior to validation."),
            confirmButtonText = "I Understand", confirmButtonCol = "#FF747E", type = "error", size = "m", html = TRUE
        )
    } else {
        completeness_passed <- func_validate_fisher_1per_completeness_check(data_sheet, datatype)
        validation_message_completeness <- func_validate_fisher_1per_completeness(data_sheet, datatype)
        if (!completeness_passed) {
            shinyalert("Alert! File 2!",
                text = paste(validation_message_completeness, "Please ensure all required columns are present prior to validation."),
                confirmButtonText = "I Understand", confirmButtonCol = "#FF747E", type = "error", size = "m", html = TRUE
            )
        } else {
            if (input$datatype_fisher_multiper == "Lobster") {
                validation_passed <- func_validate_fisher_1per_lobster_check(df_list$Lobster)
                validation_message_lobster <- func_validate_fisher_1per_lobster(df_list$Lobster)
                validation_message <- ""
                if (length(validation_message_lobster) > 0 && validation_message_lobster != "") {
                    validation_message <- paste0("Lobster Sheet:", "<br><br>", validation_message_lobster, "<br><br>")
                }
            }
            if (input$datatype_fisher_multiper == "Conch") {
                validation_passed <- func_validate_fisher_1per_conch_check(df_list$Conch)
                validation_message_conch <- func_validate_fisher_1per_conch(df_list$Conch)
                validation_message <- ""
                if (length(validation_message_conch) > 0 && validation_message_conch != "") {
                    validation_message <- paste0("Conch Sheet:", "<br><br>", validation_message_conch, "<br><br>")
                }
            }
            if (input$datatype_fisher_multiper == "Finfish") {
                validation_passed <- func_validate_fisher_1per_finfish_check(df_list$Finfish)
                validation_message_finfish <- func_validate_fisher_1per_finfish(df_list$Finfish)
                validation_message <- ""
                if (length(validation_message_finfish) > 0 && validation_message_finfish != "") {
                    validation_message <- paste0("Finfish Sheet:", "<br><br>", validation_message_finfish, "<br><br>")
                }
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
    if (fisher_multiper3_flag()) {
        # Validate Fisher Catch multiper file 3
        file3_valid <- FALSE
        df_list <- df_upload_fisher_multiper3()
        data_sheet <- df_list[[datatype]]
        sheets_passed <- func_validate_fisher_1per_sheets_check(df_list, datatype)
        validation_message_sheets <- func_validate_fisher_1per_sheets(df_list, datatype)
        if (!sheets_passed) {
            shinyalert("Alert! File 3!",
                text = paste(validation_message_sheets, "Please ensure all required sheets are present prior to validation."),
                confirmButtonText = "I Understand", confirmButtonCol = "#FF747E", type = "error", size = "m", html = TRUE
            )
        } else {
            completeness_passed <- func_validate_fisher_1per_completeness_check(data_sheet, datatype)
            validation_message_completeness <- func_validate_fisher_1per_completeness(data_sheet, datatype)
            if (!completeness_passed) {
                shinyalert("Alert! File 3!",
                    text = paste(validation_message_completeness, "Please ensure all required columns are present prior to validation."),
                    confirmButtonText = "I Understand", confirmButtonCol = "#FF747E", type = "error", size = "m", html = TRUE
                )
            } else {
                if (input$datatype_fisher_multiper == "Lobster") {
                    validation_passed <- func_validate_fisher_1per_lobster_check(df_list$Lobster)
                    validation_message_lobster <- func_validate_fisher_1per_lobster(df_list$Lobster)
                    validation_message <- ""
                    if (length(validation_message_lobster) > 0 && validation_message_lobster != "") {
                        validation_message <- paste0("Lobster Sheet:", "<br><br>", validation_message_lobster, "<br><br>")
                    }
                }
                if (input$datatype_fisher_multiper == "Conch") {
                    validation_passed <- func_validate_fisher_1per_conch_check(df_list$Conch)
                    validation_message_conch <- func_validate_fisher_1per_conch(df_list$Conch)
                    validation_message <- ""
                    if (length(validation_message_conch) > 0 && validation_message_conch != "") {
                        validation_message <- paste0("Conch Sheet:", "<br><br>", validation_message_conch, "<br><br>")
                    }
                }
                if (input$datatype_fisher_multiper == "Finfish") {
                    validation_passed <- func_validate_fisher_1per_finfish_check(df_list$Finfish)
                    validation_message_finfish <- func_validate_fisher_1per_finfish(df_list$Finfish)
                    validation_message <- ""
                    if (length(validation_message_finfish) > 0 && validation_message_finfish != "") {
                        validation_message <- paste0("Finfish Sheet:", "<br><br>", validation_message_finfish, "<br><br>")
                    }
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
    if (fisher_multiper4_flag()) {
        # Validate Fisher Catch multiper file 4
        file4_valid <- FALSE
        df_list <- df_upload_fisher_multiper4()
        data_sheet <- df_list[[datatype]]
        sheets_passed <- func_validate_fisher_1per_sheets_check(df_list, datatype)
        validation_message_sheets <- func_validate_fisher_1per_sheets(df_list, datatype)
        if (!sheets_passed) {
            shinyalert("Alert! File 4!",
                text = paste(validation_message_sheets, "Please ensure all required sheets are present prior to validation."),
                confirmButtonText = "I Understand", confirmButtonCol = "#FF747E", type = "error", size = "m", html = TRUE
            )
        } else {
            completeness_passed <- func_validate_fisher_1per_completeness_check(data_sheet, datatype)
            validation_message_completeness <- func_validate_fisher_1per_completeness(data_sheet, datatype)
            if (!completeness_passed) {
                shinyalert("Alert! File 4!",
                    text = paste(validation_message_completeness, "Please ensure all required columns are present prior to validation."),
                    confirmButtonText = "I Understand", confirmButtonCol = "#FF747E", type = "error", size = "m", html = TRUE
                )
            } else {
                if (input$datatype_fisher_multiper == "Lobster") {
                    validation_passed <- func_validate_fisher_1per_lobster_check(df_list$Lobster)
                    validation_message_lobster <- func_validate_fisher_1per_lobster(df_list$Lobster)
                    validation_message <- ""
                    if (length(validation_message_lobster) > 0 && validation_message_lobster != "") {
                        validation_message <- paste0("Lobster Sheet:", "<br><br>", validation_message_lobster, "<br><br>")
                    }
                }
                if (input$datatype_fisher_multiper == "Conch") {
                    validation_passed <- func_validate_fisher_1per_conch_check(df_list$Conch)
                    validation_message_conch <- func_validate_fisher_1per_conch(df_list$Conch)
                    validation_message <- ""
                    if (length(validation_message_conch) > 0 && validation_message_conch != "") {
                        validation_message <- paste0("Conch Sheet:", "<br><br>", validation_message_conch, "<br><br>")
                    }
                }
                if (input$datatype_fisher_multiper == "Finfish") {
                    validation_passed <- func_validate_fisher_1per_finfish_check(df_list$Finfish)
                    validation_message_finfish <- func_validate_fisher_1per_finfish(df_list$Finfish)
                    validation_message <- ""
                    if (length(validation_message_finfish) > 0 && validation_message_finfish != "") {
                        validation_message <- paste0("Finfish Sheet:", "<br><br>", validation_message_finfish, "<br><br>")
                    }
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
        enableCustomization("fisher_multiper")
        nameLengthCheck(input$fisher_multiper_name, "fisher_multiper")
    }
})
