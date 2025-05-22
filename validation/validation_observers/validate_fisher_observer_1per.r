## validate_fisher_observer_1per.r

observeEvent(input$validate_fisher_1per, {
    source("validation/validate_fisher_1per.r")
    datatype <- input$datatype_fisher_1per
    df_list <- df_upload_fisher_1per()
    data_sheet <- df_list[[datatype]]
    sheets_passed <- func_validate_fisher_1per_sheets_check(df_list, datatype)
    validation_message_sheets <- func_validate_fisher_1per_sheets(df_list, datatype)
    if (!sheets_passed) {
        shinyalert("Alert!",
            text = paste(validation_message_sheets, "Please ensure all required sheets are present prior to validation."),
            confirmButtonText = "I Understand", confirmButtonCol = "#FF747E", type = "error", size = "m", html = TRUE
        )
    } else {
        completeness_passed <- func_validate_fisher_1per_completeness_check(data_sheet, datatype)
        validation_message_completeness <- func_validate_fisher_1per_completeness(data_sheet, datatype)
        if (!completeness_passed) {
            shinyalert("Alert!",
                text = paste(validation_message_completeness, "Please ensure all required columns are present prior to validation."),
                confirmButtonText = "I Understand", confirmButtonCol = "#FF747E", type = "error", size = "m", html = TRUE
            )
        } else {
            if (input$datatype_fisher_1per == "Lobster") {
                validation_passed <- func_validate_fisher_1per_lobster_check(df_list$Lobster)
                validation_message_lobster <- func_validate_fisher_1per_lobster(df_list$Lobster)
                validation_message <- ""
                if (length(validation_message_lobster) > 0 && validation_message_lobster != "") {
                    validation_message <- paste0("Lobster Sheet:", "<br><br>", validation_message_lobster, "<br><br>")
                }
            }
            if (input$datatype_fisher_1per == "Conch") {
                validation_passed <- func_validate_fisher_1per_conch_check(df_list$Conch)
                validation_message_conch <- func_validate_fisher_1per_conch(df_list$Conch)
                validation_message <- ""
                if (length(validation_message_conch) > 0 && validation_message_conch != "") {
                    validation_message <- paste0("Conch Sheet:", "<br><br>", validation_message_conch, "<br><br>")
                }
            }
            if (input$datatype_fisher_1per == "Finfish") {
                validation_passed <- func_validate_fisher_1per_finfish_check(df_list$Finfish)
                validation_message_finfish <- func_validate_fisher_1per_finfish(df_list$Finfish)
                validation_message <- ""
                if (length(validation_message_finfish) > 0 && validation_message_finfish != "") {
                    validation_message <- paste0("Finfish Sheet:", "<br><br>", validation_message_finfish, "<br><br>")
                }
            }
            if (validation_passed || nchar(validation_message) == 0) {
                shinyalert("Success!", "Validation Successful!",
                    confirmButtonText = "Great!", confirmButtonCol = "#00AE46", type = "success", size = "s",
                    enableCustomization("fisher_1per"),
                    nameLengthCheck(input$fisher_1per_name, "fisher_1per")
                )
            } else {
                shinyalert("Attention!",
                    text = validation_message,
                    confirmButtonText = "I Understand", confirmButtonCol = "#FFA400", type = "warning", size = "m", html = TRUE,
                    enableCustomization("fisher_1per"),
                    nameLengthCheck(input$fisher_1per_name, "fisher_1per")
                )
            }
        }
    }
})
