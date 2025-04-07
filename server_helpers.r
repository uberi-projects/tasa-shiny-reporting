## server_helpers.R

# Define helpers to enable/disable elements
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
disableValidate <- function(reportType) {
    shinyjs::disable(paste0("validate_", reportType))
    shinyjs::show(paste0(reportType, "_validation_box_cover"))
}
enableUpload <- function(reportType) {
    shinyjs::enable(paste0("upload_", reportType))
    shinyjs::hide(paste0(reportType, "_upload_box_cover"))
}
disableUpload <- function(reportType) {
    shinyjs::disable(paste0("upload_", reportType))
    shinyjs::show(paste0(reportType, "_upload_box_cover"))
}
enableUploadRemoveBttn <- function(reportType) {
    shinyjs::show(paste0("remove_", reportType, "_bttn"))
}
disableUploadRemoveBttn <- function(reportType) {
    shinyjs::hide(paste0("remove_", reportType, "_bttn"))
}
removeConfirmation <- function(reportType) {
    removeUI(selector = paste0("#ui_upload_", reportType, " > .file-error-button"))
    removeUI(selector = paste0("#ui_upload_", reportType, " > .file-confirmation-button"))
}
showLoaderBar <- function(reportType, session) {
    shinyjs::show(paste0(reportType, "_loader"))
    shinyjs::hide(paste0("report_", reportType))
    for (i in seq(0, 70, by = 10)) {
        Sys.sleep(0.1)
        session$sendCustomMessage("updateLoader", list(reportType = reportType, percentage = i))
    }
}
hideLoaderBar <- function(reportType, session) {
    session$sendCustomMessage("updateLoader", list(reportType = reportType, percentage = 100))
    Sys.sleep(0.2)
    shinyjs::show(paste0("report_", reportType))
    shinyjs::hide(paste0(reportType, "_loader"))
    session$sendCustomMessage("resetLoader", list(reportType = reportType))
}

# Define helpers to check datafile date
show_error <- function(message) {
    return(div(class = "file-error-button", p(class = "p-black", paste0("⚠️ ", message))))
}
show_critical_error <- function(message) {
    return(div(class = "file-critical-error-button", p(class = "p-black", paste0("⚠️ ", message))))
}
check_datafile_dates <- function(df, type, id, year_flag, period_flag) {
    if (is.null(df)) {
        return(show_error("No valid data uploaded"))
    }
    if (!"Date" %in% names(df) || all(is.na(df$Date))) {
        return(show_critical_error("No valid dates detected"))
    }
    if (!inherits(df$Date, "Date")) {
        df$Date <- suppressWarnings(as.Date(df$Date))
    }
    if (all(is.na(df$Date))) {
        return(show_critical_error("Could not interpret any dates"))
    }
    if (type == "year") {
        study_years <- format(range(df$Date, na.rm = TRUE), "%Y")
        if (study_years[1] != study_years[2]) {
            return(show_error(paste0("Multiple years: ", study_years[1], "-", study_years[2])))
        }
        return(div(
            class = "file-confirmation-button",
            p(class = "p-black", paste0("Year: ", study_years[1]))
        ))
    }
    if (type == "period") {
        study_periods <- format(range(df$Date, na.rm = TRUE), "%b %Y")
        if (study_periods[1] != study_periods[2]) {
            return(show_error(paste0("Multiple periods: ", study_periods[1], "-", study_periods[2])))
        }
        return(div(
            class = "file-confirmation-button",
            p(class = "p-black", paste0("Period: ", study_years[1]))
        ))
    }
}

check_datafiles_dates <- function(dfs, type, id, year_flag, period_flag) {
    year_flag(FALSE)
    period_flag(FALSE)
    if (is.null(dfs) || length(dfs) == 0) {
        return(show_critical_error("No valid data uploaded"))
    }
    find_date_column <- function(df) {
        if ("Date" %in% names(df)) {
            return(df)
        }
        return(NULL)
    }
    for (df in dfs) {
        valid_df <- find_date_column(df)
        if (!is.null(valid_df)) {
            date_char <- as.character(valid_df$Date)
            valid_dates <- suppressWarnings(as.Date(date_char, format = "%Y-%m-%d"))
            valid_dates <- valid_dates[!is.na(valid_dates)]
            if (length(valid_dates) == 0) {
                next
            }
            if (type == "year") {
                study_years <- format(range(valid_dates), "%Y")
                unique_study_years <- sort(unique(format(valid_dates, "%Y")))
                if (study_years[1] != study_years[2]) {
                    year_flag(TRUE)
                    return(div(
                        show_error(paste0("Multiple years: ", study_years[1], "-", study_years[2])),
                        div(
                            class = "input-list-content",
                            prettyRadioButtons(paste0(id, "year_selection"),
                                label = "Please select one year.",
                                choices = unique_study_years,
                                inline = TRUE
                            )
                        )
                    ))
                }
                return(div(
                    class = "file-confirmation-button",
                    p(class = "p-black", paste0("Year: ", study_years[1]))
                ))
            }
            if (type == "period") {
                study_periods <- format(range(valid_dates), "%b %Y")
                unique_study_periods <- sort(unique(format(valid_dates, "%b %Y")))
                if (study_periods[1] != study_periods[2]) {
                    period_flag(TRUE)
                    return(div(
                        show_error(paste0("Multiple periods: ", study_periods[1], "-", study_periods[2])),
                        div(
                            class = "input-list-content",
                            prettyRadioButtons(paste0(id, "period_selection"),
                                label = "Please select one period.",
                                choices = unique_study_periods,
                                inline = TRUE
                            )
                        )
                    ))
                }
                return(div(
                    class = "file-confirmation-button",
                    p(class = "p-black", paste0("Period: ", study_periods[1]))
                ))
            }
        }
    }
    return(show_critical_error("No valid dates detected"))
}

# Create helper to read Fisher data
read_fisher_data <- function(file_path, datatype) {
    nas <- c("NA", "N/A", "Unknown", "Missing", "None", "N/E")
    sheets_available <- excel_sheets(file_path)
    data_list <- list()
    if (datatype == "Conch") {
        if ("Conch" %in% sheets_available) {
            data_list$Conch <- read_excel(file_path, sheet = "Conch", na = nas, guess_max = min(1000, Inf))
        }
    } else if (datatype == "Lobster") {
        if ("Lobster" %in% sheets_available) {
            data_list$Lobster <- read_excel(file_path, sheet = "Lobster", na = nas, guess_max = min(1000, Inf))
        }
    } else if (datatype == "Finfish") {
        if ("Finfish" %in% sheets_available) {
            data_list$Finfish <- read_excel(file_path, sheet = "Finfish", na = nas, guess_max = min(1000, Inf))
        }
    }
    return(data_list)
}


# Create helper to read LAMP data
read_lamp_data <- function(file_path, datatype) {
    nas <- c("NA", "N/A", "Unknown", "Missing", "None", "N/E")
    sheets_available <- excel_sheets(file_path)
    data_list <- list()
    if (datatype == "Conch") {
        if ("Survey Data" %in% sheets_available) {
            data_list$Survey_Data <- read_excel(file_path, sheet = "Survey Data", na = nas, guess_max = min(1000, Inf))
        }
        if ("Sites" %in% sheets_available) {
            data_list$Sites <- read_excel(file_path, sheet = "Sites", na = nas, guess_max = min(1000, Inf))
        }
        if ("Habitat Types" %in% sheets_available) {
            data_list$Habitat_Types <- read_excel(file_path, sheet = "Habitat Types", na = nas, guess_max = min(1000, Inf))
        }
    } else {
        if ("Species" %in% sheets_available) {
            data_list$Species <- read_excel(file_path, sheet = "Species", na = nas, guess_max = min(1000, Inf))
        }
        if ("Biomass" %in% sheets_available) {
            data_list$Biomass <- read_excel(file_path, sheet = "Biomass", na = nas, guess_max = min(1000, Inf))
        }
        if ("Sites" %in% sheets_available) {
            data_list$Sites <- read_excel(file_path, sheet = "Sites", na = nas, guess_max = min(1000, Inf))
        }
        if ("Finfish" %in% sheets_available) {
            data_list$Finfish <- read_excel(file_path, sheet = "Finfish", na = nas, guess_max = min(1000, Inf))
        }
        if ("Conch" %in% sheets_available) {
            data_list$Conch <- read_excel(file_path, sheet = "Conch", na = nas, guess_max = min(1000, Inf))
        }
        if ("Lobster" %in% sheets_available) {
            data_list$Lobster <- read_excel(file_path, sheet = "Lobster", na = nas, guess_max = min(1000, Inf))
        }
        if ("Diadema and Crab" %in% sheets_available) {
            data_list$Diadema_Crab <- read_excel(file_path, sheet = "Diadema and Crab", na = nas, guess_max = min(1000, Inf))
        }
    }
    return(data_list)
}
