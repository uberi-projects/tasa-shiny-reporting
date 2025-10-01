## server_helpers.R

# Define helpers to enable/disable elements
disableCustomization <- function(reportType) {
    shinyjs::disable(paste0(reportType, "_name"))
    shinyjs::disable(paste0("report_", reportType))
    shinyjs::show(paste0(reportType, "_input_box_cover"))
    shinyjs::hide(paste0(reportType, "_generate_cover"))
}
enableCustomization <- function(reportType) {
    shinyjs::enable(paste0(reportType, "_name"))
    shinyjs::hide(paste0(reportType, "_input_box_cover"))
    shinyjs::show(paste0(reportType, "_generate_cover"))
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
    shinyjs::hide(paste0("figures_", reportType))
    for (i in seq(0, 70, by = 10)) {
        Sys.sleep(0.1)
        session$sendCustomMessage("updateLoader", list(reportType = reportType, percentage = i))
    }
}
hideLoaderBar <- function(reportType, session) {
    session$sendCustomMessage("updateLoader", list(reportType = reportType, percentage = 100))
    Sys.sleep(0.2)
    shinyjs::show(paste0("report_", reportType))
    shinyjs::show(paste0("figures_", reportType))
    shinyjs::hide(paste0(reportType, "_loader"))
    session$sendCustomMessage("resetLoader", list(reportType = reportType))
}
disableDownload <- function(reportType) {
    shinyjs::disable(paste0("report_", reportType))
    if (!grepl("multi", reportType)) {
        shinyjs::disable(paste0("figures_", reportType))
    }
    shinyjs::show(paste0(reportType, "_generate_cover"))
}
enableDownload <- function(reportType) {
    shinyjs::enable(paste0("report_", reportType))
    if (!grepl("multi", reportType)) {
        shinyjs::enable(paste0("figures_", reportType))
    }
    shinyjs::hide(paste0(reportType, "_generate_cover"))
}
nameLengthCheck <- function(inputName, reportType) {
    if (nchar(inputName) >= 2) {
        enableDownload(reportType)
    } else {
        disableDownload(reportType)
    }
}

# Define flags for datafiles
create_flags <- function(datatype, flag_type, env = .GlobalEnv) {
    prefixes <- c("1per", "multiper1", "multiper2", "multiper3", "multiper4")
    clean_flag <- gsub(" ", "_", flag_type)
    for (prefix in prefixes) {
        flag_name <- paste0(datatype, "_", prefix, "_", clean_flag, "_selection_flag")
        assign(flag_name, reactiveVal(FALSE), envir = env)
    }
}
set_flags_1per <- function(datatype, flag_type, boolean, env = .GlobalEnv) {
    prefixes <- c("1per")
    clean_flag <- gsub(" ", "_", flag_type)
    for (prefix in prefixes) {
        flag_name <- paste0(datatype, "_", prefix, "_", clean_flag, "_selection_flag")
        if (exists(flag_name, envir = env)) {
            get(flag_name, envir = env)(boolean)
        } else {
            warning(paste("Flag", flag_name, "does not exist."))
        }
    }
}
set_flags_multiper <- function(datatype, flag_type, boolean, env = .GlobalEnv) {
    prefixes <- c("multiper1", "multiper2", "multiper3", "multiper4")
    clean_flag <- gsub(" ", "_", flag_type)
    for (prefix in prefixes) {
        flag_name <- paste0(datatype, "_", prefix, "_", clean_flag, "_selection_flag")
        if (exists(flag_name, envir = env)) {
            get(flag_name, envir = env)(boolean)
        } else {
            warning(paste("Flag", flag_name, "does not exist."))
        }
    }
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
check_datafiles_dates <- function(dfs, type, id, year_flag, period_flag, lobster_season_flag, conch_season_flag, finfish_season_flag, spag_season_flag = NULL) {
    year_flag(FALSE)
    period_flag(FALSE)
    conch_season_flag(FALSE)
    lobster_season_flag(FALSE)
    finfish_season_flag(FALSE)
    if (!is.null(spag_season_flag)) {
        spag_season_flag(FALSE)
    }
    if (is.null(dfs) || length(dfs) == 0) {
        return(show_critical_error("No valid data uploaded"))
    }
    find_date_column <- function(df) {
        if ("Date" %in% names(df)) {
            return("Date")
        } else if ("Waypoint Date" %in% names(df)) {
            return("Waypoint Date")
        } else if ("Date of Encounter" %in% names(df)) {
            return("Date of Encounter")
        }
        return(NULL)
    }
    for (df in dfs) {
        date_column <- find_date_column(df)
        if (!is.null(date_column)) {
            date_char <- as.character(df[[date_column]])
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
                        show_error(paste0("Multiple Years: ", study_years[1], "-", study_years[2])),
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
                        show_error(paste0("Multiple Periods: ", study_periods[1], "-", study_periods[2])),
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
            if (type == "lobster_season") {
                get_lobster_season <- function(d) {
                    m <- as.integer(format(d, "%m"))
                    y <- as.integer(format(d, "%Y"))
                    if (m >= 7) {
                        return(paste0(y, "-", y + 1))
                    } else if (m <= 2) {
                        return(paste0(y - 1, "-", y))
                    } else {
                        return(NA)
                    }
                }
                lobster_seasons <- sapply(valid_dates, get_lobster_season)
                unique_seasons <- sort(unique(lobster_seasons))
                if (length(unique_seasons) > 1) {
                    lobster_season_flag(TRUE)
                    return(div(
                        show_error("Multiple Lobster Seasons"),
                        div(
                            class = "input-list-content",
                            prettyRadioButtons(paste0(id, "lobster_season_selection"),
                                label = "Please select one lobster season.",
                                choices = unique_seasons,
                                inline = TRUE
                            )
                        )
                    ))
                }
                return(div(
                    class = "file-confirmation-button",
                    p(class = "p-black", paste0("Lobster Season: ", unique_seasons[1]))
                ))
            }
            if (type == "conch_season") {
                get_conch_season <- function(d) {
                    m <- as.integer(format(d, "%m"))
                    y <- as.integer(format(d, "%Y"))
                    if (m >= 10) {
                        return(paste0(y, "-", y + 1))
                    } else if (m <= 6) {
                        return(paste0(y - 1, "-", y))
                    } else {
                        return(NA)
                    }
                }
                conch_seasons <- sapply(valid_dates, get_conch_season)
                unique_seasons <- sort(unique(conch_seasons))
                if (length(unique_seasons) > 1) {
                    conch_season_flag(TRUE)
                    return(div(
                        show_error("Multiple Conch Seasons"),
                        div(
                            class = "input-list-content",
                            prettyRadioButtons(paste0(id, "conch_season_selection"),
                                label = "Please select one conch season.",
                                choices = unique_seasons,
                                inline = TRUE
                            )
                        )
                    ))
                }
                return(div(
                    class = "file-confirmation-button",
                    p(class = "p-black", paste0("Conch Season: ", unique_seasons[1]))
                ))
            }
            if (type == "finfish_season") {
                study_years <- format(range(valid_dates), "%Y")
                unique_study_years <- sort(unique(format(valid_dates, "%Y")))
                if (study_years[1] != study_years[2]) {
                    finfish_season_flag(TRUE)
                    return(div(
                        show_error("Multiple Finfish Seasons"),
                        div(
                            class = "input-list-content",
                            prettyRadioButtons(paste0(id, "year_selection"),
                                label = "Please select one season",
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
            if (type == "spag_season") {
                get_spag_season <- function(d) {
                    m <- as.integer(format(d, "%m"))
                    y <- as.integer(format(d, "%Y"))
                    if (m >= 12) {
                        return(paste0(y, "-", y + 1))
                    } else if (m <= 5) {
                        return(paste0(y - 1, "-", y))
                    } else {
                        return(NA)
                    }
                }
                spag_seasons <- sapply(valid_dates, get_spag_season)
                unique_seasons <- sort(unique(spag_seasons))
                if (length(unique_seasons) > 1) {
                    spag_season_flag(TRUE)
                    return(div(
                        show_error("Multiple SPAG Seasons"),
                        div(
                            class = "input-list-content",
                            prettyRadioButtons(paste0(id, "spag_season_selection"),
                                label = "Please select one spag season.",
                                choices = unique_seasons,
                                inline = TRUE
                            )
                        )
                    ))
                }
                return(div(
                    class = "file-confirmation-button",
                    p(class = "p-black", paste0("SPAG Season: ", unique_seasons[1]))
                ))
            }
        }
    }
    return(show_critical_error("No valid dates detected"))
}
check_datafiles_dates_fisheries_1per <- function(type, df) {
    check_datafiles_dates(
        df, type, "fisheries_1per_", fisheries_1per_year_selection_flag, fisheries_1per_period_selection_flag,
        fisheries_1per_lobster_season_selection_flag, fisheries_1per_conch_season_selection_flag, fisheries_1per_finfish_season_selection_flag
    )
}
check_datafiles_dates_fisheries_multiper1 <- function(type, df) {
    check_datafiles_dates(
        df, type, "fisheries_multiper1_", fisheries_multiper1_year_selection_flag, fisheries_multiper1_period_selection_flag,
        fisheries_multiper1_lobster_season_selection_flag, fisheries_multiper1_conch_season_selection_flag, fisheries_multiper1_finfish_season_selection_flag
    )
}
check_datafiles_dates_fisheries_multiper2 <- function(type, df) {
    check_datafiles_dates(
        df, type, "fisheries_multiper2_", fisheries_multiper2_year_selection_flag, fisheries_multiper2_period_selection_flag,
        fisheries_multiper2_lobster_season_selection_flag, fisheries_multiper2_conch_season_selection_flag, fisheries_multiper2_finfish_season_selection_flag
    )
}
check_datafiles_dates_fisheries_multiper3 <- function(type, df) {
    check_datafiles_dates(
        df, type, "fisheries_multiper3_", fisheries_multiper3_year_selection_flag, fisheries_multiper3_period_selection_flag,
        fisheries_multiper3_lobster_season_selection_flag, fisheries_multiper3_conch_season_selection_flag, fisheries_multiper3_finfish_season_selection_flag
    )
}
check_datafiles_dates_fisheries_multiper4 <- function(type, df) {
    check_datafiles_dates(
        df, type, "fisheries_multiper4_", fisheries_multiper4_year_selection_flag, fisheries_multiper4_period_selection_flag,
        fisheries_multiper4_lobster_season_selection_flag, fisheries_multiper4_conch_season_selection_flag, fisheries_multiper4_finfish_season_selection_flag
    )
}
check_datafiles_dates_fisher_1per <- function(type, df) {
    check_datafiles_dates(
        df, type, "fisher_1per_", fisher_1per_year_selection_flag, fisher_1per_period_selection_flag,
        fisher_1per_lobster_season_selection_flag, fisher_1per_conch_season_selection_flag, fisher_1per_finfish_season_selection_flag
    )
}
check_datafiles_dates_fisher_multiper1 <- function(type, df) {
    check_datafiles_dates(
        df, type, "fisher_multiper1_", fisher_multiper1_year_selection_flag, fisher_multiper1_period_selection_flag,
        fisher_multiper1_lobster_season_selection_flag, fisher_multiper1_conch_season_selection_flag, fisher_multiper1_finfish_season_selection_flag
    )
}
check_datafiles_dates_fisher_multiper2 <- function(type, df) {
    check_datafiles_dates(
        df, type, "fisher_multiper2_", fisher_multiper2_year_selection_flag, fisher_multiper2_period_selection_flag,
        fisher_multiper2_lobster_season_selection_flag, fisher_multiper2_conch_season_selection_flag, fisher_multiper2_finfish_season_selection_flag
    )
}
check_datafiles_dates_fisher_multiper3 <- function(type, df) {
    check_datafiles_dates(
        df, type, "fisher_multiper3_", fisher_multiper3_year_selection_flag, fisher_multiper3_period_selection_flag,
        fisher_multiper3_lobster_season_selection_flag, fisher_multiper3_conch_season_selection_flag, fisher_multiper3_finfish_season_selection_flag
    )
}
check_datafiles_dates_fisher_multiper4 <- function(type, df) {
    check_datafiles_dates(
        df, type, "fisher_multiper4_", fisher_multiper4_year_selection_flag, fisher_multiper4_period_selection_flag,
        fisher_multiper4_lobster_season_selection_flag, fisher_multiper4_conch_season_selection_flag, fisher_multiper4_finfish_season_selection_flag
    )
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

# Create helper to read SPAG data
read_spag_data <- function(file_path, datatype) {
    nas <- c("NA", "N/A", "Unknown", "Missing", "None", "N/E")
    sheets_available <- excel_sheets(file_path)
    data_list <- list()
    data_list$Conditions <- read_excel(file_path, sheet = "Surface_Underwater_Conditions", na = nas, guess_max = min(1000, Inf))
    data_list$Visual <- read_excel(file_path, sheet = "Species_Findings", na = nas, guess_max = min(1000, Inf))
    data_list$Laser <- read_excel(file_path, sheet = "Laser", na = nas, guess_max = min(1000, Inf))
    data_list$Species <- read_excel(file_path, sheet = "Species", na = nas, guess_max = min(1000, Inf))
    return(data_list)
}
