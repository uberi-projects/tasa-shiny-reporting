## validate_lampconch_1per.r

# Source general helper functions ---------------------------
source("validation/general_validation_helpers.r")

# Define helper functions for Survey Data sheet ---------------------------
## Site ID (Survey Data) & Site ID (Sites)
validate_site_check <- function(site_col_1, site_col_2) {
    valid <- is.na(site_col_1) | site_col_1 %in% site_col_2
    return(all(valid))
}
validate_site <- function(site_col_1, site_col_2) {
    invalid_sites <- unique(site_col_1[!(is.na(site_col_1) | site_col_1 %in% site_col_2)])
    if (length(invalid_sites) == 0) {
    } else {
        display_values <- if (length(invalid_sites) > 10) {
            paste(c(invalid_sites[1:10], "..."), collapse = ", ")
        } else {
            paste(invalid_sites, collapse = ", ")
        }
        return(paste0(
            "- Some Site IDs in the Survey Data sheet do not match the surveyed Sites sheet: ",
            display_values,
            paste0(" (unexpected values occurred ", length(site_col_1[!(is.na(site_col_1) | site_col_1 %in% site_col_2)]), " times)."),
            "<br><br>"
        ))
    }
}
## Transect
validate_transect_check <- function(transect_col) {
    valid <- (!is.na(transect_col) & transect_col %in% 1:4) | transect_col == "MISSING"
    return(all(valid))
}
validate_transect <- function(transect_col) {
    valid <- (!is.na(transect_col) & transect_col %in% 1:4) | transect_col == "MISSING"
    if (all(valid)) {
    } else {
        invalid_values <- unique(transect_col[!valid])
        error_message <- paste0(
            "- These Transect values are unexpected: ",
            paste(invalid_values, collapse = ", "),
            paste0(" (unexpected values occurred ", length(transect_col[!valid]), " times).")
        )
        na_count <- sum(is.na(transect_col))
        if (na_count > 0) {
            error_message <- paste0(error_message, sprintf(" (NA occurs %d times)", na_count), "<br><br>")
        } else {
            error_message <- paste0(error_message, "<br><br>")
        }
        return(error_message)
    }
}
## Conch Count
validate_conchcount_check <- function(conchcount_col) {
    valid <- (!is.na(conchcount_col) & conchcount_col %in% c(0, 1)) | conchcount_col == "MISSING"
    return(all(valid))
}
validate_conchcount <- function(conchcount_col) {
    valid <- (!is.na(conchcount_col) & conchcount_col %in% c(0, 1)) | conchcount_col == "MISSING"
    if (all(valid)) {
    } else {
        invalid_values <- unique(conchcount_col[!valid])
        error_message <- paste0(
            "- These Conch Count values are unexpected: ",
            paste(invalid_values, collapse = ", "),
            paste0(" (unexpected values occurred ", length(conchcount_col[!valid]), " times). Please ensure all conch count values are 0 or 1.")
        )
        na_count <- sum(is.na(conchcount_col))
        if (na_count > 0) {
            error_message <- paste0(error_message, sprintf(" (NA occurs %d times)", na_count), "<br><br>")
        } else {
            error_message <- paste0(error_message, "<br><br>")
        }
        return(error_message)
    }
}
## Conch Depth (ft)
validate_conchdepth_check <- function(conchdepth_col) {
    conchdepth_col <- as.numeric(conchdepth_col)
    valid <- (conchdepth_col <= 60 & conchdepth_col > 0) | conchdepth_col == "MISSING" | is.na(conchdepth_col)
    return(all(valid))
}
validate_conchdepth <- function(conchdepth_col) {
    conchdepth_col <- as.numeric(conchdepth_col)
    valid <- (conchdepth_col <= 60 & conchdepth_col > 0) | conchdepth_col == "MISSING" | is.na(conchdepth_col)
    if (all(valid)) {
    } else {
        invalid_values <- unique(conchdepth_col[!valid])
        error_message <- paste0(
            "- These Conch Depth (ft) values are unexpected: ",
            paste(invalid_values, collapse = ", "),
            paste0(" (unexpected values occurred ", length(conchdepth_col[!valid]), " times). Please double check these values."),
            "<br><br>"
        )
        return(error_message)
    }
}
## Shell Length (in)
validate_shelllength_check <- function(shell_col) {
    shell_col <- as.numeric(shell_col)
    valid <- (shell_col > 0) | shell_col == "MISSING" | is.na(shell_col)
    return(all(valid))
}
validate_shelllength <- function(shell_col) {
    shell_col <- as.numeric(shell_col)
    valid <- (shell_col > 0) | shell_col == "MISSING" | is.na(shell_col)
    if (all(valid)) {
    } else {
        invalid_values <- unique(shell_col[!valid])
        error_message <- paste0(
            "- These Shell Length (in) values are unexpected: ",
            paste(invalid_values, collapse = ", "),
            paste0(" (unexpected values occurred ", length(shell_col[!valid]), " times). Please double check these values."),
            "<br><br>"
        )
        return(error_message)
    }
}
## Lip Thickness (mm)
validate_lipthickness_check <- function(lip_col) {
    lip_col <- as.numeric(lip_col)
    valid <- (lip_col >= 0) | lip_col == "MISSING" | is.na(lip_col)
    return(all(valid))
}
validate_lipthickness <- function(lip_col) {
    lip_col <- as.numeric(lip_col)
    valid <- (lip_col >= 0) | lip_col == "MISSING" | is.na(lip_col)
    if (all(valid)) {
    } else {
        invalid_values <- unique(lip_col[!valid])
        error_message <- paste0(
            "- These Lip Thickness (mm) values are unexpected: ",
            paste(invalid_values, collapse = ", "),
            paste0(" (unexpected values occurred ", length(lip_col[!valid]), " times). Please double check these values."),
            "<br><br>"
        )
        return(error_message)
    }
}

# Define helper functions for Sites sheet ---------------------------
## Site ID
validate_site_2_check <- function(site_col_2) {
    valid <- (is.na(site_col_2) | grepl("^(NO|NE|SE|SW)", site_col_2) | site_col_2 == "MISSING")
    return(all(valid))
}
validate_site_2 <- function(site_col_2) {
    site_col_2 <- as.character(site_col_2)
    valid <- (is.na(site_col_2) | grepl("^(NO|NE|SE|SW)", site_col_2) | site_col_2 == "MISSING")
    if (all(valid)) {
    } else {
        invalid_values <- unique(site_col_2[!valid & !is.na(site_col_2)])
        error_message <- ""
        if (length(invalid_values) > 0) {
            error_message <- paste0(
                "- These Site IDs are unexpected: ",
                paste(invalid_values, collapse = ", "),
                sprintf(" (unexpected values occurred %d times).", sum(!valid & !is.na(site_col_2))),
                "<br><br>"
            )
        }
        return(error_message)
    }
}
## Section
validate_section_check <- function(section_col) {
    valid <- (is.na(section_col) | section_col %in% c("NO", "NE", "SE", "SW") | section_col == "MISSING")
    return(all(valid))
}
validate_section <- function(section_col) {
    section_col <- as.character(section_col)
    valid <- (is.na(section_col) | section_col %in% c("NO", "NE", "SE", "SW") | section_col == "MISSING")
    if (all(valid)) {
    } else {
        invalid_values <- unique(section_col[!valid & !is.na(section_col)])
        error_message <- ""
        if (length(invalid_values) > 0) {
            error_message <- paste0(
                "- These Section values are unexpected: ",
                paste(invalid_values, collapse = ", "),
                sprintf(" (unexpected values occurred %d times).", sum(!valid & !is.na(section_col))),
                "<br><br>"
            )
        }
        return(error_message)
    }
}
## Site ID & Section match
validate_sitesectionmatch_check <- function(siteid_col, sections_col) {
    siteid_col <- as.character(siteid_col)
    sections_col <- as.character(sections_col)
    extracted_prefix <- substr(siteid_col, 1, 2)
    valid <- (is.na(siteid_col) | is.na(sections_col) | extracted_prefix == sections_col) | siteid_col == "MISSING"
    return(all(valid))
}
validate_sitesectionmatch <- function(siteid_col, sections_col) {
    siteid_col <- as.character(siteid_col)
    sections_col <- as.character(sections_col)
    extracted_prefix <- substr(siteid_col, 1, 2)
    valid <- (is.na(siteid_col) | is.na(sections_col) | extracted_prefix == sections_col) | siteid_col == "MISSING"
    if (all(valid)) {
    } else {
        mismatched_sites <- unique(siteid_col[!valid & !is.na(siteid_col) & !is.na(sections_col)])
        error_message <- ""
        if (length(mismatched_sites) > 0) {
            error_message <- paste0(
                "- These Site IDs do not match their corresponding Section in the data: ",
                paste(mismatched_sites, collapse = ", "),
                sprintf(" (mismatch occurred %d times).", sum(!valid)),
                "<br><br>"
            )
        }
        return(error_message)
    }
}
## Latitude & Longitude
validate_coords_check <- function(latitude_col, longitude_col) {
    valid <- (latitude_col >= 17.117024 & latitude_col <= 17.664305) & (longitude_col >= -87.989322 & longitude_col <= -87.720954) | is.na(latitude_col) | is.na(longitude_col)
    return(all(valid))
}
validate_coords <- function(latitude_col, longitude_col) {
    valid <- (latitude_col >= 17.117024 & latitude_col <= 17.664305) & (longitude_col >= -87.989322 & longitude_col <= -87.720954) | is.na(latitude_col) | is.na(longitude_col)
    if (all(valid, na.rm = TRUE)) {
    } else {
        invalid_coords <- data.frame(
            Latitude = latitude_col[!valid & !is.na(latitude_col) & !is.na(longitude_col)],
            Longitude = longitude_col[!valid & !is.na(latitude_col) & !is.na(longitude_col)]
        )
        error_message <- paste0(
            "- These Latitude/Longitude values are outside the boundaries of TAMR: ",
            paste(
                apply(invalid_coords, 1, function(row) paste0("(", row[1], ", ", row[2], ")")),
                collapse = ", "
            ),
            sprintf(" (unexpected coordinates occurred %d times).", nrow(invalid_coords)),
            "<br><br>"
        )
        return(error_message)
    }
}
## Habitat (Sites) & Habitat Type (Habitat Types)
validate_habitat_check <- function(habitat_col1, habitat_col2) {
    habitat_col1 <- unlist(strsplit(paste(na.omit(habitat_col1), collapse = ", "), split = ", "))
    habitat_col2 <- unique(trimws(habitat_col2))
    valid <- habitat_col1 %in% habitat_col2
    return(all(valid, na.rm = TRUE))
}
validate_habitat <- function(habitat_col1, habitat_col2) {
    habitat_col1 <- unlist(strsplit(paste(na.omit(habitat_col1), collapse = ", "), split = ", "))
    habitat_col1 <- trimws(habitat_col1)
    habitat_col2 <- unique(trimws(habitat_col2))
    valid <- habitat_col1 %in% habitat_col2
    invalid <- unique(habitat_col1[!valid])
    if (length(invalid) == 0) {
    } else {
        error_message <- paste0(
            "- These Habitat values are unexpected, as they do not match a valid Habitat Type from the Habitat Types sheet: ",
            paste(invalid, collapse = ", "),
            sprintf(" (unexpected values occurred %d times).", sum(!valid)),
            "<br><br>"
        )
        return(error_message)
    }
}
## Method
validate_method_check <- function(method_col) {
    valid_methods <- c("Dive", "Snorkel", "Wade")
    valid <- method_col %in% valid_methods | is.na(method_col)
    all(valid, na.rm = TRUE)
}
validate_method <- function(method_col) {
    valid_methods <- c("Dive", "Snorkel", "Wade")
    valid <- method_col %in% valid_methods | is.na(method_col)
    if (all(valid, na.rm = TRUE)) {
    } else {
        invalid <- unique(method_col[!valid & !is.na(method_col)])
        return(paste0(
            "- These Method values are unexpected (Method should be Dive, Snorkel, or Wade with proper capitalization): ",
            paste(invalid, collapse = ", "),
            sprintf(" (unexpected values occurred %d times).", sum(!valid & !is.na(method_col))),
            "<br><br>"
        ))
    }
}
## Recorder(s)
validate_recorders_check <- function(recorder_col) {
    valid <- grepl("^[A-Za-z ]+(, [A-Za-z ]+)*$", recorder_col) | is.na(recorder_col)
    all(valid, na.rm = TRUE)
}
validate_recorders <- function(recorder_col) {
    valid <- grepl("^[A-Za-z ]+(, [A-Za-z ]+)*$", recorder_col) | is.na(recorder_col)
    if (all(valid, na.rm = TRUE)) {
    } else {
        invalid <- unique(recorder_col[!valid & !is.na(recorder_col)])
        return(paste0(
            "- These `Recorder(s)` values do not match the expected format (comma-separated full names): ",
            paste(invalid, collapse = ", "),
            sprintf(" (unexpected values occurred %d times).", sum(!valid & !is.na(recorder_col))),
            "<br><br>"
        ))
    }
}

# Define primary functions ---------------------------
## Check df list for required sheets
func_validate_lampconch_1per_sheets_check <- function(df_list) {
    sheets_check(df_list, c("Survey_Data", "Habitat_Types", "Sites"))
}
func_validate_lampconch_1per_sheets <- function(df_list) {
    sheets(df_list, c("Survey_Data", "Habitat_Types", "Sites"), "Conch LAMP")
}
## Check sheets for required columns (completeness)
func_validate_lampconch_1per_completeness_check <- function(survey_sheet, sites_sheet, habitats_sheet) {
    required_columns_surveydata <- c("Date", "Year", "Site ID", "Transect", "Conch Count", "Conch Depth (ft)", "Shell Length (in)", "Lip Thickness (mm)")
    required_columns_sites <- c("Site ID", "Year", "Date", "Section", "Latitude", "Longitude", "Habitat", "Method", "Recorder(s)")
    required_columns_habitattypes <- c("Habitat Type")
    complete_x <- completeness_check(survey_sheet, required_columns_surveydata)
    complete_y <- completeness_check(sites_sheet, required_columns_sites)
    complete_z <- completeness_check(habitats_sheet, required_columns_habitattypes)
    all(c(complete_x, complete_y, complete_z))
}
func_validate_lampconch_1per_completeness <- function(survey_sheet, sites_sheet, habitats_sheet) {
    required_columns_surveydata <- c("Date", "Year", "Site ID", "Transect", "Conch Count", "Conch Depth (ft)", "Shell Length (in)", "Lip Thickness (mm)")
    required_columns_sites <- c("Site ID", "Year", "Date", "Section", "Latitude", "Longitude", "Habitat", "Method", "Recorder(s)")
    required_columns_habitattypes <- c("Habitat Type")
    complete_x <- completeness(survey_sheet, required_columns_surveydata, "Survey Data")
    complete_y <- completeness(sites_sheet, required_columns_sites, "Sites")
    complete_z <- completeness(habitats_sheet, required_columns_habitattypes, "Habitat Types")
    paste0(complete_x, complete_y, complete_z)
}
## See if the sheets pass ALL validation checks
func_validate_lampconch_1per_check <- function(survey_sheet, sites_sheet, habitats_sheet) {
    date_valid <- validate_date_check(survey_sheet$Date)
    site_valid <- validate_site_check(survey_sheet$`Site ID`, sites_sheet$`Site ID`)
    transect_valid <- validate_transect_check(survey_sheet$Transect)
    conchcount_valid <- validate_conchcount_check(survey_sheet$`Conch Count`)
    conchdepth_valid <- validate_conchdepth_check(survey_sheet$`Conch Depth (ft)`)
    shelllength_valid <- validate_shelllength_check(survey_sheet$`Shell Length (in)`)
    lipthickness_valid <- validate_lipthickness_check(survey_sheet$`Lip Thickness (mm)`)
    date_y_valid <- validate_date_check(sites_sheet$Date)
    site_y_valid <- validate_site_2_check(sites_sheet$`Site ID`)
    section_valid <- validate_section_check(sites_sheet$Section)
    sitesectionmatch_valid <- validate_sitesectionmatch_check(sites_sheet$`Site ID`, sites_sheet$Section)
    coords_valid <- validate_coords_check(sites_sheet$Latitude, sites_sheet$Longitude)
    habitat_valid <- validate_habitat_check(sites_sheet$Habitat, habitats_sheet$`Habitat Type`)
    method_valid <- validate_method_check(sites_sheet$Method)
    recorders_valid <- validate_recorders_check(sites_sheet$`Recorder(s)`)
    return(all(c(
        date_valid, site_valid, transect_valid, conchcount_valid, conchdepth_valid, shelllength_valid, lipthickness_valid, # Survey Data sheet
        date_y_valid, site_y_valid, section_valid, sitesectionmatch_valid, coords_valid, habitat_valid, method_valid, recorders_valid # Sites sheet
    )))
}
## Perform validation for survey data sheet
func_validate_lampconch_1per_surveydata <- function(survey_sheet, sites_sheet) {
    date_valid <- validate_date(survey_sheet$Date)
    site_valid <- validate_site(survey_sheet$`Site ID`, sites_sheet$`Site ID`)
    transect_valid <- validate_transect(survey_sheet$Transect)
    conchcount_valid <- validate_conchcount(survey_sheet$`Conch Count`)
    conchdepth_valid <- validate_conchdepth(survey_sheet$`Conch Depth (ft)`)
    shelllength_valid <- validate_shelllength(survey_sheet$`Shell Length (in)`)
    lipthickness_valid <- validate_lipthickness(survey_sheet$`Lip Thickness (mm)`)
    return(paste(date_valid, site_valid, transect_valid, conchcount_valid, conchdepth_valid, shelllength_valid, lipthickness_valid))
}
## Perform validation for sites sheet
func_validate_lampconch_1per_sites <- function(survey_sheet, sites_sheet, habitats_sheet) {
    date_y_valid <- validate_date(sites_sheet$Date)
    site_y_valid <- validate_site_2(sites_sheet$`Site ID`)
    section_valid <- validate_section(sites_sheet$Section)
    sitesectionmatch_valid <- validate_sitesectionmatch(sites_sheet$`Site ID`, sites_sheet$Section)
    coords_valid <- validate_coords(sites_sheet$Latitude, sites_sheet$Longitude)
    habitat_valid <- validate_habitat(sites_sheet$Habitat, habitats_sheet$`Habitat Type`)
    method_valid <- validate_method(sites_sheet$Method)
    recorders_valid <- validate_recorders(sites_sheet$`Recorder(s)`)
    return(paste(date_y_valid, site_y_valid, section_valid, sitesectionmatch_valid, coords_valid, habitat_valid, method_valid, recorders_valid))
}
