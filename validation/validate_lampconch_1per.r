## validate_lampconch_1per.r

# Load packages ---------------------------
library(tidyverse)

# Define functions to check for required columns ---------------------------
completeness_check <- function(df, required_columns) {
    missing_cols <- setdiff(required_columns, colnames(df))
    if (length(missing_cols) > 0) {
        FALSE
    } else {
        TRUE
    }
}
completeness <- function(df, required_columns, df_name) {
    missing_cols <- setdiff(required_columns, colnames(df))
    if (length(missing_cols) > 0) {
        paste("Missing required columns from", df_name, ":", paste(missing_cols, collapse = ", "), "<br><br>")
    }
}

# Define helper functions for Survey Data sheet ---------------------------
validate_date_check <- function(x) {
    invalid_serials <- grepl("^[0-9]+$", x)
    return(!any(invalid_serials))
}
validate_date <- function(x) {
    invalid_serials <- x[grepl("^[0-9]+$", x)]
    if (length(invalid_serials) == 0) {
    } else {
        display_values <- if (length(invalid_serials) > 10) {
            paste(c(invalid_serials[1:10], "..."), collapse = ", ")
        } else {
            paste(invalid_serials, collapse = ", ")
        }
        return(paste0(
            "- Please ensure the Date column in Excel is formatted as YYYY-MM-DD. Some dates are being incorrectly imported as serial numbers due to inconsistent formatting: ",
            display_values,
            "<br><br>"
        ))
    }
}
validate_site_check <- function(x, y) {
    valid <- is.na(x) | x %in% y
    return(all(valid))
}
validate_site <- function(x, y) {
    invalid_sites <- unique(x[!(is.na(x) | x %in% y)])
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
            paste0(" (unexpected values occurred ", length(x[!(is.na(x) | x %in% y)]), " times)."),
            "<br><br>"
        ))
    }
}
validate_transect_check <- function(x) {
    valid <- (!is.na(x) & x %in% 1:4) | x == "MISSING"
    return(all(valid))
}
validate_transect <- function(x) {
    valid <- (!is.na(x) & x %in% 1:4) | x == "MISSING"
    if (all(valid)) {
    } else {
        invalid_values <- unique(x[!valid])
        error_message <- paste0(
            "- These Transect values are unexpected: ",
            paste(invalid_values, collapse = ", "),
            paste0(" (unexpected values occurred ", length(x[!valid]), " times).")
        )
        na_count <- sum(is.na(x))
        if (na_count > 0) {
            error_message <- paste0(error_message, sprintf(" (NA occurs %d times)", na_count), "<br><br>")
        } else {
            error_message <- paste0(error_message, "<br><br>")
        }
        return(error_message)
    }
}
validate_conchcount_check <- function(x) {
    valid <- (!is.na(x) & x %in% c(0, 1)) | x == "MISSING"
    return(all(valid))
}
validate_conchcount <- function(x) {
    valid <- (!is.na(x) & x %in% c(0, 1)) | x == "MISSING"
    if (all(valid)) {
    } else {
        invalid_values <- unique(x[!valid])
        error_message <- paste0(
            "- These Conch Count values are unexpected: ",
            paste(invalid_values, collapse = ", "),
            paste0(" (unexpected values occurred ", length(x[!valid]), " times). Please ensure all conch count values are 0 or 1.")
        )
        na_count <- sum(is.na(x))
        if (na_count > 0) {
            error_message <- paste0(error_message, sprintf(" (NA occurs %d times)", na_count), "<br><br>")
        } else {
            error_message <- paste0(error_message, "<br><br>")
        }
        return(error_message)
    }
}
validate_conchdepth_check <- function(x) {
    x <- as.numeric(x)
    valid <- (x <= 60 & x > 0) | x == "MISSING" | is.na(x)
    return(all(valid))
}
validate_conchdepth <- function(x) {
    x <- as.numeric(x)
    valid <- (x <= 60 & x > 0) | x == "MISSING" | is.na(x)
    if (all(valid)) {
    } else {
        invalid_values <- unique(x[!valid])
        error_message <- paste0(
            "- These Conch Depth (ft) values are unexpected: ",
            paste(invalid_values, collapse = ", "),
            paste0(" (unexpected values occurred ", length(x[!valid]), " times). Please double check these values."),
            "<br><br>"
        )
        return(error_message)
    }
}
validate_shelllength_check <- function(x) {
    x <- as.numeric(x)
    valid <- (x > 0) | x == "MISSING" | is.na(x)
    return(all(valid))
}
validate_shelllength <- function(x) {
    x <- as.numeric(x)
    valid <- (x > 0) | x == "MISSING" | is.na(x)
    if (all(valid)) {
    } else {
        invalid_values <- unique(x[!valid])
        error_message <- paste0(
            "- These Shell Length (in) values are unexpected: ",
            paste(invalid_values, collapse = ", "),
            paste0(" (unexpected values occurred ", length(x[!valid]), " times). Please double check these values."),
            "<br><br>"
        )
        return(error_message)
    }
}
validate_lipthickness_check <- function(x) {
    x <- as.numeric(x)
    valid <- (x >= 0) | x == "MISSING" | is.na(x)
    return(all(valid))
}
validate_lipthickness <- function(x) {
    x <- as.numeric(x)
    valid <- (x >= 0) | x == "MISSING" | is.na(x)
    if (all(valid)) {
    } else {
        invalid_values <- unique(x[!valid])
        error_message <- paste0(
            "- These Lip Thickness (mm) values are unexpected: ",
            paste(invalid_values, collapse = ", "),
            paste0(" (unexpected values occurred ", length(x[!valid]), " times). Please double check these values."),
            "<br><br>"
        )
        return(error_message)
    }
}
# Define helper functions for Sites sheet ---------------------------
validate_date_y_check <- function(y) {
    invalid_serials <- grepl("^[0-9]+$", y)
    return(!any(invalid_serials))
}
validate_date_y <- function(y) {
    invalid_serials <- y[grepl("^[0-9]+$", y)]
    if (length(invalid_serials) == 0) {
    } else {
        display_values <- if (length(invalid_serials) > 10) {
            paste(c(invalid_serials[1:10], "..."), collapse = ", ")
        } else {
            paste(invalid_serials, collapse = ", ")
        }
        return(paste0(
            "- Please ensure the Date column in Excel is formatted as YYYY-MM-DD. Some dates are being incorrectly imported as serial numbers due to inconsistent formatting: ",
            display_values,
            "<br><br>"
        ))
    }
}
validate_site_y_check <- function(y) {
    valid <- (is.na(y) | grepl("^(NO|NE|SE|SW)", y) | y == "MISSING")
    return(all(valid))
}
validate_site_y <- function(y) {
    y <- as.character(y)
    valid <- (is.na(y) | grepl("^(NO|NE|SE|SW)", y) | y == "MISSING")
    if (all(valid)) {
    } else {
        invalid_values <- unique(y[!valid & !is.na(y)])
        error_message <- ""
        if (length(invalid_values) > 0) {
            error_message <- paste0(
                "- These Site IDs are unexpected: ",
                paste(invalid_values, collapse = ", "),
                sprintf(" (unexpected values occurred %d times).", sum(!valid & !is.na(y))),
                "<br><br>"
            )
        }
        return(error_message)
    }
}
validate_section_check <- function(y) {
    valid <- (is.na(y) | y %in% c("NO", "NE", "SE", "SW") | y == "MISSING")
    return(all(valid))
}
validate_section <- function(y) {
    y <- as.character(y)
    valid <- (is.na(y) | y %in% c("NO", "NE", "SE", "SW") | y == "MISSING")
    if (all(valid)) {
    } else {
        invalid_values <- unique(y[!valid & !is.na(y)])
        error_message <- ""
        if (length(invalid_values) > 0) {
            error_message <- paste0(
                "- These Section values are unexpected: ",
                paste(invalid_values, collapse = ", "),
                sprintf(" (unexpected values occurred %d times).", sum(!valid & !is.na(y))),
                "<br><br>"
            )
        }
        return(error_message)
    }
}
validate_sitesectionmatch_check <- function(site_ids, sections) {
    site_ids <- as.character(site_ids)
    sections <- as.character(sections)
    extracted_prefix <- substr(site_ids, 1, 2)
    valid <- (is.na(site_ids) | is.na(sections) | extracted_prefix == sections) | site_ids == "MISSING"
    return(all(valid))
}
validate_sitesectionmatch <- function(site_ids, sections) {
    site_ids <- as.character(site_ids)
    sections <- as.character(sections)
    extracted_prefix <- substr(site_ids, 1, 2)
    valid <- (is.na(site_ids) | is.na(sections) | extracted_prefix == sections) | site_ids == "MISSING"
    if (all(valid)) {
    } else {
        mismatched_sites <- unique(site_ids[!valid & !is.na(site_ids) & !is.na(sections)])
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
validate_coords_check <- function(latitude, longitude) {
    valid <- (latitude >= 17.117024 & latitude <= 17.664305) & (longitude >= -87.989322 & longitude <= -87.720954) | is.na(latitude) | is.na(longitude)
    return(all(valid))
}
validate_coords <- function(latitude, longitude) {
    valid <- (latitude >= 17.117024 & latitude <= 17.664305) & (longitude >= -87.989322 & longitude <= -87.720954) | is.na(latitude) | is.na(longitude)
    if (all(valid, na.rm = TRUE)) {
    } else {
        invalid_coords <- data.frame(
            Latitude = latitude[!valid & !is.na(latitude) & !is.na(longitude)],
            Longitude = longitude[!valid & !is.na(latitude) & !is.na(longitude)]
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
validate_habitat_check <- function(y_habitats, z_habitats) {
    y_habitats <- unlist(strsplit(paste(na.omit(y_habitats), collapse = ", "), split = ", "))
    z_habitats <- unique(trimws(z_habitats))
    valid <- y_habitats %in% z_habitats
    return(all(valid, na.rm = TRUE))
}
validate_habitat <- function(y_habitats, z_habitats) {
    y_habitats <- unlist(strsplit(paste(na.omit(y_habitats), collapse = ", "), split = ", "))
    y_habitats <- trimws(y_habitats)
    z_habitats <- unique(trimws(z_habitats))
    valid <- y_habitats %in% z_habitats
    invalid <- unique(y_habitats[!valid])
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
validate_method_check <- function(y) {
    valid_methods <- c("Dive", "Snorkel", "Wade")
    valid <- y %in% valid_methods | is.na(y)
    all(valid, na.rm = TRUE)
}
validate_method <- function(y) {
    valid_methods <- c("Dive", "Snorkel", "Wade")
    valid <- y %in% valid_methods | is.na(y)
    if (all(valid, na.rm = TRUE)) {
    } else {
        invalid <- unique(y[!valid & !is.na(y)])
        return(paste0(
            "- These Method values are unexpected (Method should be Dive, Snorkel, or Wade with proper capitalization): ",
            paste(invalid, collapse = ", "),
            sprintf(" (unexpected values occurred %d times).", sum(!valid & !is.na(y))),
            "<br><br>"
        ))
    }
}
validate_recorders_check <- function(y) {
    valid <- grepl("^[A-Za-z ]+(, [A-Za-z ]+)*$", y) | is.na(y)
    all(valid, na.rm = TRUE)
}
validate_recorders <- function(y) {
    valid <- grepl("^[A-Za-z ]+(, [A-Za-z ]+)*$", y) | is.na(y)
    if (all(valid, na.rm = TRUE)) {
    } else {
        invalid <- unique(y[!valid & !is.na(y)])
        return(paste0(
            "- These `Recorder(s)` values do not match the expected format (comma-separated full names): ",
            paste(invalid, collapse = ", "),
            sprintf(" (unexpected values occurred %d times).", sum(!valid & !is.na(y))),
            "<br><br>"
        ))
    }
}

# Define primary functions ---------------------------
func_validate_lampconch_1per_completeness_check <- function(x, y, z) {
    required_columns_surveydata <- c("Date", "Site ID", "Transect", "Conch Count", "Conch Depth (ft)", "Shell Length (in)", "Lip Thickness (mm)")
    required_columns_sites <- c("Site ID", "Date", "Section", "Latitude", "Longitude", "Habitat", "Method", "Recorder(s)")
    required_columns_habitattypes <- c("Habitat Type")
    complete_x <- completeness_check(x, required_columns_surveydata)
    complete_y <- completeness_check(y, required_columns_sites)
    complete_z <- completeness_check(z, required_columns_habitattypes)
    all(c(complete_x, complete_y, complete_z))
}
func_validate_lampconch_1per_check <- function(x, y, z) {
    date_valid <- validate_date_check(x$Date)
    site_valid <- validate_site_check(x$`Site ID`, y$`Site ID`)
    transect_valid <- validate_transect_check(x$Transect)
    conchcount_valid <- validate_conchcount_check(x$`Conch Count`)
    conchdepth_valid <- validate_conchdepth_check(x$`Conch Depth (ft)`)
    shelllength_valid <- validate_shelllength_check(x$`Shell Length (in)`)
    lipthickness_valid <- validate_lipthickness_check(x$`Lip Thickness (mm)`)
    date_y_valid <- validate_date_y_check(y$Date)
    site_y_valid <- validate_site_y_check(y$`Site ID`)
    section_valid <- validate_section_check(y$Section)
    sitesectionmatch_valid <- validate_sitesectionmatch_check(y$`Site ID`, y$Section)
    coords_valid <- validate_coords_check(y$Latitude, y$Longitude)
    habitat_valid <- validate_habitat_check(y$Habitat, z$`Habitat Type`)
    method_valid <- validate_method_check(y$Method)
    recorders_valid <- validate_recorders_check(y$`Recorder(s)`)
    return(all(c(
        date_valid, site_valid, transect_valid, conchcount_valid, conchdepth_valid, shelllength_valid, lipthickness_valid, # Survey Data sheet
        date_y_valid, site_y_valid, section_valid, sitesectionmatch_valid, coords_valid, habitat_valid, method_valid, recorders_valid # Sites sheet
    )))
}
func_validate_lampconch_1per_completeness <- function(x, y, z) {
    required_columns_surveydata <- c("Date", "Site ID", "Transect", "Conch Count", "Conch Depth (ft)", "Shell Length (in)", "Lip Thickness (mm)")
    required_columns_sites <- c("Site ID", "Date", "Section", "Latitude", "Longitude", "Habitat", "Method", "Recorder(s)")
    required_columns_habitattypes <- c("Habitat Type")
    complete_x <- completeness(x, required_columns_surveydata, "Survey Data")
    complete_y <- completeness(y, required_columns_sites, "Sites")
    complete_z <- completeness(z, required_columns_habitattypes, "Habitat Types")
    paste0(complete_x, complete_y, complete_z)
}
func_validate_lampconch_1per_surveydata <- function(x, y) {
    date_valid <- validate_date(x$Date)
    site_valid <- validate_site(x$`Site ID`, y$`Site ID`)
    transect_valid <- validate_transect(x$Transect)
    conchcount_valid <- validate_conchcount(x$`Conch Count`)
    conchdepth_valid <- validate_conchdepth(x$`Conch Depth (ft)`)
    shelllength_valid <- validate_shelllength(x$`Shell Length (in)`)
    lipthickness_valid <- validate_lipthickness(x$`Lip Thickness (mm)`)
    return(paste(date_valid, site_valid, transect_valid, conchcount_valid, conchdepth_valid, shelllength_valid, lipthickness_valid))
}
func_validate_lampconch_1per_sites <- function(x, y, z) {
    date_y_valid <- validate_date_y(y$Date)
    site_y_valid <- validate_site_y(y$`Site ID`)
    section_valid <- validate_section(y$Section)
    sitesectionmatch_valid <- validate_sitesectionmatch(y$`Site ID`, y$Section)
    coords_valid <- validate_coords(y$Latitude, y$Longitude)
    habitat_valid <- validate_habitat(y$Habitat, z$`Habitat Type`)
    method_valid <- validate_method(y$Method)
    recorders_valid <- validate_recorders(y$`Recorder(s)`)
    return(paste(date_y_valid, site_y_valid, section_valid, sitesectionmatch_valid, coords_valid, habitat_valid, method_valid, recorders_valid))
}
