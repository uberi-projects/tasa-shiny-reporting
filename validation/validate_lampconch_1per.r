## validate_lampconch_1per.r

# Load packages ---------------------------
library(tidyverse)

# Define validation helper functions ---------------------------
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
            "Please ensure the Date column in Excel is formatted as YYYY-MM-DD. Some dates are being incorrectly imported as serial numbers due to inconsistent formatting: ",
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
            "Some Site IDs in the Survey Data sheet do not match the surveyed Sites sheet: ",
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
            "These transect values are unexpected: ",
            paste(invalid_values, collapse = ", "),
            paste0(" (unexpected values occurred ", length(x[!valid]), " times)."),
            "<br><br>"
        )
        na_count <- sum(is.na(x))
        if (na_count > 0) {
            error_message <- paste0(error_message, sprintf(" (NA occurs %d times)", na_count))
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
            "These conch count values are unexpected: ",
            paste(invalid_values, collapse = ", "),
            paste0(" (unexpected values occurred ", length(x[!valid]), " times). Please ensure all conch count values are 0 or 1."),
            "<br><br>"
        )
        na_count <- sum(is.na(x))
        if (na_count > 0) {
            error_message <- paste0(error_message, sprintf(" (NA occurs %d times)", na_count))
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
            "These conch depth values are unexpected: ",
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
            "These shell length values are unexpected: ",
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
            "These lip thickness values are unexpected: ",
            paste(invalid_values, collapse = ", "),
            paste0(" (unexpected values occurred ", length(x[!valid]), " times). Please double check these values."),
            "<br><br>"
        )
        return(error_message)
    }
}

# Define primary functions ---------------------------
func_validate_lampconch_1per_check <- function(x, y) {
    date_valid <- validate_date_check(x$Date)
    site_valid <- validate_site_check(x$`Site ID`, y$`Site ID`)
    transect_valid <- validate_transect_check(x$Transect)
    conchcount_valid <- validate_conchcount_check(x$`Conch Count`)
    conchdepth_valid <- validate_conchdepth_check(x$`Conch Depth (ft)`)
    shelllength_valid <- validate_shelllength_check(x$`Shell Length (in)`)
    lipthickness_valid <- validate_lipthickness_check(x$`Lip Thickness (mm)`)
    return(all(c(date_valid, site_valid, transect_valid, conchcount_valid, conchdepth_valid, shelllength_valid, lipthickness_valid)))
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
