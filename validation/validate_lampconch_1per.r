## validate_lampconch_1per.r

# Load packages ---------------------------
library(tidyverse)

# Define validation helper functions ---------------------------
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
            "

            "
        )
        na_count <- sum(is.na(x))
        if (na_count > 0) {
            error_message <- paste0(error_message, sprintf(" (NA occurs %d times)", na_count))
        }
        return(error_message)
    }
}
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
            "

            "
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
        return("Site ID column is validated!")
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
            "

            "
        ))
    }
}

# Define primary functions ---------------------------
func_validate_lampconch_1per_check <- function(x, y) {
    date_valid <- validate_date_check(x$Date)
    site_valid <- validate_site_check(x$`Site ID`, y$`Site ID`)
    transect_valid <- validate_transect_check(x$Transect)
    return(all(c(date_valid, site_valid, transect_valid)))
}
func_validate_lampconch_1per <- function(x, y) {
    date_valid <- validate_date(x$Date)
    site_valid <- validate_site(x$`Site ID`, y$`Site ID`)
    transect_valid <- validate_transect(x$Transect)
    return(paste(date_valid, site_valid, transect_valid))
}
