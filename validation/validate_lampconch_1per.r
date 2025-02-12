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

# Define primary functions ---------------------------
func_validate_lampconch_1per_check <- function(x) {
    transect_valid <- validate_transect_check(x$Transect)
    date_valid <- validate_date_check(x$Date)
    return(all(c(transect_valid, date_valid)))
}
func_validate_lampconch_1per <- function(x) {
    transect_valid <- validate_transect(x$Transect)
    date_valid <- validate_date(x$Date)
    return(paste(transect_valid, date_valid))
}
