## validate_lampconch_1per.r

# Load packages ---------------------------
library(tidyverse)

# Define validation helper functions ---------------------------

#### testing only
# library(readxl)
# datafile <- list(
#    Survey_Data = read_excel("reports/Conch Data 2013 - Standardized.xlsx", sheet = "Survey Data"),
#    Sites = read_excel("reports/Conch Data 2013 - Standardized.xlsx", sheet = "Sites")
# )
validate_transect_check <- function(x) {
    valid <- (!is.na(x) & x %in% 1:3) | x == "MISSING"
    return(all(valid))
}
validate_transect <- function(x) {
    valid <- (!is.na(x) & x %in% 1:3) | x == "MISSING"
    if (all(valid)) {
        return("Transect is validated!")
    } else {
        invalid_values <- unique(x[!valid])
        error_message <- paste0(
            "Attention: These transect values are unexpected: ",
            paste(invalid_values, collapse = ", "),
            paste0(" (unexpected values occurred ", length(x[!valid]), " times).")
        )
        na_count <- sum(is.na(x))
        if (na_count > 0) {
            error_message <- paste0(error_message, sprintf(" (NA occurs %d times)", na_count))
        }
        return(error_message)
    }
}

# validate_transect_check(datafile$Survey_Data$Transect)
