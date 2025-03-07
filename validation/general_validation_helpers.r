## general_validation_helpers.r

# Attach packages ---------------------------
library(tidyverse)

# Define functions to check for required sheets ---------------------------
sheets_check <- function(df_list, required_sheets) {
    missing_sheets <- setdiff(required_sheets, names(df_list))
    if (length(missing_sheets) > 0) {
        FALSE
    } else {
        TRUE
    }
}
sheets <- function(df_list, required_sheets, df_list_name) {
    missing_sheets <- setdiff(required_sheets, names(df_list))
    if (length(missing_sheets) > 0) {
        paste("Missing required sheets from", df_list_name, "datafile:", paste(missing_sheets, collapse = ", "), "<br><br>")
    }
}

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
        paste0("Missing required columns from ", df_name, ": ", paste(missing_cols, collapse = ", "), "<br><br>")
    }
}

# Define common columns validation functions ---------------------------
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
