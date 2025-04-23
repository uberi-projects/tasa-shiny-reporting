## validate_fisher_1per.r

# Source general helper functions ---------------------------
source("validation/general_validation_helpers.r")

# Define helper functions for multiple sheets ---------------------------
## X & Y
validate_coords_check <- function(longitude_col, latitude_col) {
    valid <- ((latitude_col >= 17.117024 & latitude_col <= 17.664305) &
        (longitude_col >= -87.989322 & longitude_col <= -87.720954)) |
        is.na(latitude_col) | is.na(longitude_col)
    return(all(valid))
}
validate_coords <- function(longitude_col, latitude_col) {
    valid <- ((latitude_col >= 17.117024 & latitude_col <= 17.664305) &
        (longitude_col >= -87.989322 & longitude_col <= -87.720954)) |
        is.na(latitude_col) | is.na(longitude_col)

    if (all(valid, na.rm = TRUE)) {
        return(NULL)
    } else {
        invalid_coords <- data.frame(
            X = longitude_col[!valid & !is.na(latitude_col) & !is.na(longitude_col)],
            Y = latitude_col[!valid & !is.na(latitude_col) & !is.na(longitude_col)]
        )
        coord_strings <- apply(invalid_coords, 1, function(row) paste0("(", row[1], ", ", row[2], ")"))
        unique_coord_strings <- unique(coord_strings)
        display_coords <- if (length(unique_coord_strings) > 10) {
            paste(c(unique_coord_strings[1:10], "..."), collapse = ", ")
        } else {
            paste(unique_coord_strings, collapse = ", ")
        }

        error_message <- paste0(
            "- These X/Y values are outside the boundaries of TAMR: ",
            display_coords,
            sprintf(" (unexpected coordinates occurred %d times).", nrow(invalid_coords)),
            "<br><br>"
        )
        return(error_message)
    }
}


# Define primary functions ---------------------------
## Check df list for required sheets
func_validate_fisher_1per_sheets_check <- function(df_list, datatype) {
    if (datatype == "Lobster") {
        check <- sheets_check(df_list, c("Lobster"))
        return(check)
    }
    if (datatype == "Conch") {
        check <- sheets_check(df_list, c("Conch"))
        return(check)
    }
    if (datatype == "Finfish") {
        check <- sheets_check(df_list, c("Finfish"))
        return(check)
    }
}
func_validate_fisher_1per_sheets <- function(df_list, datatype) {
    if (datatype == "Lobster") {
        sheets(df_list, c("Lobster"))
    }
    if (datatype == "Conch") {
        sheets(df_list, c("Conch"))
    }
    if (datatype == "Finfish") {
        sheets(df_list, c("Finfish"))
    }
}
## Check sheets for required columns (completeness)
func_validate_fisher_1per_completeness_check <- function(data_sheet, datatype) {
    required_columns_lobster <- c(
        "Y", "X", "Total Lbs of Catch", "Hours Fished", "Type of Fishing Gears",
        "Weight type", "Sex", "Carapace Length (mm)", "Weight (G)"
    )
    required_columns_conch <- c(
        "Y", "X", "Total Lbs of Catch", "Hours Fished", "Weight Type", "Weight (G)"
    )
    required_columns_finfish <- c(
        "Y", "X", "Total Lbs of Catch", "Hours Fished", "Type of Gear", "Fish Species",
        "Weight Type", "Total Length (cm)", "Fork Length (cm)", "Weight (lbs)"
    )
    if (datatype == "Lobster") {
        required_columns <- required_columns_lobster
    } else if (datatype == "Conch") {
        required_columns <- required_columns_conch
    } else if (datatype == "Finfish") {
        required_columns <- required_columns_finfish
    }
    complete <- completeness_check(data_sheet, required_columns)
    check <- is.logical(complete) && all(complete)
    return(check)
}
func_validate_fisher_1per_completeness <- function(data_sheet, datatype) {
    required_columns_lobster <- c(
        "Y", "X", "Total Lbs of Catch", "Hours Fished", "Type of Fishing Gears",
        "Weight type", "Sex", "Carapace Length (mm)", "Weight (G)"
    )
    required_columns_conch <- c(
        "Y", "X", "Total Lbs of Catch", "Hours Fished", "Weight Type", "Weight (G)"
    )
    required_columns_finfish <- c(
        "Y", "X", "Total Lbs of Catch", "Hours Fished", "Type of Gear", "Fish Species",
        "Weight Type", "Total Length (cm)", "Fork Length (cm)", "Weight (lbs)"
    )
    if (datatype == "Lobster") {
        required_columns <- required_columns_lobster
    } else if (datatype == "Conch") {
        required_columns <- required_columns_conch
    } else if (datatype == "Finfish") {
        required_columns <- required_columns_finfish
    }
    complete <- completeness(data_sheet, required_columns, datatype)
    paste0(c(complete))
}
## Perform validation for lobster sheet
func_validate_fisher_1per_lobster_check <- function(lobster_sheet) {
    coords_lobster_valid <- validate_coords_check(lobster_sheet$X, lobster_sheet$Y)
    return(all(c(coords_lobster_valid)))
}
func_validate_fisher_1per_lobster <- function(lobster_sheet) {
    coords_lobster_valid <- validate_coords(lobster_sheet$X, lobster_sheet$Y)
    return(paste(coords_lobster_valid))
}
## Perform validation for conch sheet
func_validate_fisher_1per_conch_check <- function(conch_sheet) {
    coords_conch_valid <- validate_coords_check(conch_sheet$X, conch_sheet$Y)
    return(all(c(coords_conch_valid)))
}
func_validate_fisher_1per_conch <- function(conch_sheet) {
    coords_conch_valid <- validate_coords(conch_sheet$X, conch_sheet$Y)
    return(paste(coords_conch_valid))
}
## Perform validation for finfish sheet
func_validate_fisher_1per_finfish_check <- function(finfish_sheet) {
    coords_finfish_valid <- validate_coords_check(finfish_sheet$X, finfish_sheet$Y)
    return(all(c(coords_finfish_valid)))
}
func_validate_fisher_1per_finfish <- function(finfish_sheet) {
    coords_finfish_valid <- validate_coords(finfish_sheet$X, finfish_sheet$Y)
    return(paste(coords_finfish_valid))
}
