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
## Total Lbs of Catch
validate_lbcatch_check <- function(lbcatch_col) {
    lbcatch_col <- as.numeric(lbcatch_col)
    valid <- (lbcatch_col <= 100 & lbcatch_col > 0) | lbcatch_col == "MISSING" | is.na(lbcatch_col)
    return(all(valid))
}
validate_lbcatch <- function(lbcatch_col) {
    lbcatch_col <- as.numeric(lbcatch_col)
    valid <- (lbcatch_col <= 100 & lbcatch_col > 0) | lbcatch_col == "MISSING" | is.na(lbcatch_col)
    if (all(valid)) {
    } else {
        invalid_values <- unique(lbcatch_col[!valid])
        error_message <- paste0(
            "- These Total Lbs of Catch values are unexpected: ",
            paste(invalid_values, collapse = ", "),
            paste0(" (unexpected values occurred ", length(lbcatch_col[!valid]), " times). Please double check these values."),
            "<br><br>"
        )
        return(error_message)
    }
}
## Hours Fished
validate_hrs_check <- function(hrs_col) {
    hrs_col <- as.numeric(hrs_col)
    valid <- (hrs_col <= 20 & hrs_col >= 0.5) | hrs_col == "MISSING" | is.na(hrs_col)
    return(all(valid))
}
validate_hrs <- function(hrs_col) {
    hrs_col <- as.numeric(hrs_col)
    valid <- (hrs_col <= 20 & hrs_col >= 0.5) | hrs_col == "MISSING" | is.na(hrs_col)
    if (all(valid)) {
    } else {
        invalid_values <- unique(hrs_col[!valid])
        error_message <- paste0(
            "- These Hours Fished values are unexpected: ",
            paste(invalid_values, collapse = ", "),
            paste0(" (unexpected values occurred ", length(hrs_col[!valid]), " times). Please double check these values."),
            "<br><br>"
        )
        return(error_message)
    }
}
## `Type of Fishing Gears` or `Type of Gear`
validate_gear_check <- function(gear_col) {
    gear_col <- as.character(gear_col)
    valid <- gear_col %in% (c("Hook Stick", "Shade", "Snare", "Spear Guns", "Fish Traps", "Handline", "Speargun")) | gear_col == "MISSING" | is.na(gear_col)
    return(all(valid))
}
validate_gear <- function(gear_col, column_name) {
    gear_col <- as.character(gear_col)
    valid <- gear_col %in% (c("Hook Stick", "Shade", "Snare", "Spear Guns", "Fish Traps", "Handline", "Speargun")) | gear_col == "MISSING" | is.na(gear_col)
    if (all(valid)) {
    } else {
        invalid_values <- unique(gear_col[!valid])
        error_message <- paste0(
            "- These ", column_name, " values are unexpected: ",
            paste(invalid_values, collapse = ", "),
            paste0(" (unexpected values occurred ", length(gear_col[!valid]), " times). Please double check these values."),
            "<br><br>"
        )
        return(error_message)
    }
}

# Define helper functions for Lobster sheet ---------------------------
required_colum_lobster <- c(
    "Type of Fishing Gears",
    "Weight type", "Sex", "Carapace Length (mm)", "Weight (G)"
)

# Define helper functions for Conch sheet ---------------------------
required_colum_conch <- c(
    "Weight Type", "Weight (G)"
)

# Define helper functions for Finfish sheet ---------------------------
required_colum_finfish <- c(
    "Type of Gear", "Fish Species",
    "Weight Type", "Total Length (cm)", "Fork Length (cm)", "Weight (lbs)"
)

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
    coords_valid <- validate_coords_check(lobster_sheet$X, lobster_sheet$Y)
    lbscatch_valid <- validate_lbcatch_check(lobster_sheet$`Total Lbs of Catch`)
    hrs_valid <- validate_hrs_check(lobster_sheet$`Hours Fished`)
    gear_valid <- validate_gear_check(lobster_sheet$`Type of Fishing Gears`)
    return(all(c(coords_valid, lbscatch_valid, hrs_valid, gear_valid)))
}
func_validate_fisher_1per_lobster <- function(lobster_sheet) {
    coords_valid <- validate_coords(lobster_sheet$X, lobster_sheet$Y)
    lbscatch_valid <- validate_lbcatch(lobster_sheet$`Total Lbs of Catch`)
    hrs_valid <- validate_hrs(lobster_sheet$`Hours Fished`)
    gear_valid <- validate_gear(lobster_sheet$`Type of Fishing Gears`, "Type of Fishing Gears")
    return(paste(coords_valid, lbscatch_valid, hrs_valid, gear_valid))
}
## Perform validation for conch sheet
func_validate_fisher_1per_conch_check <- function(conch_sheet) {
    coords_valid <- validate_coords_check(conch_sheet$X, conch_sheet$Y)
    lbscatch_valid <- validate_lbcatch_check(conch_sheet$`Total Lbs of Catch`)
    hrs_valid <- validate_hrs_check(conch_sheet$`Hours Fished`)
    return(all(c(coords_valid, lbscatch_valid, hrs_valid)))
}
func_validate_fisher_1per_conch <- function(conch_sheet) {
    coords_valid <- validate_coords(conch_sheet$X, conch_sheet$Y)
    lbscatch_valid <- validate_lbcatch(conch_sheet$`Total Lbs of Catch`)
    hrs_valid <- validate_hrs(conch_sheet$`Hours Fished`)
    return(paste(coords_valid, lbscatch_valid, hrs_valid))
}
## Perform validation for finfish sheet
func_validate_fisher_1per_finfish_check <- function(finfish_sheet) {
    coords_valid <- validate_coords_check(finfish_sheet$X, finfish_sheet$Y)
    lbscatch_valid <- validate_lbcatch_check(finfish_sheet$`Total Lbs of Catch`)
    hrs_valid <- validate_hrs_check(finfish_sheet$`Hours Fished`)
    gear_valid <- validate_gear_check(finfish_sheet$`Type of Gear`)
    return(all(c(coords_valid, lbscatch_valid, hrs_valid, gear_valid)))
}
func_validate_fisher_1per_finfish <- function(finfish_sheet) {
    coords_valid <- validate_coords(finfish_sheet$X, finfish_sheet$Y)
    lbscatch_valid <- validate_lbcatch(finfish_sheet$`Total Lbs of Catch`)
    hrs_valid <- validate_hrs(finfish_sheet$`Hours Fished`)
    gear_valid <- validate_gear(finfish_sheet$`Type of Gear`, "Type of Gear")
    return(paste(coords_valid, lbscatch_valid, hrs_valid, gear_valid))
}
