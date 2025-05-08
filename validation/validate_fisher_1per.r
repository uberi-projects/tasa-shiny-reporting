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
validate_lbcatch_check <- function(lbcatch_col, max) {
    valid <- (as.numeric(lbcatch_col) <= max & as.numeric(lbcatch_col) > 0) | lbcatch_col == "MISSING" | is.na(lbcatch_col)
    return(all(valid))
}
validate_lbcatch <- function(lbcatch_col, max) {
    valid <- (as.numeric(lbcatch_col) <= max & as.numeric(lbcatch_col) > 0) | lbcatch_col == "MISSING" | is.na(lbcatch_col)
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
    valid <- (as.numeric(hrs_col) <= 20 & as.numeric(hrs_col) >= 0.5) | hrs_col == "MISSING" | is.na(hrs_col)
    return(all(valid))
}
validate_hrs <- function(hrs_col) {
    valid <- (as.numeric(hrs_col) <= 20 & as.numeric(hrs_col) >= 0.5) | hrs_col == "MISSING" | is.na(hrs_col)
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
## Weight type
validate_weighttype_lobster_check <- function(weighttype_col) {
    weighttype_col <- as.character(weighttype_col)
    valid <- weighttype_col %in% (c("headed", "whole")) | weighttype_col == "MISSING" | is.na(weighttype_col)
    return(all(valid))
}
validate_weighttype_lobster <- function(weighttype_col) {
    weighttype_col <- as.character(weighttype_col)
    valid <- weighttype_col %in% (c("headed", "whole")) | weighttype_col == "MISSING" | is.na(weighttype_col)
    if (all(valid)) {
    } else {
        invalid_values <- unique(weighttype_col[!valid])
        error_message <- paste0(
            "- These Weight type values are unexpected: ",
            paste(invalid_values, collapse = ", "),
            paste0(" (unexpected values occurred ", length(weighttype_col[!valid]), " times).
            Weight type should be headed or whole. Please double check these values."),
            "<br><br>"
        )
        return(error_message)
    }
}
## Sex
validate_sex_check <- function(sex_col) {
    sex_col <- as.character(sex_col)
    valid <- sex_col %in% (c("Male", "Female")) | sex_col == "MISSING" | is.na(sex_col)
    return(all(valid))
}
validate_sex <- function(sex_col) {
    sex_col <- as.character(sex_col)
    valid <- sex_col %in% (c("Male", "Female")) | sex_col == "MISSING" | is.na(sex_col)
    if (all(valid)) {
    } else {
        invalid_values <- unique(sex_col[!valid])
        error_message <- paste0(
            "- These Sex values are unexpected: ",
            paste(invalid_values, collapse = ", "),
            paste0(" (unexpected values occurred ", length(sex_col[!valid]), " times).
            Sex should be Male or Female. Please double check these values."),
            "<br><br>"
        )
        return(error_message)
    }
}
## Weight (G)
validate_weight_lobster_check <- function(weight_col) {
    valid <- (as.numeric(weight_col) >= 300 & as.numeric(weight_col) <= 2500) | weight_col == "MISSING" | is.na(weight_col)
    return(all(valid))
}
validate_weight_lobster <- function(weight_col) {
    valid <- (as.numeric(weight_col) >= 300 & as.numeric(weight_col) <= 2500) | weight_col == "MISSING" | is.na(weight_col)
    if (all(valid)) {
    } else {
        invalid_values <- unique(weight_col[!valid])
        error_message <- paste0(
            "- These Weight (G) values are unexpected: ",
            paste(invalid_values, collapse = ", "),
            paste0(" (unexpected values occurred ", length(weight_col[!valid]), " times). Please double check these values."),
            "<br><br>"
        )
        return(error_message)
    }
}
## Carapace Length (mm)
validate_carapace_check <- function(carapace_col) {
    valid <- (as.numeric(carapace_col) >= 70 & as.numeric(carapace_col) <= 180) | carapace_col == "MISSING" | is.na(carapace_col)
    return(all(valid))
}
validate_carapace <- function(carapace_col) {
    valid <- (as.numeric(carapace_col) >= 70 & as.numeric(carapace_col) <= 180) | carapace_col == "MISSING" | is.na(carapace_col)
    if (all(valid)) {
    } else {
        invalid_values <- unique(carapace_col[!valid])
        error_message <- paste0(
            "- These Carapace Length (mm) values are unexpected: ",
            paste(invalid_values, collapse = ", "),
            paste0(" (unexpected values occurred ", length(carapace_col[!valid]), " times). Please double check these values."),
            "<br><br>"
        )
        return(error_message)
    }
}

# Define helper functions for Conch sheet ---------------------------
## Weight Type
validate_weighttype_conch_check <- function(weighttype_col) {
    weighttype_col <- as.character(weighttype_col)
    valid <- weighttype_col %in% (c("Market Clean", "Fillet")) | weighttype_col == "MISSING" | is.na(weighttype_col)
    return(all(valid))
}
validate_weighttype_conch <- function(weighttype_col) {
    weighttype_col <- as.character(weighttype_col)
    valid <- weighttype_col %in% (c("Market Clean", "Fillet")) | weighttype_col == "MISSING" | is.na(weighttype_col)
    if (all(valid)) {
    } else {
        invalid_values <- unique(weighttype_col[!valid])
        error_message <- paste0(
            "- These Weight Type values are unexpected: ",
            paste(invalid_values, collapse = ", "),
            paste0(" (unexpected values occurred ", length(weighttype_col[!valid]), " times).
            Weight Type should be Market Clean or Fillet. Please double check these values."),
            "<br><br>"
        )
        return(error_message)
    }
}
## Weight (G)
validate_weight_conch_check <- function(weight_col) {
    valid <- (as.numeric(weight_col) >= 150 & as.numeric(weight_col) <= 1000) | weight_col == "MISSING" | is.na(weight_col)
    return(all(valid))
}
validate_weight_conch <- function(weight_col) {
    valid <- (as.numeric(weight_col) >= 150 & as.numeric(weight_col) <= 1000) | weight_col == "MISSING" | is.na(weight_col)
    if (all(valid)) {
    } else {
        invalid_values <- unique(weight_col[!valid])
        error_message <- paste0(
            "- These Weight (G) values are unexpected: ",
            paste(invalid_values, collapse = ", "),
            paste0(" (unexpected values occurred ", length(weight_col[!valid]), " times). Please double check these values."),
            "<br><br>"
        )
        return(error_message)
    }
}

# Define helper functions for Finfish sheet ---------------------------
## Weight Type
validate_weighttype_finfish_check <- function(weighttype_col) {
    weighttype_col <- as.character(weighttype_col)
    valid <- weighttype_col %in% (c("Gutted", "Whole")) | weighttype_col == "MISSING" | is.na(weighttype_col)
    return(all(valid))
}
validate_weighttype_finfish <- function(weighttype_col) {
    weighttype_col <- as.character(weighttype_col)
    valid <- weighttype_col %in% (c("Gutted", "Whole")) | weighttype_col == "MISSING" | is.na(weighttype_col)
    if (all(valid)) {
    } else {
        invalid_values <- unique(weighttype_col[!valid])
        error_message <- paste0(
            "- These Weight Type values are unexpected: ",
            paste(invalid_values, collapse = ", "),
            paste0(" (unexpected values occurred ", length(weighttype_col[!valid]), " times).
            Weight Type should be Gutted or Whole. Please double check these values."),
            "<br><br>"
        )
        return(error_message)
    }
}
## Weight (lbs)
validate_weight_finfish_check <- function(weight_col) {
    valid <- (as.numeric(weight_col) >= 0.25 & as.numeric(weight_col) <= 150) | weight_col == "MISSING" | is.na(weight_col)
    return(all(valid))
}
validate_weight_finfish <- function(weight_col) {
    valid <- (as.numeric(weight_col) >= 0.25 & as.numeric(weight_col) <= 150) | weight_col == "MISSING" | is.na(weight_col)
    if (all(valid)) {
    } else {
        invalid_values <- unique(weight_col[!valid])
        error_message <- paste0(
            "- These Weight (lbs) values are unexpected: ",
            paste(invalid_values, collapse = ", "),
            paste0(" (unexpected values occurred ", length(weight_col[!valid]), " times). Please double check these values."),
            "<br><br>"
        )
        return(error_message)
    }
}
## Fish Species
validate_species_check <- function(species_col) {
    valid <- is.character(species_col) | is.na(species_col)
    return(all(valid))
}
validate_species <- function(species_col) {
    valid <- is.character(species_col) | is.na(species_col)
    if (all(valid)) {
    } else {
        invalid_values <- unique(species_col[!valid])
        error_message <- paste0(
            "- These Fish Species values are unexpected: ",
            paste(invalid_values, collapse = ", "),
            paste0(" (unexpected values occurred ", length(species_col[!valid]), " times). Please double check these values."),
            "<br><br>"
        )
        return(error_message)
    }
}
## Total Length (cm)
validate_totallength_check <- function(totallength_col) {
    valid <- (as.numeric(totallength_col) >= 10 & as.numeric(totallength_col) <= 150) | totallength_col == "MISSING" | is.na(totallength_col)
    return(all(valid))
}
validate_totallength <- function(totallength_col) {
    valid <- (as.numeric(totallength_col) >= 10 & as.numeric(totallength_col) <= 150) | totallength_col == "MISSING" | is.na(totallength_col)
    if (all(valid)) {
    } else {
        invalid_values <- unique(totallength_col[!valid])
        error_message <- paste0(
            "- These Total Length (cm) values are unexpected: ",
            paste(invalid_values, collapse = ", "),
            paste0(" (unexpected values occurred ", length(totallength_col[!valid]), " times). Please double check these values."),
            "<br><br>"
        )
        return(error_message)
    }
}
## Fork Length (cm)
validate_forklength_check <- function(forklength_col) {
    valid <- (as.numeric(forklength_col) >= 10 & as.numeric(forklength_col) <= 140) | forklength_col == "MISSING" | is.na(forklength_col)
    return(all(valid))
}
validate_forklength <- function(forklength_col) {
    valid <- (as.numeric(forklength_col) >= 10 & as.numeric(forklength_col) <= 140) | forklength_col == "MISSING" | is.na(forklength_col)
    if (all(valid)) {
    } else {
        invalid_values <- unique(forklength_col[!valid])
        error_message <- paste0(
            "- These Fork Length (cm) values are unexpected: ",
            paste(invalid_values, collapse = ", "),
            paste0(" (unexpected values occurred ", length(forklength_col[!valid]), " times). Please double check these values."),
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
    coords_valid <- validate_coords_check(lobster_sheet$X, lobster_sheet$Y)
    lbscatch_valid <- validate_lbcatch_check(lobster_sheet$`Total Lbs of Catch`, 300)
    hrs_valid <- validate_hrs_check(lobster_sheet$`Hours Fished`)
    gear_valid <- validate_gear_check(lobster_sheet$`Type of Fishing Gears`)
    weighttype_valid <- validate_weighttype_lobster_check(lobster_sheet$`Weight type`)
    sex_valid <- validate_sex_check(lobster_sheet$Sex)
    weight_valid <- validate_weight_lobster_check(lobster_sheet$`Weight (G)`)
    carapace_valid <- validate_carapace_check(lobster_sheet$`Carapace Length (mm)`)
    return(all(c(
        coords_valid, lbscatch_valid, hrs_valid,
        gear_valid, weighttype_valid, sex_valid, weight_valid, carapace_valid
    )))
}
func_validate_fisher_1per_lobster <- function(lobster_sheet) {
    coords_valid <- validate_coords(lobster_sheet$X, lobster_sheet$Y)
    lbscatch_valid <- validate_lbcatch(lobster_sheet$`Total Lbs of Catch`, 300)
    hrs_valid <- validate_hrs(lobster_sheet$`Hours Fished`)
    gear_valid <- validate_gear(lobster_sheet$`Type of Fishing Gears`, "Type of Fishing Gears")
    weighttype_valid <- validate_weighttype_lobster(lobster_sheet$`Weight type`)
    sex_valid <- validate_sex(lobster_sheet$Sex)
    weight_valid <- validate_weight_lobster(lobster_sheet$`Weight (G)`)
    carapace_valid <- validate_carapace(lobster_sheet$`Carapace Length (mm)`)
    return(paste(
        coords_valid, lbscatch_valid, hrs_valid, gear_valid,
        weighttype_valid, sex_valid, weight_valid, carapace_valid
    ))
}
## Perform validation for conch sheet
func_validate_fisher_1per_conch_check <- function(conch_sheet) {
    coords_valid <- validate_coords_check(conch_sheet$X, conch_sheet$Y)
    lbscatch_valid <- validate_lbcatch_check(conch_sheet$`Total Lbs of Catch`, 500)
    hrs_valid <- validate_hrs_check(conch_sheet$`Hours Fished`)
    weighttype_valid <- validate_weighttype_conch_check(conch_sheet$`Weight Type`)
    weight_valid <- validate_weight_conch_check(conch_sheet$`Weight (G)`)
    return(all(c(coords_valid, lbscatch_valid, hrs_valid, weighttype_valid, weight_valid)))
}
func_validate_fisher_1per_conch <- function(conch_sheet) {
    coords_valid <- validate_coords(conch_sheet$X, conch_sheet$Y)
    lbscatch_valid <- validate_lbcatch(conch_sheet$`Total Lbs of Catch`, 500)
    hrs_valid <- validate_hrs(conch_sheet$`Hours Fished`)
    weighttype_valid <- validate_weighttype_conch(conch_sheet$`Weight Type`)
    weight_valid <- validate_weight_conch(conch_sheet$`Weight (G)`)
    return(paste(coords_valid, lbscatch_valid, hrs_valid, weighttype_valid, weight_valid))
}
## Perform validation for finfish sheet
func_validate_fisher_1per_finfish_check <- function(finfish_sheet) {
    coords_valid <- validate_coords_check(finfish_sheet$X, finfish_sheet$Y)
    lbscatch_valid <- validate_lbcatch_check(finfish_sheet$`Total Lbs of Catch`, 1000)
    hrs_valid <- validate_hrs_check(finfish_sheet$`Hours Fished`)
    gear_valid <- validate_gear_check(finfish_sheet$`Type of Gear`)
    weighttype_valid <- validate_weighttype_finfish_check(finfish_sheet$`Weight Type`)
    weight_valid <- validate_weight_finfish_check(finfish_sheet$`Weight (lbs)`)
    species_valid <- validate_species_check(finfish_sheet$`Fish Species`)
    totallength_valid <- validate_totallength_check(finfish_sheet$`Total Length (cm)`)
    forklength_valid <- validate_forklength_check(finfish_sheet$`Fork Length (cm)`)
    return(all(c(
        coords_valid, lbscatch_valid, hrs_valid, gear_valid,
        weighttype_valid, weight_valid, species_valid, totallength_valid, forklength_valid
    )))
}
func_validate_fisher_1per_finfish <- function(finfish_sheet) {
    coords_valid <- validate_coords(finfish_sheet$X, finfish_sheet$Y)
    lbscatch_valid <- validate_lbcatch(finfish_sheet$`Total Lbs of Catch`, 1000)
    hrs_valid <- validate_hrs(finfish_sheet$`Hours Fished`)
    gear_valid <- validate_gear(finfish_sheet$`Type of Gear`, "Type of Gear")
    weighttype_valid <- validate_weighttype_finfish(finfish_sheet$`Weight Type`)
    weight_valid <- validate_weight_finfish(finfish_sheet$`Weight (lbs)`)
    species_valid <- validate_species(finfish_sheet$`Fish Species`)
    totallength_valid <- validate_totallength(finfish_sheet$`Total Length (cm)`)
    forklength_valid <- validate_forklength(finfish_sheet$`Fork Length (cm)`)
    return(paste(
        coords_valid, lbscatch_valid, hrs_valid, gear_valid,
        weighttype_valid, weight_valid, species_valid, totallength_valid, forklength_valid
    ))
}
