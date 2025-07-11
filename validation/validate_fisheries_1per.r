## validate_fisheries_1per.r

# Source general helper functions ---------------------------
source("validation/general_validation_helpers.r")

# Define helper functions for multiple sheets ---------------------------

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

# Define helper functions for Lobster sheet ---------------------------
## Weight Type
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
            "- These Weight Type values are unexpected: ",
            paste(invalid_values, collapse = ", "),
            paste0(" (unexpected values occurred ", length(weighttype_col[!valid]), " times).
            Weight Type should be headed or whole. Please double check these values."),
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
## Total Weight (g)
validate_weight_lobster_check <- function(weight_col) {
    valid <- (as.numeric(weight_col) >= 298 & as.numeric(weight_col) <= 2495) | weight_col == "MISSING" | is.na(weight_col)
    return(all(valid))
}
validate_weight_lobster <- function(weight_col) {
    valid <- (as.numeric(weight_col) >= 298 & as.numeric(weight_col) <= 2495) | weight_col == "MISSING" | is.na(weight_col)
    if (all(valid)) {
    } else {
        invalid_values <- unique(weight_col[!valid])
        error_message <- paste0(
            "- These Total Weight (g) values are unexpected: ",
            paste(invalid_values, collapse = ", "),
            paste0(" (unexpected values occurred ", length(weight_col[!valid]), " times). Please double check these values."),
            "<br><br>"
        )
        return(error_message)
    }
}
## Carapace Length (mm)
validate_carapace_check <- function(carapace_col) {
    valid <- (as.numeric(carapace_col) >= 71 & as.numeric(carapace_col) <= 178) | carapace_col == "MISSING" | is.na(carapace_col)
    return(all(valid))
}
validate_carapace <- function(carapace_col) {
    valid <- (as.numeric(carapace_col) >= 71 & as.numeric(carapace_col) <= 178) | carapace_col == "MISSING" | is.na(carapace_col)
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
## Pleopod Stage
validate_pleopod_lobster_check <- function(pleopod_col) {
    pleopod_col <- as.character(pleopod_col)
    valid <- pleopod_col %in% (c("headed", "whole")) | pleopod_col == "MISSING" | is.na(pleopod_col)
    return(all(valid))
}
validate_pleopod_lobster <- function(pleopod_col) {
    pleopod_col <- as.character(pleopod_col)
    valid <- pleopod_col %in% (c("1", "2", "3", "I", "II", "III")) | pleopod_col == "MISSING" | is.na(pleopod_col)
    if (all(valid)) {
    } else {
        invalid_values <- unique(pleopod_col[!valid])
        error_message <- paste0(
            "- These Pleopod Stage values are unexpected: ",
            paste(invalid_values, collapse = ", "),
            paste0(" (unexpected values occurred ", length(pleopod_col[!valid]), " times).
            Pleopod Stage should be 1, 2, 3, I, II, or III. Please double check these values."),
            "<br><br>"
        )
        return(error_message)
    }
}

# Define helper functions for Conch sheet ---------------------------
## Weight Type
validate_weighttype_conch_check <- function(weighttype_col) {
    weighttype_col <- as.character(weighttype_col)
    valid <- weighttype_col %in% (c("Market Clean", "Fillet", "Unprocessed", "Whole", "Whole in Shell")) | weighttype_col == "MISSING" | is.na(weighttype_col)
    return(all(valid))
}
validate_weighttype_conch <- function(weighttype_col) {
    weighttype_col <- as.character(weighttype_col)
    valid <- weighttype_col %in% (c("Market Clean", "Fillet", "Unprocessed", "Whole", "Whole in Shell")) | weighttype_col == "MISSING" | is.na(weighttype_col)
    if (all(valid)) {
    } else {
        invalid_values <- unique(weighttype_col[!valid])
        error_message <- paste0(
            "- These Weight Type values are unexpected: ",
            paste(invalid_values, collapse = ", "),
            paste0(" (unexpected values occurred ", length(weighttype_col[!valid]), " times).
            Weight Type should be Market Clean, Fillet, Unprocessed, Whole, or Whole in Shell. Please double check these values."),
            "<br><br>"
        )
        return(error_message)
    }
}
## Weight (g)
validate_weight_conch_check <- function(weight_col) {
    valid <- (as.numeric(weight_col) >= 150 & as.numeric(weight_col) <= 992) | weight_col == "MISSING" | is.na(weight_col)
    return(all(valid))
}
validate_weight_conch <- function(weight_col) {
    valid <- (as.numeric(weight_col) >= 150 & as.numeric(weight_col) <= 992) | weight_col == "MISSING" | is.na(weight_col)
    if (all(valid)) {
    } else {
        invalid_values <- unique(weight_col[!valid])
        error_message <- paste0(
            "- These Weight (g) values are unexpected: ",
            paste(invalid_values, collapse = ", "),
            paste0(" (unexpected values occurred ", length(weight_col[!valid]), " times). Please double check these values."),
            "<br><br>"
        )
        return(error_message)
    }
}
## Shell Length (mm)
validate_shelllength_conch_check <- function(shell_col) {
    shell_col <- as.numeric(shell_col)
    valid <- (shell_col > 0) | shell_col == "MISSING" | is.na(shell_col)
    return(all(valid))
}
validate_shelllength_conch <- function(shell_col) {
    shell_col <- as.numeric(shell_col)
    valid <- (shell_col > 0) | shell_col == "MISSING" | is.na(shell_col)
    if (all(valid)) {
    } else {
        invalid_values <- unique(shell_col[!valid])
        error_message <- paste0(
            "- These Shell Length (mm) values are unexpected: ",
            paste(invalid_values, collapse = ", "),
            paste0(" (unexpected values occurred ", length(shell_col[!valid]), " times). Please double check these values."),
            "<br><br>"
        )
        return(error_message)
    }
}
## Lip Thickness (mm)
validate_lipthickness_conch_check <- function(lip_col) {
    lip_col <- as.numeric(lip_col)
    valid <- (lip_col >= 0) | lip_col == "MISSING" | is.na(lip_col)
    return(all(valid))
}
validate_lipthickness_conch <- function(lip_col) {
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

# Define helper functions for Finfish sheet ---------------------------
## Weight (g)
validate_weight_finfish_check <- function(weight_col) {
    valid <- (as.numeric(weight_col) >= 113 & as.numeric(weight_col) <= 68039) | weight_col == "MISSING" | is.na(weight_col)
    return(all(valid))
}
validate_weight_finfish <- function(weight_col) {
    valid <- (as.numeric(weight_col) >= 113 & as.numeric(weight_col) <= 68039) | weight_col == "MISSING" | is.na(weight_col)
    if (all(valid)) {
    } else {
        invalid_values <- unique(weight_col[!valid])
        error_message <- paste0(
            "- These Weight (g) values are unexpected: ",
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
func_validate_fisheries_1per_sheets_check <- function(df_list, datatype) {
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
func_validate_fisheries_1per_sheets <- function(df_list, datatype) {
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
func_validate_fisheries_1per_completeness_check <- function(data_sheet, datatype) {
    required_columns_lobster <- c(
        "Hours Fished", "Weight Type", "Sex", "Carapace Length (mm)", "Total Weight (g)", "Pleopod Stage"
    )
    required_columns_conch <- c(
        "Hours Fished", "Weight Type", "Weight (g)", "Shell Length (mm)", "Lip Thickness (mm)"
    )
    required_columns_finfish <- c(
        "Hours Fished", "Fish Species", "Total Length (cm)", "Fork Length (cm)", "Weight (g)"
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
func_validate_fisheries_1per_completeness <- function(data_sheet, datatype) {
    required_columns_lobster <- c(
        "Hours Fished", "Weight Type", "Sex", "Carapace Length (mm)", "Total Weight (g)", "Pleopod Stage"
    )
    required_columns_conch <- c(
        "Hours Fished", "Weight Type", "Weight (g)", "Shell Length (mm)", "Lip Thickness (mm)"
    )
    required_columns_finfish <- c(
        "Hours Fished", "Fish Species", "Total Length (cm)", "Fork Length (cm)", "Weight (g)"
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
func_validate_fisheries_1per_lobster_check <- function(lobster_sheet) {
    hrs_valid <- validate_hrs_check(lobster_sheet$`Hours Fished`)
    weighttype_valid <- validate_weighttype_lobster_check(lobster_sheet$`Weight Type`)
    sex_valid <- validate_sex_check(lobster_sheet$Sex)
    weight_valid <- validate_weight_lobster_check(lobster_sheet$`Total Weight (g)`)
    carapace_valid <- validate_carapace_check(lobster_sheet$`Carapace Length (mm)`)
    pleopod_valid <- validate_pleopod_lobster_check(lobster_sheet$`Pleopod Stage`)
    return(all(c(
        hrs_valid, weighttype_valid, sex_valid, weight_valid, carapace_valid, pleopod_valid
    )))
}
func_validate_fisheries_1per_lobster <- function(lobster_sheet) {
    hrs_valid <- validate_hrs(lobster_sheet$`Hours Fished`)
    weighttype_valid <- validate_weighttype_lobster(lobster_sheet$`Weight Type`)
    sex_valid <- validate_sex(lobster_sheet$Sex)
    weight_valid <- validate_weight_lobster(lobster_sheet$`Total Weight (g)`)
    carapace_valid <- validate_carapace(lobster_sheet$`Carapace Length (mm)`)
    pleopod_valid <- validate_pleopod_lobster(lobster_sheet$`Pleopod Stage`)
    return(paste(
        hrs_valid, weighttype_valid, sex_valid, weight_valid, carapace_valid, pleopod_valid
    ))
}
## Perform validation for conch sheet
func_validate_fisheries_1per_conch_check <- function(conch_sheet) {
    hrs_valid <- validate_hrs_check(conch_sheet$`Hours Fished`)
    weighttype_valid <- validate_weighttype_conch_check(conch_sheet$`Weight Type`)
    weight_valid <- validate_weight_conch_check(conch_sheet$`Weight (g)`)
    thickness_valid <- validate_lipthickness_conch_check(conch_sheet$`Lip Thickness (mm)`)
    length_valid <- validate_shelllength_conch_check(conch_sheet$`Shell Length (mm)`)
    return(all(c(hrs_valid, weighttype_valid, weight_valid, thickness_valid, length_valid)))
}
func_validate_fisheries_1per_conch <- function(conch_sheet) {
    hrs_valid <- validate_hrs(conch_sheet$`Hours Fished`)
    weighttype_valid <- validate_weighttype_conch(conch_sheet$`Weight Type`)
    weight_valid <- validate_weight_conch(conch_sheet$`Weight (g)`)
    thickness_valid <- validate_lipthickness_conch(conch_sheet$`Lip Thickness (mm)`)
    length_valid <- validate_shelllength_conch(conch_sheet$`Shell Length (mm)`)
    return(paste(hrs_valid, weighttype_valid, weight_valid, thickness_valid, length_valid))
}
## Perform validation for finfish sheet
func_validate_fisheries_1per_finfish_check <- function(finfish_sheet) {
    hrs_valid <- validate_hrs_check(finfish_sheet$`Hours Fished`)
    weight_valid <- validate_weight_finfish_check(finfish_sheet$`Weight (g)`)
    species_valid <- validate_species_check(finfish_sheet$`Fish Species`)
    totallength_valid <- validate_totallength_check(finfish_sheet$`Total Length (cm)`)
    forklength_valid <- validate_forklength_check(finfish_sheet$`Fork Length (cm)`)
    return(all(c(
        hrs_valid, weight_valid, species_valid, totallength_valid, forklength_valid
    )))
}
func_validate_fisheries_1per_finfish <- function(finfish_sheet) {
    hrs_valid <- validate_hrs(finfish_sheet$`Hours Fished`)
    weight_valid <- validate_weight_finfish(finfish_sheet$`Weight (g)`)
    species_valid <- validate_species(finfish_sheet$`Fish Species`)
    totallength_valid <- validate_totallength(finfish_sheet$`Total Length (cm)`)
    forklength_valid <- validate_forklength(finfish_sheet$`Fork Length (cm)`)
    return(paste(
        hrs_valid, weight_valid, species_valid, totallength_valid, forklength_valid
    ))
}
