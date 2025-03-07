## validate_lampgeneral_1per.r

# Source general helper functions ---------------------------
source("validation/general_validation_helpers.r")

# Define general helper functions ---------------------------
func_validate_transect_check <- function(x) {
    valid <- (is.na(x) | x %in% c(1:4))
    return(all(valid))
}
func_validate_transect <- function(x) {
    valid <- (is.na(x) | x %in% c(1:4))
    if (all(valid, na.rm = TRUE)) {
    } else {
        invalid <- unique(x[!valid & !is.na(x)])
        return(paste0(
            "- Only 4 transects labelled 1 through 4 are expected per Site. These values are unexpected: ",
            paste(invalid, collapse = ", "),
            sprintf(" (unexpected values occurred %d times).", sum(!valid & !is.na(x))),
            "<br><br>"
        ))
    }
}

# Redefine helper functions to adjust based on presence of Finfish sheet ---------------------------
sheets_check <- function(df_list, required_sheets) {
    if ("Finfish" %in% names(df_list)) {
        required_sheets <- c(required_sheets, "Biomass", "Species")
    }
    missing_sheets <- setdiff(required_sheets, names(df_list))
    length(missing_sheets) == 0
}
sheets <- function(df_list, required_sheets, df_list_name) {
    if ("Finfish" %in% names(df_list)) {
        required_sheets <- c(required_sheets, "Biomass", "Species")
    }
    missing_sheets <- setdiff(required_sheets, names(df_list))
    if (length(missing_sheets) > 0) {
        paste("Missing required sheets from", df_list_name, "datafile:", paste(missing_sheets, collapse = ", "), "<br><br>")
    }
}

# Define helper functions for Species sheet ---------------------------
validate_grouping_check <- function(x) {
    valid <- (is.na(x) | x %in% c("Herbivorous", "Reef Fish", "Other Target", "Commercial"))
    return(all(valid))
}
validate_grouping <- function(x) {
    valid <- (is.na(x) | x %in% c("Herbivorous", "Reef Fish", "Other Target", "Commercial"))
    if (all(valid, na.rm = TRUE)) {
    } else {
        invalid <- unique(x[!valid & !is.na(x)])
        return(paste0(
            "- Grouping values are expected to either be Herbivorous, Reef Fish, Other Target, or Commercial.
            These Grouping values are unexpected and will not be included in the report: ",
            paste(invalid, collapse = ", "),
            sprintf(" (unexpected values occurred %d times).", sum(!valid & !is.na(x))),
            "<br><br>"
        ))
    }
}

# Define helper functions for Sites sheet ---------------------------
validate_site_check <- function(x) {
    valid <- (is.na(x) | grepl("^(GU|CZ|SM|LO)", x) | x == "MISSING")
    return(all(valid))
}
validate_site <- function(x) {
    x <- as.character(x)
    valid <- (is.na(x) | grepl("^(GU|CZ|SM|LO)", x) | x == "MISSING")
    if (all(valid)) {
    } else {
        invalid_values <- unique(x[!valid & !is.na(x)])
        error_message <- ""
        if (length(invalid_values) > 0) {
            error_message <- paste0(
                "- Site IDs are expected to begin with the prefix GU, CZ, SM, or LO. These Site IDs are unexpected: ",
                paste(invalid_values, collapse = ", "),
                sprintf(" (unexpected values occurred %d times).", sum(!valid & !is.na(x))),
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
validate_recorders_check <- function(x) {
    valid <- grepl("^[A-Za-z ]+(, [A-Za-z ]+)*$", x) | is.na(x)
    all(valid, na.rm = TRUE)
}
validate_recorders <- function(x) {
    valid <- grepl("^[A-Za-z ]+(, [A-Za-z ]+)*$", x) | is.na(x)
    if (all(valid, na.rm = TRUE)) {
    } else {
        invalid <- unique(x[!valid & !is.na(x)])
        return(paste0(
            "- These `Recorder(s)` values do not match the expected format (comma-separated full names): ",
            paste(invalid, collapse = ", "),
            sprintf(" (unexpected values occurred %d times).", sum(!valid & !is.na(x))),
            "<br><br>"
        ))
    }
}
validate_zone_check <- function(x) {
    valid <- (is.na(x) | x %in% c("General Use Zone", "Special Management Zone", "Conservation Zone") | x == "MISSING")
    return(all(valid))
}
validate_zone <- function(x) {
    x <- as.character(x)
    valid <- (is.na(x) | x %in% c("General Use Zone", "Special Management Zone", "Conservation Zone") | x == "MISSING")
    if (all(valid)) {
    } else {
        invalid_values <- unique(x[!valid & !is.na(x)])
        error_message <- ""
        if (length(invalid_values) > 0) {
            error_message <- paste0(
                "- These Mgmt Zone values are unexpected: ",
                paste(invalid_values, collapse = ", "),
                sprintf(" (unexpected values occurred %d times).", sum(!valid & !is.na(x))),
                "<br><br>"
            )
        }
        return(error_message)
    }
}

# Define helper functions for finfish sheet ---------------------------
validate_site_finfish_check <- function(x) {
    valid <- (is.na(x) | grepl("^(GU|CZ|SM|LO)", x) | x == "MISSING")
    return(all(valid))
}
func_validate_site_finfish <- function(x) {
    x <- as.character(x)
    valid <- (is.na(x) | grepl("^(GU|CZ|SM|LO)", x) | x == "MISSING")
    if (all(valid)) {
    } else {
        invalid_values <- unique(x[!valid & !is.na(x)])
        error_message <- ""
        if (length(invalid_values) > 0) {
            error_message <- paste0(
                "- Site IDs are expected to begin with the prefix GU, CZ, SM, or LO. These Site IDs are unexpected: ",
                paste(invalid_values, collapse = ", "),
                sprintf(" (unexpected values occurred %d times).", sum(!valid & !is.na(x))),
                "<br><br>"
            )
        }
        return(error_message)
    }
}
func_validate_site_finfish_match_check <- function(x, y) {
    valid <- is.na(x) | x %in% y
    return(all(valid))
}
func_validate_site_finfish_match <- function(x, y) {
    invalid_sites <- unique(x[!(is.na(x) | x %in% y)])
    if (length(invalid_sites) == 0) {
    } else {
        display_values <- if (length(invalid_sites) > 10) {
            paste(c(invalid_sites[1:10], "..."), collapse = ", ")
        } else {
            paste(invalid_sites, collapse = ", ")
        }
        return(paste0(
            "- Some Site IDs in the Finfish sheet do not match the surveyed Sites sheet: ",
            display_values,
            paste0(" (unexpected values occurred ", length(x[!(is.na(x) | x %in% y)]), " times)."),
            "<br><br>"
        ))
    }
}
func_validate_species_finfish_match_check <- function(x, y) {
    valid <- is.na(x) | x %in% y
    return(all(valid))
}
func_validate_species_finfish_match <- function(x, y) {
    invalid_species <- unique(x[!(is.na(x) | x %in% y)])
    if (length(invalid_species) == 0) {
    } else {
        display_values <- if (length(invalid_species) > 10) {
            paste(c(invalid_species[1:10], "..."), collapse = ", ")
        } else {
            paste(invalid_species, collapse = ", ")
        }
        return(paste0(
            "- Some Species in the Finfish sheet do not match with a species name on the Species sheet: ",
            display_values,
            paste0(" (unexpected values occurred ", length(x[!(is.na(x) | x %in% y)]), " times). Only Species with a match will be included in the report."),
            "<br><br>"
        ))
    }
}
func_validate_size_finfish_check <- function(x) {
    valid <- is.na(x) | x %in% c(0:1000) | x %in% as.character(c(0:1000))
    return(all(valid))
}
func_validate_size_finfish <- function(x, class) {
    valid <- is.na(x) | x %in% c(0:1000) | x %in% as.character(c(0:1000))
    invalid <- unique(x[!(is.na(x) | x %in% c(0:1000) | x %in% as.character(c(0:1000)))])
    if (length(invalid) == 0) {
    } else {
        display_values <- if (length(invalid) > 10) {
            paste(c(invalid[1:10], "..."), collapse = ", ")
        } else {
            paste(invalid, collapse = ", ")
        }
        return(paste0(
            "- Some values for the size class ",
            class, " are unexpected: ",
            display_values,
            paste0(" (unexpected values occurred ", length(x[!(is.na(x) | x %in% c(0:1000) | x %in% as.character(c(0:1000)))]), " times)."),
            "<br><br>"
        ))
    }
}
# Define helper functions for conch sheet ---------------------------
validate_site_conch_check <- function(x) {
    valid <- (is.na(x) | grepl("^(GU|CZ|SM|LO)", x) | x == "MISSING")
    return(all(valid))
}
func_validate_site_conch <- function(x) {
    x <- as.character(x)
    valid <- (is.na(x) | grepl("^(GU|CZ|SM|LO)", x) | x == "MISSING")
    if (all(valid)) {
    } else {
        invalid_values <- unique(x[!valid & !is.na(x)])
        error_message <- ""
        if (length(invalid_values) > 0) {
            error_message <- paste0(
                "- Site IDs are expected to begin with the prefix GU, CZ, SM, or LO. These Site IDs are unexpected: ",
                paste(invalid_values, collapse = ", "),
                sprintf(" (unexpected values occurred %d times).", sum(!valid & !is.na(x))),
                "<br><br>"
            )
        }
        return(error_message)
    }
}
func_validate_site_conch_match_check <- function(x, y) {
    valid <- is.na(x) | x %in% y
    return(all(valid))
}
func_validate_site_conch_match <- function(x, y) {
    invalid_sites <- unique(x[!(is.na(x) | x %in% y)])
    if (length(invalid_sites) == 0) {
    } else {
        display_values <- if (length(invalid_sites) > 10) {
            paste(c(invalid_sites[1:10], "..."), collapse = ", ")
        } else {
            paste(invalid_sites, collapse = ", ")
        }
        return(paste0(
            "- Some Site IDs in the Conch sheet do not match the surveyed Sites sheet: ",
            display_values,
            paste0(" (unexpected values occurred ", length(x[!(is.na(x) | x %in% y)]), " times)."),
            "<br><br>"
        ))
    }
}
func_validate_species_conch_check <- function(x) {
    valid <- (x %in% c("Ag", "Sc", "0", "NE"))
    return(all(valid))
}
func_validate_species_conch <- function(x) {
    valid <- (x %in% c("Ag", "Sc", "0", "NE"))
    if (all(valid, na.rm = TRUE)) {
    } else {
        invalid <- unique(x[!valid & !is.na(x)])
        return(paste0(
            "- Conch species should be listed using the codes Ag or Sc, with no conch being indicated by 0 or NE. These values are unexpected: ",
            paste(invalid, collapse = ", "),
            sprintf(" (unexpected values occurred %d times).", sum(!valid & !is.na(x))),
            "<br><br>"
        ))
    }
}
func_validate_shell_thickness_check <- function(x) {
    valid <- (x %in% c(0:300) | x %in% as.character(c(0:300)))
    return(all(valid))
}
func_validate_shell_thickness <- function(x) {
    valid <- (x %in% c(0:300) | x %in% as.character(c(0:300)))
    if (all(valid, na.rm = TRUE)) {
    } else {
        invalid <- unique(x[!valid & !is.na(x)])
        return(paste0(
            "- Conch shell length is expected to be between 0-300mm. If no conch is found at the transect, put a value of 0. These values are unexpected: ",
            paste(invalid, collapse = ", "),
            sprintf(" (unexpected values occurred %d times).", sum(!valid & !is.na(x))),
            "<br><br>"
        ))
    }
}
func_validate_shell_lip_check <- function(x) {
    valid <- (x %in% c(0:30) | x %in% as.character(c(0:30)))
    return(all(valid))
}
func_validate_shell_lip <- function(x) {
    valid <- (x %in% c(0:30) | x %in% as.character(c(0:30)))
    if (all(valid, na.rm = TRUE)) {
    } else {
        invalid <- unique(x[!valid & !is.na(x)])
        return(paste0(
            "- Conch lip thickness is expected to be between 0-30mm. If no conch is found at the transect, put a value of 0. These values are unexpected: ",
            paste(invalid, collapse = ", "),
            sprintf(" (unexpected values occurred %d times).", sum(!valid & !is.na(x))),
            "<br><br>"
        ))
    }
}
func_validate_eggs_check <- function(x) {
    valid <- (is.na(x) | x %in% c("Y", "N", "0"))
    return(all(valid))
}
func_validate_eggs <- function(x) {
    valid <- (is.na(x) | x %in% c("Y", "N", "0"))
    if (all(valid, na.rm = TRUE)) {
    } else {
        invalid <- unique(x[!valid & !is.na(x)])
        return(paste0(
            "- Eggs should either be a Y for yes, or N for no. If no conch is found at the transect, put a value of 0. These values are unexpected: ",
            paste(invalid, collapse = ", "),
            sprintf(" (unexpected values occurred %d times).", sum(!valid & !is.na(x))),
            "<br><br>"
        ))
    }
}

# Define primary functions ---------------------------
func_validate_lampgeneral_1per_sheets_check <- function(x) {
    sheets_check(x, c("Species", "Sites"))
}
func_validate_lampgeneral_1per_completeness_check <- function(x) {
    required_columns_sites <- c("Site ID", "Date", "Mgmt Zone", "Latitude", "Longitude", "Recorder(s)")
    complete_x <- completeness_check(x, required_columns_sites)
    all(c(complete_x))
}
func_validate_lampgeneral_1per_check <- function(df_list, species, sites, finfish, conch) {
    validation_results <- list(
        validate_grouping_check(species$Grouping),
        validate_date_check(sites$Date),
        validate_site_check(sites$`Site ID`),
        validate_coords_check(sites$Latitude, sites$Longitude),
        validate_recorders_check(sites$`Recorder(s)`),
        validate_zone_check(sites$`Mgmt Zone`)
    )
    if ("Finfish" %in% names(df_list)) {
        validation_results <- c(validation_results, list(
            validate_site_finfish_check(finfish$`Site ID`),
            func_validate_site_finfish_match_check(finfish$`Site ID`, species$`Site ID`),
            func_validate_transect_check(finfish$Transect),
            func_validate_species_finfish_match_check(finfish$Species, species$Species),
            func_validate_size_finfish_check(finfish$`0-5 cm`),
            func_validate_size_finfish_check(finfish$`6-10 cm`),
            func_validate_size_finfish_check(finfish$`11-20 cm`),
            func_validate_size_finfish_check(finfish$`21-30 cm`),
            func_validate_size_finfish_check(finfish$`31-40 cm`),
            func_validate_size_finfish_check(finfish$`>40 cm`)
        ))
    }
    if ("Conch" %in% names(df_list)) {
        validation_results <- c(validation_results, list(
            validate_site_conch_check(conch$`Site ID`),
            func_validate_site_conch_match_check(conch$`Site ID`, species$`Site ID`),
            func_validate_transect_check(conch$Transect),
            func_validate_species_conch_check(conch$Species),
            func_validate_shell_thickness_check(conch$`Shell Length (mm)`),
            func_validate_shell_lip_check(conch$`Lip Thickness (mm)`),
            func_validate_eggs_check(conch$Eggs)
        ))
    }
    return(all(unlist(validation_results)))
}
func_validate_lampgeneral_1per_sheets <- function(x) {
    sheets(x, c("Species", "Sites"), "General LAMP")
}
func_validate_lampgeneral_1per_completeness <- function(x) {
    required_columns_sites <- c("Site ID", "Date", "Mgmt Zone", "Latitude", "Longitude", "Recorder(s)")
    complete_x <- completeness(x, required_columns_sites, "General LAMP")
    paste0(complete_x)
}
func_validate_lampgeneral_1per_species <- function(species) {
    grouping_valid <- validate_grouping(species$Grouping)
    return(paste(
        grouping_valid
    ))
}
func_validate_lampgeneral_1per_sites <- function(sites) {
    date_valid <- validate_date(sites$Date)
    site_valid <- validate_site(sites$`Site ID`)
    coords_valid <- validate_coords(sites$Latitude, sites$Longitude)
    recorders_valid <- validate_recorders(sites$`Recorder(s)`)
    zones_valid <- validate_zone(sites$`Mgmt Zone`)
    return(paste(
        date_valid, site_valid, coords_valid, recorders_valid, zones_valid
    ))
}
func_validate_lampgeneral_1per_finfish <- function(finfish, sites, species) {
    site_valid <- func_validate_site_finfish(finfish$`Site ID`)
    site_match_valid <- func_validate_site_finfish_match(finfish$`Site ID`, sites$`Site ID`)
    transect_valid <- func_validate_transect(finfish$Transect)
    species_finfish_valid <- func_validate_species_finfish_match(finfish$Species, species$Species)
    size_1_valid <- func_validate_size_finfish(finfish$`0-5 cm`, "0-5 cm")
    size_2_valid <- func_validate_size_finfish(finfish$`6-10 cm`, "6-10 cm")
    size_3_valid <- func_validate_size_finfish(finfish$`11-20 cm`, "11-20 cm")
    size_4_valid <- func_validate_size_finfish(finfish$`21-30 cm`, "21-30 cm")
    size_5_valid <- func_validate_size_finfish(finfish$`31-40 cm`, "31-40 cm")
    size_6_valid <- func_validate_size_finfish(finfish$`>40 cm`, ">40 cm")
    return(paste(
        site_valid, site_match_valid, transect_valid, species_finfish_valid, size_1_valid, size_2_valid, size_3_valid, size_4_valid, size_5_valid, size_6_valid
    ))
}
func_validate_lampgeneral_1per_conch <- function(conch, sites) {
    site_valid <- func_validate_site_conch(conch$`Site ID`)
    site_match_valid <- func_validate_site_conch_match(conch$`Site ID`, sites$`Site ID`)
    transect_valid <- func_validate_transect(conch$Transect)
    species_valid <- func_validate_species_conch(conch$Species)
    shell_thickness_valid <- func_validate_shell_thickness(conch$`Shell Length (mm)`)
    shell_lip_valid <- func_validate_shell_lip(conch$`Lip Thickness (mm)`)
    eggs_valid <- func_validate_eggs(conch$Eggs)
    return(paste(
        site_valid, site_match_valid, transect_valid, species_valid, shell_thickness_valid, shell_lip_valid, eggs_valid
    ))
}
