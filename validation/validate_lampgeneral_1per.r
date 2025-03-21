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
    valid <- is.na(x) | x %in% y | x == "NE"
    return(all(valid))
}
func_validate_species_finfish_match <- function(x, y) {
    invalid_species <- unique(x[!(is.na(x) | x %in% y | x == "NE")])
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
            paste0(" (unexpected values occurred ", length(x[!(is.na(x) | x %in% y | x == "NE")]), " times). Only Species with a match will be included in the report."),
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
    valid <- (x %in% c("Ag", "Sc", "0", "0.0", "NE"))
    return(all(valid))
}
func_validate_species_conch <- function(x) {
    valid <- (x %in% c("Ag", "Sc", "0", "0.0", "NE"))
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
    valid <- (is.na(x) | x %in% c("Y", "N", "0", "0.0"))
    return(all(valid))
}
func_validate_eggs <- function(x) {
    valid <- (is.na(x) | x %in% c("Y", "N", "0", "0.0"))
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

# Define helper functions for lobster sheet ---------------------------
validate_site_lobster_check <- function(x) {
    valid <- (is.na(x) | grepl("^(GU|CZ|SM|LO)", x) | x == "MISSING")
    return(all(valid))
}
func_validate_site_lobster <- function(x) {
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
func_validate_site_lobster_match_check <- function(x, y) {
    valid <- is.na(x) | x %in% y
    return(all(valid))
}
func_validate_site_lobster_match <- function(x, y) {
    invalid_sites <- unique(x[!(is.na(x) | x %in% y)])
    if (length(invalid_sites) == 0) {
    } else {
        display_values <- if (length(invalid_sites) > 10) {
            paste(c(invalid_sites[1:10], "..."), collapse = ", ")
        } else {
            paste(invalid_sites, collapse = ", ")
        }
        return(paste0(
            "- Some Site IDs in the Lobster sheet do not match the surveyed Sites sheet: ",
            display_values,
            paste0(" (unexpected values occurred ", length(x[!(is.na(x) | x %in% y)]), " times)."),
            "<br><br>"
        ))
    }
}
func_validate_species_lobster_check <- function(x) {
    valid <- (x %in% c("Pa", "Pg", "0", "0.0", "NE"))
    return(all(valid))
}
func_validate_species_lobster <- function(x) {
    valid <- (x %in% c("Pa", "Pg", "0", "0.0", "NE"))
    if (all(valid, na.rm = TRUE)) {
    } else {
        invalid <- unique(x[!valid & !is.na(x)])
        return(paste0(
            "- Lobster species should be listed using the codes Pa or Pg, with no lobster being indicated by 0 or NE. These values are unexpected: ",
            paste(invalid, collapse = ", "),
            sprintf(" (unexpected values occurred %d times).", sum(!valid & !is.na(x))),
            "<br><br>"
        ))
    }
}

# Define helper functions for Diadema/crab sheet ---------------------------
validate_site_diadema_crab_check <- function(x) {
    valid <- (is.na(x) | grepl("^(GU|CZ|SM|LO)", x) | x == "MISSING")
    return(all(valid))
}
func_validate_site_diadema_crab <- function(x) {
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
func_validate_site_diadema_crab_match_check <- function(x, y) {
    valid <- is.na(x) | x %in% y
    return(all(valid))
}
func_validate_site_diadema_crab_match <- function(x, y) {
    invalid_sites <- unique(x[!(is.na(x) | x %in% y)])
    if (length(invalid_sites) == 0) {
    } else {
        display_values <- if (length(invalid_sites) > 10) {
            paste(c(invalid_sites[1:10], "..."), collapse = ", ")
        } else {
            paste(invalid_sites, collapse = ", ")
        }
        return(paste0(
            "- Some Site IDs in the Diadema and Crab sheet do not match the surveyed Sites sheet: ",
            display_values,
            paste0(" (unexpected values occurred ", length(x[!(is.na(x) | x %in% y)]), " times)."),
            "<br><br>"
        ))
    }
}
func_validate_diadema_crab_counts_check <- function(x) {
    valid <- (x %in% c(0:200) | x %in% as.character(c(0:200)) | x == "NE" | x == "N/E")
    return(all(valid))
}
func_validate_diadema_crab_counts <- function(x, type) {
    valid <- (x %in% c(0:200) | x %in% as.character(c(0:200)) | x == "NE" | x == "N/E")
    if (all(valid, na.rm = TRUE)) {
    } else {
        invalid <- unique(x[!valid & !is.na(x)])
        return(paste0(
            "- These ", type, " values are unexpected: ",
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
func_validate_lampgeneral_1per_completeness_check <- function(df_list) {
    required_columns_sites <- c("Site ID", "Date", "Mgmt Zone", "Latitude", "Longitude", "Recorder(s)")
    required_columns_species <- c("Grouping", "Species", "Family", "Scientific Name")
    required_columns_biomass <- c("Family", "Scientific Name", "LWRconv", "LWRa", "LWRb")
    required_columns_finfish <- c("Site ID", "Transect", "Species", "0-5 cm", "6-10 cm", "11-20 cm", "21-30 cm", "31-40 cm", ">40 cm")
    required_columns_conch <- c("Site ID", "Transect", "Species", "Shell Length (mm)", "Lip Thickness (mm)", "Eggs")
    required_columns_lobster <- c("Site ID", "Transect", "Species", "Carapace Length (mm)", "Sex", "Eggs")
    required_columns_diadema_crab <- c("Site ID", "Transect", "Adult Diadema antillarum", "Juvenile Diadema antillarum", "Mithrax spinosissumus")
    complete_sites <- completeness_check(df_list$Sites, required_columns_sites)
    complete_species <- completeness_check(df_list$Species, required_columns_species)
    complete_biomass <- if ("Biomass" %in% names(df_list)) completeness_check(df_list$Biomass, required_columns_biomass) else TRUE
    complete_finfish <- if ("Finfish" %in% names(df_list)) completeness_check(df_list$Finfish, required_columns_finfish) else TRUE
    complete_conch <- if ("Conch" %in% names(df_list)) completeness_check(df_list$Conch, required_columns_conch) else TRUE
    complete_lobster <- if ("Lobster" %in% names(df_list)) completeness_check(df_list$Lobster, required_columns_lobster) else TRUE
    complete_diadema_crab <- if ("Diadema_Crab" %in% names(df_list)) completeness_check(df_list$Diadema_Crab, required_columns_diadema_crab) else TRUE
    all(c(complete_sites, complete_species, complete_biomass, complete_finfish, complete_conch, complete_lobster, complete_diadema_crab))
}
func_validate_lampgeneral_1per_check <- function(df_list) {
    validation_results <- list(
        validate_grouping_check(df_list$Species$Grouping),
        validate_date_check(df_list$Sites$Date),
        validate_site_check(df_list$Sites$`Site ID`),
        validate_coords_check(df_list$Sites$Latitude, df_list$Sites$Longitude),
        validate_recorders_check(df_list$Sites$`Recorder(s)`),
        validate_zone_check(df_list$Sites$`Mgmt Zone`)
    )
    if ("Finfish" %in% names(df_list)) {
        validation_results <- c(validation_results, list(
            validate_site_finfish_check(df_list$Finfish$`Site ID`),
            func_validate_site_finfish_match_check(df_list$Finfish$`Site ID`, df_list$Species$`Site ID`),
            func_validate_transect_check(df_list$Finfish$Transect),
            func_validate_species_finfish_match_check(df_list$Finfish$Species, df_list$Species$Species),
            func_validate_size_finfish_check(df_list$Finfish$`0-5 cm`),
            func_validate_size_finfish_check(df_list$Finfish$`6-10 cm`),
            func_validate_size_finfish_check(df_list$Finfish$`11-20 cm`),
            func_validate_size_finfish_check(df_list$Finfish$`21-30 cm`),
            func_validate_size_finfish_check(df_list$Finfish$`31-40 cm`),
            func_validate_size_finfish_check(df_list$Finfish$`>40 cm`)
        ))
    }
    if ("Conch" %in% names(df_list)) {
        validation_results <- c(validation_results, list(
            validate_site_conch_check(df_list$Conch$`Site ID`),
            func_validate_site_conch_match_check(df_list$Conch$`Site ID`, df_list$Species$`Site ID`),
            func_validate_transect_check(df_list$Conch$Transect),
            func_validate_species_conch_check(df_list$Conch$Species),
            func_validate_shell_thickness_check(df_list$Conch$`Shell Length (mm)`),
            func_validate_shell_lip_check(df_list$Conch$`Lip Thickness (mm)`),
            func_validate_eggs_check(df_list$Conch$Eggs)
        ))
    }
    if ("Lobster" %in% names(df_list)) {
        validation_results <- c(validation_results, list(
            validate_site_lobster_check(df_list$Lobster$`Site ID`),
            func_validate_site_lobster_match_check(df_list$Lobster$`Site ID`, df_list$Species$`Site ID`),
            func_validate_transect_check(df_list$Lobster$Transect),
            func_validate_species_lobster_check(df_list$Lobster$Species)
        ))
    }
    if ("Diadema_Crab" %in% names(df_list)) {
        validation_results <- c(validation_results, list(
            validate_site_diadema_crab_check(df_list$Diadema_Crab$`Site ID`),
            func_validate_site_diadema_crab_match_check(df_list$Diadema_Crab$`Site ID`, df_list$Species$`Site ID`),
            func_validate_transect_check(df_list$Diadema_Crab$Transect),
            func_validate_diadema_crab_counts_check(df_list$Diadema_Crab$`Adult Diadema antillarum`),
            func_validate_diadema_crab_counts_check(df_list$Diadema_Crab$`Juvenile Diadema antillarum`),
            func_validate_diadema_crab_counts_check(df_list$Diadema_Crab$`Mithrax spinosissumus`)
        ))
    }
    return(all(unlist(validation_results)))
}
func_validate_lampgeneral_1per_sheets <- function(x) {
    sheets(x, c("Species", "Sites"), "General LAMP")
}
func_validate_lampgeneral_1per_completeness <- function(df_list) {
    required_columns_sites <- c("Site ID", "Date", "Mgmt Zone", "Latitude", "Longitude", "Recorder(s)")
    required_columns_species <- c("Grouping", "Species", "Family", "Scientific Name")
    required_columns_biomass <- c("Family", "Scientific Name", "LWRconv", "LWRa", "LWRb")
    required_columns_finfish <- c("Site ID", "Transect", "Species", "0-5 cm", "6-10 cm", "11-20 cm", "21-30 cm", "31-40 cm", ">40 cm")
    required_columns_conch <- c("Site ID", "Transect", "Species", "Shell Length (mm)", "Lip Thickness (mm)", "Eggs")
    required_columns_lobster <- c("Site ID", "Transect", "Species", "Carapace Length (mm)", "Sex", "Eggs")
    required_columns_diadema_crab <- c("Site ID", "Transect", "Adult Diadema antillarum", "Juvenile Diadema antillarum", "Mithrax spinosissumus")
    complete_sites <- completeness(df_list$Sites, required_columns_sites, "Sites")
    complete_species <- completeness(df_list$Species, required_columns_species, "Species")
    complete_biomass <- if ("Biomass" %in% names(df_list)) completeness(df_list$Biomass, required_columns_biomass, "Biomass") else ""
    complete_finfish <- if ("Finfish" %in% names(df_list)) completeness(df_list$Finfish, required_columns_finfish, "Finfish") else ""
    complete_conch <- if ("Conch" %in% names(df_list)) completeness(df_list$Conch, required_columns_conch, "Conch") else ""
    complete_lobster <- if ("Lobster" %in% names(df_list)) completeness(df_list$Lobster, required_columns_lobster, "Lobster") else ""
    complete_diadema_crab <- if ("Diadema_Crab" %in% names(df_list)) completeness(df_list$Diadema_Crab, required_columns_diadema_crab, "Diadema and Crab") else ""
    paste0(complete_sites, complete_species, complete_biomass, complete_finfish, complete_conch, complete_lobster, complete_diadema_crab)
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
func_validate_lampgeneral_1per_lobster <- function(lobster, sites) {
    site_valid <- func_validate_site_lobster(lobster$`Site ID`)
    site_match_valid <- func_validate_site_lobster_match(lobster$`Site ID`, sites$`Site ID`)
    transect_valid <- func_validate_transect(lobster$Transect)
    species_valid <- func_validate_species_lobster(lobster$Species)
    return(paste(
        site_valid, site_match_valid, transect_valid, species_valid
    ))
}
func_validate_lampgeneral_1per_diadema_crab <- function(diadema_crab, sites) {
    site_valid <- func_validate_site_diadema_crab(diadema_crab$`Site ID`)
    site_match_valid <- func_validate_site_diadema_crab_match(diadema_crab$`Site ID`, sites$`Site ID`)
    transect_valid <- func_validate_transect(diadema_crab$Transect)
    ad_diadema_valid <- func_validate_diadema_crab_counts(diadema_crab$`Adult Diadema antillarum`, "Adult Diadema antillarum")
    juv_diadema_valid <- func_validate_diadema_crab_counts(diadema_crab$`Juvenile Diadema antillarum`, "Juvenile Diadema antillarum")
    mithrax_valid <- func_validate_diadema_crab_counts(diadema_crab$`Mithrax spinosissumus`, "Mithrax spinosissumus")
    return(paste(
        site_valid, site_match_valid, transect_valid, ad_diadema_valid, juv_diadema_valid, mithrax_valid
    ))
}
