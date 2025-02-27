## validate_lampgeneral_1per.r

# Source general helper functions ---------------------------
source("validation/general_validation_helpers.r")

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


# Define primary functions ---------------------------
func_validate_lampgeneral_1per_sheets_check <- function(x) {
    sheets_check(x, c("Species", "Sites"))
}
func_validate_lampgeneral_1per_completeness_check <- function(x) {
    required_columns_sites <- c("Site ID", "Date", "Mgmt Zone", "Latitude", "Longitude", "Recorder(s)")
    complete_x <- completeness_check(x, required_columns_sites)
    all(c(complete_x))
}
func_validate_lampgeneral_1per_check <- function(x) {
    date_valid <- validate_date_check(x$Date)
    site_valid <- validate_site_check(x$`Site ID`)
    coords_valid <- validate_coords_check(x$Latitude, x$Longitude)
    recorders_valid <- validate_recorders_check(x$`Recorder(s)`)
    zones_valid <- validate_zone_check(x$`Mgmt Zone`)
    return(all(c(
        date_valid, site_valid, coords_valid, recorders_valid, zones_valid
    )))
}
func_validate_lampgeneral_1per_sheets <- function(x) {
    sheets(x, c("Species", "Sites"), "General LAMP")
}
func_validate_lampgeneral_1per_completeness <- function(x) {
    required_columns_sites <- c("Site ID", "Date", "Mgmt Zone", "Latitude", "Longitude", "Recorder(s)")
    complete_x <- completeness(x, required_columns_sites, "General LAMP")
    paste0(complete_x)
}
func_validate_lampgeneral_1per_sites <- function(x) {
    date_valid <- validate_date(x$Date)
    site_valid <- validate_site(x$`Site ID`)
    coords_valid <- validate_coords(x$Latitude, x$Longitude)
    recorders_valid <- validate_recorders(x$`Recorder(s)`)
    zones_valid <- validate_zone(x$`Mgmt Zone`)
    return(paste(
        date_valid, site_valid, coords_valid, recorders_valid, zones_valid
    ))
}
