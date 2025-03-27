## server.R

# Attach packages ---------------------------
library(readxl)

# Source code ---------------------------
source("server_helpers.r")

# Define server ---------------------------
server <- function(input, output, session) {
    shinyjs::hide("feedback-content-box")

    # Store/update flags for defining dataframes
    clear_upload_fisheries_multiyr3 <- reactiveVal(FALSE)
    observeEvent(input$upload_fisheries_multiyr3, {
        if (!is.null(input$upload_fisheries_multiyr3)) {
            clear_upload_fisheries_multiyr3(FALSE)
        }
    })
    clear_upload_fisheries_multiyr4 <- reactiveVal(FALSE)
    observeEvent(input$upload_fisheries_multiyr4, {
        if (!is.null(input$upload_fisheries_multiyr4)) {
            clear_upload_fisheries_multiyr4(FALSE)
        }
    })
    clear_upload_fisher_multiyr3 <- reactiveVal(FALSE)
    observeEvent(input$upload_fisher_multiyr3, {
        if (!is.null(input$upload_fisher_multiyr3)) {
            clear_upload_fisher_multiyr3(FALSE)
        }
    })
    clear_upload_fisher_multiyr4 <- reactiveVal(FALSE)
    observeEvent(input$upload_fisher_multiyr4, {
        if (!is.null(input$upload_fisher_multiyr4)) {
            clear_upload_fisher_multiyr4(FALSE)
        }
    })
    clear_upload_lamp_multiper3 <- reactiveVal(FALSE)
    observeEvent(input$upload_lamp_multiper3, {
        if (!is.null(input$upload_lamp_multiper3)) {
            clear_upload_lamp_multiper3(FALSE)
        }
    })
    clear_upload_lamp_multiper4 <- reactiveVal(FALSE)
    observeEvent(input$upload_lamp_multiper4, {
        if (!is.null(input$upload_lamp_multiper4)) {
            clear_upload_lamp_multiper4(FALSE)
        }
    })
    clear_upload_spag_multiper3 <- reactiveVal(FALSE)
    observeEvent(input$upload_spag_multiper3, {
        if (!is.null(input$upload_spag_multiper3)) {
            clear_upload_spag_multiper3(FALSE)
        }
    })
    clear_upload_spag_multiper4 <- reactiveVal(FALSE)
    observeEvent(input$upload_spag_multiper4, {
        if (!is.null(input$upload_spag_multiper4)) {
            clear_upload_spag_multiper4(FALSE)
        }
    })

    # Define dataframes from uploads
    nas <- c("NA", "N/A", "Unknown", "Missing", "None", "")
    df_upload_fisheries_1yr <- reactive({
        req(input$upload_fisheries_1yr)
        read_excel(input$upload_fisheries_1yr$datapath, sheet = 1, na = nas)
    })
    df_upload_fisheries_multiyr1 <- reactive({
        req(input$upload_fisheries_multiyr1)
        read_excel(input$upload_fisheries_multiyr1$datapath, sheet = 1, na = nas)
    })
    df_upload_fisheries_multiyr2 <- reactive({
        req(input$upload_fisheries_multiyr2)
        read_excel(input$upload_fisheries_multiyr2$datapath, sheet = 1, na = nas)
    })
    df_upload_fisheries_multiyr3 <- reactive({
        if (clear_upload_fisheries_multiyr3()) {
            return(NULL)
        }
        req(input$upload_fisheries_multiyr3)
        read_excel(input$upload_fisheries_multiyr3$datapath, sheet = 1, na = nas)
    })
    df_upload_fisheries_multiyr4 <- reactive({
        if (clear_upload_fisheries_multiyr4()) {
            return(NULL)
        }
        req(input$upload_fisheries_multiyr4)
        read_excel(input$upload_fisheries_multiyr4$datapath, sheet = 1, na = nas)
    })
    df_upload_fisher_1yr <- reactive({
        req(input$upload_fisher_1yr)
        read_excel(input$upload_fisher_1yr$datapath, sheet = 1, na = nas)
    })
    df_upload_fisher_multiyr1 <- reactive({
        req(input$upload_fisher_multiyr1)
        read_excel(input$upload_fisher_multiyr1$datapath, sheet = 1, na = nas)
    })
    df_upload_fisher_multiyr2 <- reactive({
        req(input$upload_fisher_multiyr2)
        read_excel(input$upload_fisher_multiyr2$datapath, sheet = 1, na = nas)
    })
    df_upload_fisher_multiyr3 <- reactive({
        if (clear_upload_fisher_multiyr3()) {
            return(NULL)
        }
        req(input$upload_fisher_multiyr3)
        read_excel(input$upload_fisher_multiyr3$datapath, sheet = 1, na = nas)
    })
    df_upload_fisher_multiyr4 <- reactive({
        if (clear_upload_fisher_multiyr4()) {
            return(NULL)
        }
        req(input$upload_fisher_multiyr4)
        read_excel(input$upload_fisher_multiyr4$datapath, sheet = 1, na = nas)
    })
    df_upload_lamp_1per <- reactive({
        req(input$upload_lamp_1per)
        file_path <- input$upload_lamp_1per$datapath
        datatype <- input$datatype_lamp_1per
        read_lamp_data(file_path, datatype)
    })
    df_upload_lamp_multiper1 <- reactive({
        req(input$upload_lamp_multiper1)
        file_path <- input$upload_lamp_multiper1$datapath
        datatype <- input$datatype_lamp_multiper
        read_lamp_data(file_path, datatype)
    })
    df_upload_lamp_multiper2 <- reactive({
        req(input$upload_lamp_multiper2)
        file_path <- input$upload_lamp_multiper2$datapath
        datatype <- input$datatype_lamp_multiper
        read_lamp_data(file_path, datatype)
    })
    df_upload_lamp_multiper3 <- reactive({
        if (clear_upload_lamp_multiper3()) {
            return(NULL)
        }
        req(input$upload_lamp_multiper3)
        file_path <- input$upload_lamp_multiper3$datapath
        datatype <- input$datatype_lamp_multiper
        read_lamp_data(file_path, datatype)
    })
    df_upload_lamp_multiper4 <- reactive({
        if (clear_upload_lamp_multiper4()) {
            return(NULL)
        }
        req(input$upload_lamp_multiper4)
        file_path <- input$upload_lamp_multiper4$datapath
        datatype <- input$datatype_lamp_multiper
        read_lamp_data(file_path, datatype)
    })
    df_upload_spag_1per <- reactive({
        req(input$upload_spag_1per)
        read_excel(input$upload_spag_1per$datapath, sheet = 1, na = nas)
    })
    df_upload_spag_multiper1 <- reactive({
        req(input$upload_spag_multiper1)
        read_excel(input$upload_spag_multiper1$datapath, sheet = 1, na = nas)
    })
    df_upload_spag_multiper2 <- reactive({
        req(input$upload_spag_multiper2)
        read_excel(input$upload_spag_multiper2$datapath, sheet = 1, na = nas)
    })
    df_upload_spag_multiper3 <- reactive({
        if (clear_upload_spag_multiper3()) {
            return(NULL)
        }
        req(input$upload_spag_multiper3)
        read_excel(input$upload_spag_multiper3$datapath, sheet = 1, na = nas)
    })
    df_upload_spag_multiper4 <- reactive({
        if (clear_upload_spag_multiper4()) {
            return(NULL)
        }
        req(input$upload_spag_multiper4)
        read_excel(input$upload_spag_multiper4$datapath, sheet = 1, na = nas)
    })

    # Read and report year of datafile
    output$ui_upload_fisheries_1yr <- renderUI({
        check_datafile_dates(df_upload_fisheries_1yr())
    })
    output$ui_upload_fisheries_multiyr1 <- renderUI({
        check_datafile_dates(df_upload_fisheries_multiyr1())
    })
    output$ui_upload_fisheries_multiyr2 <- renderUI({
        check_datafile_dates(df_upload_fisheries_multiyr2())
    })
    output$ui_upload_fisheries_multiyr3 <- renderUI({
        check_datafile_dates(df_upload_fisheries_multiyr3())
    })
    output$ui_upload_fisheries_multiyr4 <- renderUI({
        check_datafile_dates(df_upload_fisheries_multiyr4())
    })
    output$ui_upload_fisher_1yr <- renderUI({
        check_datafile_dates(df_upload_fisher_1yr())
    })
    output$ui_upload_fisher_multiyr1 <- renderUI({
        check_datafile_dates(df_upload_fisher_multiyr1())
    })
    output$ui_upload_fisher_multiyr2 <- renderUI({
        check_datafile_dates(df_upload_fisher_multiyr2())
    })
    output$ui_upload_fisher_multiyr3 <- renderUI({
        check_datafile_dates(df_upload_fisher_multiyr3())
    })
    output$ui_upload_fisher_multiyr4 <- renderUI({
        check_datafile_dates(df_upload_fisher_multiyr4())
    })
    output$ui_upload_lamp_1per <- renderUI({
        check_datafiles_dates(df_upload_lamp_1per())
    })
    output$ui_upload_lamp_multiper1 <- renderUI({
        check_datafiles_dates(df_upload_lamp_multiper1())
    })
    output$ui_upload_lamp_multiper2 <- renderUI({
        check_datafiles_dates(df_upload_lamp_multiper2())
    })
    output$ui_upload_lamp_multiper3 <- renderUI({
        check_datafiles_dates(df_upload_lamp_multiper3())
    })
    output$ui_upload_lamp_multiper4 <- renderUI({
        check_datafiles_dates(df_upload_lamp_multiper4())
    })
    output$ui_upload_spag_1per <- renderUI({
        check_datafile_dates(df_upload_spag_1per())
    })
    output$ui_upload_spag_multiper1 <- renderUI({
        check_datafile_dates(df_upload_spag_multiper1())
    })
    output$ui_upload_spag_multiper2 <- renderUI({
        check_datafile_dates(df_upload_spag_multiper2())
    })
    output$ui_upload_spag_multiper3 <- renderUI({
        check_datafile_dates(df_upload_spag_multiper3())
    })
    output$ui_upload_spag_multiper4 <- renderUI({
        check_datafile_dates(df_upload_spag_multiper4())
    })

    # Change image based on datatype
    observeEvent(input$datatype_lamp_1per, {
        if (input$datatype_lamp_1per == "Conch") {
            is_conch <- TRUE
        } else {
            is_conch <- FALSE
        }
        disableCustomization("lamp_1per")
        session$sendCustomMessage("triggerChangeLampImg", list(isConch = is_conch, isMulti = FALSE))
    })
    observeEvent(input$datatype_lamp_multiper, {
        if (input$datatype_lamp_multiper == "Conch") {
            is_conch <- TRUE
        } else {
            is_conch <- FALSE
        }
        disableCustomization("lamp_multiper")
        session$sendCustomMessage("triggerChangeLampImg", list(isConch = is_conch, isMulti = TRUE))
    })
    observeEvent(input$datatype_spag_1per, {
        if (input$datatype_spag_1per == "Laser") {
            is_visual <- FALSE
        } else {
            is_visual <- TRUE
        }
        disableCustomization("spag_1per")
        session$sendCustomMessage("triggerChangeSpagImg", list(isVisual = is_visual, isMulti = FALSE))
    })
    observeEvent(input$datatype_spag_multiper, {
        if (input$datatype_spag_multiper == "Laser") {
            is_visual <- FALSE
        } else {
            is_visual <- TRUE
        }
        disableCustomization("spag_multiper")
        session$sendCustomMessage("triggerChangeSpagImg", list(isVisual = is_visual, isMulti = TRUE))
    })

    # Validate dataframes
    observeEvent(input$validate_fisheries_1yr, {
        shinyalert("Notice!", "Validation has not been implemented for Fisheries Single Year as of yet!",
            confirmButtonText = "I Understand", confirmButtonCol = "#cde9f0", type = "info", size = "s"
        )
        enableCustomization("fisheries_1yr")
    })
    observeEvent(input$validate_fisheries_multiyr, {
        shinyalert("Notice!", "Validation has not been implemented for Fisheries Multi-Year as of yet!",
            confirmButtonText = "I Understand", confirmButtonCol = "#cde9f0", type = "info", size = "s"
        )
        enableCustomization("fisheries_multiyr")
    })
    observeEvent(input$validate_fisher_1yr, {
        shinyalert("Notice!", "Validation has not been implemented for Fisher Project Single Year as of yet!",
            confirmButtonText = "I Understand", confirmButtonCol = "#cde9f0", type = "info", size = "s"
        )
        enableCustomization("fisher_1yr")
    })
    observeEvent(input$validate_fisher_multiyr, {
        shinyalert("Notice!", "Validation has not been implemented for Fisher Project Multi-Year as of yet!",
            confirmButtonText = "I Understand", confirmButtonCol = "#cde9f0", type = "info", size = "s"
        )
        enableCustomization("fisher_multiyr")
    })
    source("validation/validation_observers/validate_lamp_observer_1per.r", local = TRUE)
    source("validation/validation_observers/validate_lamp_observer_multiper.r", local = TRUE)
    observeEvent(input$validate_spag_1per, {
        shinyalert("Notice!", "Validation has not been implemented for SPAG Single Year as of yet!",
            confirmButtonText = "I Understand", confirmButtonCol = "#cde9f0", type = "info", size = "s"
        )
        enableCustomization("spag_1per")
    })
    observeEvent(input$validate_spag_multiper, {
        shinyalert("Notice!", "Validation has not been implemented for SPAG Multi-Year as of yet!",
            confirmButtonText = "I Understand", confirmButtonCol = "#cde9f0", type = "info", size = "s"
        )
        enableCustomization("spag_multiper")
    })

    # Create reports
    output$report_fisheries_1yr <- downloadHandler(
        filename = function() "report_fisheries_1yr.docx",
        content = function(file) {
            report_file <- "report_fisheries_1yr.Rmd"
            src <- normalizePath(c(
                paste0("reports/", report_file),
                "reports/report_template.docx",
                "www/images/TASA_logo_full_color.png"
            ))
            owd <- setwd(tempdir())
            on.exit(setwd(owd))
            file.copy(src, c(report_file, "report_template.docx", "TASA_logo_full_color.png"), overwrite = TRUE)
            out <- render(
                report_file,
                params = list(user_name = input$fisheries_1yr_name, datafile = df_upload_fisheries_1yr()),
                envir = new.env(parent = globalenv())
            )
            file.rename(out, file)
        }
    )
    output$report_fisheries_multiyr <- downloadHandler(
        filename = function() "report_fisheries_multiyr.docx",
        content = function(file) {
            report_file <- "report_fisheries_multiyr.Rmd"
            src <- normalizePath(c(
                paste0("reports/", report_file),
                "reports/report_template.docx",
                "www/images/TASA_logo_full_color.png"
            ))
            owd <- setwd(tempdir())
            on.exit(setwd(owd))
            file.copy(src, c(report_file, "report_template.docx", "TASA_logo_full_color.png"), overwrite = TRUE)
            out <- render(
                report_file,
                params = list(user_name = input$fisheries_multiyr_name, datafile = df_upload_fisheries_multiyr1()),
                envir = new.env(parent = globalenv())
            )
            file.rename(out, file)
        }
    )
    output$report_fisher_1yr <- downloadHandler(
        filename = function() "report_fisher_1yr.docx",
        content = function(file) {
            report_file <- "report_fisher_1yr.Rmd"
            src <- normalizePath(c(
                paste0("reports/", report_file),
                "reports/report_template.docx",
                "www/images/TASA_logo_full_color.png"
            ))
            owd <- setwd(tempdir())
            on.exit(setwd(owd))
            file.copy(src, c(report_file, "report_template.docx", "TASA_logo_full_color.png"), overwrite = TRUE)
            out <- render(
                report_file,
                params = list(user_name = input$fisher_1yr_name, datafile = df_upload_fisher_1yr()),
                envir = new.env(parent = globalenv())
            )
            file.rename(out, file)
        }
    )
    output$report_fisher_multiyr <- downloadHandler(
        filename = function() "report_fisher_multiyr.docx",
        content = function(file) {
            report_file <- "report_fisher_multiyr.Rmd"
            src <- normalizePath(c(
                paste0("reports/", report_file),
                "reports/report_template.docx",
                "www/images/TASA_logo_full_color.png"
            ))
            owd <- setwd(tempdir())
            on.exit(setwd(owd))
            file.copy(src, c(report_file, "report_template.docx", "TASA_logo_full_color.png"), overwrite = TRUE)
            out <- render(
                report_file,
                params = list(user_name = input$fisher_multiyr_name, datafile = df_upload_fisher_multiyr1()),
                envir = new.env(parent = globalenv())
            )
            file.rename(out, file)
        }
    )
    output$report_lamp_1per <- downloadHandler(
        filename = function() {
            report_file <- switch(input$datatype_lamp_1per,
                "Conch" = "report_lampconch_1per.Rmd",
                "General LAMP" = "report_lampgen_1per.Rmd",
            )
            gsub(".Rmd", ".docx", report_file)
        },
        content = function(file) {
            report_file <- switch(input$datatype_lamp_1per,
                "Conch" = "report_lampconch_1per.Rmd",
                "General LAMP" = "report_lampgen_1per.Rmd",
            )
            shapefiles <- list.files("shapefiles", full.names = TRUE)
            normalized_shapefiles <- normalizePath(shapefiles)
            src <- normalizePath(c(
                paste0("reports/", report_file),
                "reports/report_template.docx",
                "www/images/TAMR_map.jpg",
                "theme.r", "map.r",
                normalized_shapefiles
            ))
            owd <- setwd(tempdir())
            on.exit(setwd(owd))
            file.copy(src, c(report_file, "report_template.docx", "TAMR_map.jpg", "theme.r", "map.r", basename(shapefiles)), overwrite = TRUE)
            out <- render(
                report_file,
                params = list(user_name = input$lamp_1per_name, datafile_name = input$upload_lamp_1per$name, datafile = df_upload_lamp_1per()),
                envir = new.env(parent = globalenv())
            )
            file.rename(out, file)
        }
    )
    output$report_lamp_multiper <- downloadHandler(
        filename = function() {
            report_file <- switch(input$datatype_lamp_multiper,
                "Conch" = "report_lampconch_multiper.Rmd",
                "General LAMP" = "report_lampgen_multiper.Rmd",
            )
            gsub(".Rmd", ".docx", report_file)
        },
        content = function(file) {
            report_file <- switch(input$datatype_lamp_multiper,
                "Conch" = "report_lampconch_multiper.Rmd",
                "General LAMP" = "report_lampgen_multiper.Rmd",
            )
            shapefiles <- list.files("shapefiles", full.names = TRUE)
            normalized_shapefiles <- normalizePath(shapefiles)
            src <- normalizePath(c(
                paste0("reports/", report_file),
                "reports/report_template.docx",
                "www/images/TAMR_map.jpg",
                "theme.r", "map.r",
                normalized_shapefiles
            ))
            owd <- setwd(tempdir())
            on.exit(setwd(owd))
            file.copy(src, c(report_file, "report_template.docx", "TAMR_map.jpg", "theme.r", "map.r", basename(shapefiles)), overwrite = TRUE)
            params_list <- list(
                user_name = input$lamp_multiper_name,
                datafile1_name = input$upload_lamp_multiper1$name,
                datafile1 = df_upload_lamp_multiper1(),
                datafile2_name = input$upload_lamp_multiper2$name,
                datafile2 = df_upload_lamp_multiper2()
            )
            if (!is.null(input$upload_lamp_multiper3)) {
                params_list$datafile3_name <- input$upload_lamp_multiper3$name
                params_list$datafile3 <- df_upload_lamp_multiper3()
            }
            if (!is.null(input$upload_lamp_multiper4)) {
                params_list$datafile4_name <- input$upload_lamp_multiper4$name
                params_list$datafile4 <- df_upload_lamp_multiper4()
            }
            out <- render(
                report_file,
                params = params_list,
                envir = new.env(parent = globalenv())
            )
            file.rename(out, file)
        }
    )
    output$report_spag_1per <- downloadHandler(
        filename = function() {
            report_file <- switch(input$datatype_spag_1per,
                "Visual Census" = "report_spagvis_1per.Rmd",
                "Laser" = "report_spaglaser_1per.Rmd"
            )
            gsub(".Rmd", ".docx", report_file)
        },
        content = function(file) {
            report_file <- switch(input$datatype_spag_1per,
                "Visual Census" = "report_spagvis_1per.Rmd",
                "Laser" = "report_spaglaser_1per.Rmd"
            )
            src <- normalizePath(c(
                paste0("reports/", report_file),
                "reports/report_template.docx",
                "www/images/TASA_logo_full_color.png"
            ))
            owd <- setwd(tempdir())
            on.exit(setwd(owd))
            file.copy(src, c(report_file, "report_template.docx", "TASA_logo_full_color.png"), overwrite = TRUE)
            out <- render(
                report_file,
                params = list(user_name = input$spag_name, datafile = df_upload_spag_1per()),
                envir = new.env(parent = globalenv())
            )
            file.rename(out, file)
        }
    )
    output$report_spag_multiper <- downloadHandler(
        filename = function() {
            report_file <- switch(input$datatype_spag_multiper,
                "Visual Census" = "report_spagvis_multiper.Rmd",
                "Laser" = "report_spaglaser_multiper.Rmd"
            )
            gsub(".Rmd", ".docx", report_file)
        },
        content = function(file) {
            report_file <- switch(input$datatype_spag_multiper,
                "Visual Census" = "report_spagvis_multiper.Rmd",
                "Laser" = "report_spaglaser_multiper.Rmd"
            )
            src <- normalizePath(c(
                paste0("reports/", report_file),
                "reports/report_template.docx",
                "www/images/TASA_logo_full_color.png"
            ))
            owd <- setwd(tempdir())
            on.exit(setwd(owd))
            file.copy(src, c(report_file, "report_template.docx", "TASA_logo_full_color.png"), overwrite = TRUE)
            out <- render(
                report_file,
                params = list(user_name = input$spag_name, datafile = df_upload_spag_multiper1()),
                envir = new.env(parent = globalenv())
            )
            file.rename(out, file)
        }
    )

    # Observe Upload
    observeEvent(input$upload_fisheries_1yr, {
        if (!is.null(input$upload_fisheries_1yr)) {
            enableValidate("fisheries_1yr")
            disableCustomization("fisheries_1yr")
        }
    })
    observeEvent(input$upload_fisher_1yr, {
        if (!is.null(input$upload_fisher_1yr)) {
            enableValidate("fisher_1yr")
            disableCustomization("fisher_1yr")
        }
    })
    observeEvent(input$upload_lamp_1per, {
        if (!is.null(input$upload_lamp_1per)) {
            enableValidate("lamp_1per")
            disableCustomization("lamp_1per")
        }
    })
    observeEvent(input$upload_spag_1per, {
        if (!is.null(input$upload_spag_1per)) {
            enableValidate("spag_1per")
            disableCustomization("spag_1per")
        }
    })

    # Observe Multi Upload
    observeEvent(input$upload_fisheries_multiyr1, {
        enableUpload("fisheries_multiyr2")
        disableCustomization("fisheries_multiyr")
    })
    observeEvent(input$upload_fisheries_multiyr2, {
        enableUpload("fisheries_multiyr3")
        enableValidate("fisheries_multiyr")
        disableCustomization("fisheries_multiyr")
    })
    observeEvent(input$upload_fisheries_multiyr3, {
        enableUpload("fisheries_multiyr4")
        enableUploadRemoveBttn("fisheries_multiyr3")
        disableCustomization("fisheries_multiyr")
    })
    observeEvent(input$upload_fisheries_multiyr4, {
        disableUploadRemoveBttn("fisheries_multiyr3")
        enableUploadRemoveBttn("fisheries_multiyr4")
        disableCustomization("fisheries_multiyr")
    })
    observeEvent(input$upload_fisher_multiyr1, {
        enableUpload("fisher_multiyr2")
        disableCustomization("fisher_multiyr")
    })
    observeEvent(input$upload_fisher_multiyr2, {
        enableUpload("fisher_multiyr3")
        enableValidate("fisher_multiyr")
        disableCustomization("fisher_multiyr")
    })
    observeEvent(input$upload_fisher_multiyr3, {
        enableUpload("fisher_multiyr4")
        enableUploadRemoveBttn("fisher_multiyr3")
        disableCustomization("fisher_multiyr")
    })
    observeEvent(input$upload_fisher_multiyr4, {
        disableUploadRemoveBttn("fisher_multiyr3")
        enableUploadRemoveBttn("fisher_multiyr4")
        disableCustomization("fisher_multiyr")
    })
    observeEvent(input$upload_lamp_multiper1, {
        enableUpload("lamp_multiper2")
        disableCustomization("lamp_multiper")
    })
    observeEvent(input$upload_lamp_multiper2, {
        enableUpload("lamp_multiper3")
        enableValidate("lamp_multiper")
        disableCustomization("lamp_multiper")
    })
    observeEvent(input$upload_lamp_multiper3, {
        enableUpload("lamp_multiper4")
        enableUploadRemoveBttn("lamp_multiper3")
        disableCustomization("lamp_multiper")
    })
    observeEvent(input$upload_lamp_multiper4, {
        disableUploadRemoveBttn("lamp_multiper3")
        enableUploadRemoveBttn("lamp_multiper4")
        disableCustomization("lamp_multiper")
    })
    observeEvent(input$upload_spag_multiper1, {
        enableUpload("spag_multiper2")
        disableCustomization("spag_multiper")
    })
    observeEvent(input$upload_spag_multiper2, {
        enableUpload("spag_multiper3")
        enableValidate("spag_multiper")
        disableCustomization("spag_multiper")
    })
    observeEvent(input$upload_spag_multiper3, {
        enableUpload("spag_multiper4")
        enableUploadRemoveBttn("spag_multiper3")
        disableCustomization("spag_multiper")
    })
    observeEvent(input$upload_spag_multiper4, {
        disableUploadRemoveBttn("spag_multiper3")
        enableUploadRemoveBttn("spag_multiper4")
        disableCustomization("spag_multiper")
    })

    # Observe File Removal
    observeEvent(input$remove_fisheries_multiyr3_bttn, {
        reset("upload_fisheries_multiyr3")
        clear_upload_fisheries_multiyr3(TRUE)
        enableUpload("fisheries_multiyr3")
        disableUpload("fisheries_multiyr4")
        disableUploadRemoveBttn("fisheries_multiyr3")
        enableUploadRemoveBttn("fisheries_multiyr2")
        removeConfirmation("fisheries_multiyr3")
    })
    observeEvent(input$remove_fisheries_multiyr4_bttn, {
        reset("upload_fisheries_multiyr4")
        clear_upload_fisheries_multiyr4(TRUE)
        enableUpload("fisheries_multiyr4")
        disableUploadRemoveBttn("fisheries_multiyr4")
        enableUploadRemoveBttn("fisheries_multiyr3")
        removeConfirmation("fisheries_multiyr4")
    })
    observeEvent(input$remove_fisher_multiyr3_bttn, {
        reset("upload_fisher_multiyr3")
        clear_upload_fisher_multiyr3(TRUE)
        enableUpload("fisher_multiyr3")
        disableUpload("fisher_multiyr4")
        disableUploadRemoveBttn("fisher_multiyr3")
        enableUploadRemoveBttn("fisher_multiyr2")
        removeConfirmation("fisher_multiyr3")
    })
    observeEvent(input$remove_fisher_multiyr4_bttn, {
        reset("upload_fisher_multiyr4")
        clear_upload_fisher_multiyr4(TRUE)
        enableUpload("fisher_multiyr4")
        disableUploadRemoveBttn("fisher_multiyr4")
        enableUploadRemoveBttn("fisher_multiyr3")
        removeConfirmation("fisher_multiyr4")
    })
    observeEvent(input$remove_lamp_multiper3_bttn, {
        reset("upload_lamp_multiper3")
        clear_upload_lamp_multiper3(TRUE)
        enableUpload("lamp_multiper3")
        disableUpload("lamp_multiper4")
        disableUploadRemoveBttn("lamp_multiper3")
        removeConfirmation("lamp_multiper3")
    })
    observeEvent(input$remove_lamp_multiper4_bttn, {
        reset("upload_lamp_multiper4")
        clear_upload_lamp_multiper4(TRUE)
        enableUpload("lamp_multiper4")
        disableUploadRemoveBttn("lamp_multiper4")
        enableUploadRemoveBttn("lamp_multiper3")
        removeConfirmation("lamp_multiper4")
    })
    observeEvent(input$remove_spag_multiper3_bttn, {
        reset("upload_spag_multiper3")
        clear_upload_spag_multiper3(TRUE)
        enableUpload("spag_multiper3")
        disableUpload("spag_multiper4")
        disableUploadRemoveBttn("spag_multiper3")
        enableUploadRemoveBttn("spag_multiper2")
        removeConfirmation("spag_multiper3")
    })
    observeEvent(input$remove_spag_multiper4_bttn, {
        reset("upload_spag_multiper4")
        clear_upload_spag_multiper4(TRUE)
        enableUpload("spag_multiper4")
        disableUploadRemoveBttn("spag_multiper4")
        enableUploadRemoveBttn("spag_multiper3")
        removeConfirmation("spag_multiper4")
    })

    # Observe Feedback
    observeEvent(input$feedback_opn_bttn, {
        shinyjs::show("feedback-content-box")
        shinyjs::hide("feedback_opn_bttn")
    })
    observeEvent(input$feedback_bttn, {
        if (is.null(input$feedback_text) || trimws(input$feedback_text) == "") {
            shinyalert("Error!", "Please enter feedback before submitting.",
                confirmButtonText = "Okay!", confirmButtonCol = "#E90C0C", type = "", size = "s"
            )
        } else {
            shinyjs::hide("feedback-content-box")
            shinyjs::show("feedback_opn_bttn")
            updateTextInput(session, "feedback_text", value = "")
            shinyalert("Success!", "Feedback Submitted!",
                confirmButtonText = "Okay!", confirmButtonCol = "#00AE46", type = "", size = "s"
            )
        }
    })

    # Observe action button "Manual" on home page
    observeEvent(input$go_to_manual_tab, {
        updateTabsetPanel(session = session, "navbar_page", selected = "Manual")
    })

    # Render list of data templates
    output$template_list_table <- renderUI({
        link_text <- paste(readLines("text/links.txt"))
        tags$table(
            class = "templates-table",
            tags$thead(
                tags$tr(
                    tags$th("Datatype"),
                    tags$th("Subtype"),
                    tags$th("Link to Template")
                )
            ),
            tags$tbody(
                # Iterates through lists to create table info
                lapply(1:6, function(i) {
                    tags$tr(
                        tags$td(c("Fisheries Catch", "Fisher Catch", "LAMP", "LAMP", "SPAG", "SPAG")[i]),
                        tags$td(c("-", "-", "Conch", "General", "Visual Census", "Laser Data")[i]),
                        tags$td(
                            tags$a(href = link_text[i + 1], "View Template", target = "_blank")
                        )
                    )
                })
            )
        )
    })
}
