## server.R

# Define release ---------------------------
release_version <- "v1.2.0"

# Source code ---------------------------
source("server_helpers.r")
source("theme.r")

# Define server ---------------------------
server <- function(input, output, session) {
    shinyjs::hide("feedback-content-box")

    # Check for pandoc
    if (!rmarkdown::pandoc_available()) {
        showModal(modalDialog(
            title = "Missing Pandoc",
            "Report generation is not available because Pandoc is not installed on this computer. Please install Pandoc or run the app in RStudio.",
            easyClose = TRUE
        ))
        return()
    }

    # Define flags for optional files being used
    fisheries_multiper3_flag <- reactiveVal(FALSE)
    fisheries_multiper4_flag <- reactiveVal(FALSE)
    fisher_multiper3_flag <- reactiveVal(FALSE)
    fisher_multiper4_flag <- reactiveVal(FALSE)
    lamp_multiper3_flag <- reactiveVal(FALSE)
    lamp_multiper4_flag <- reactiveVal(FALSE)
    spag_multiper3_flag <- reactiveVal(FALSE)
    spag_multiper4_flag <- reactiveVal(FALSE)
    dud_flag <- reactiveVal(FALSE)

    # Define flags for time period manual selection
    create_flags("lamp", "year")
    create_flags("lamp", "period")
    create_flags("fisher", "year")
    create_flags("fisher", "period")
    create_flags("fisher", "lobster_season")
    create_flags("fisher", "conch_season")
    create_flags("fisher", "finfish_season")

    # Define dataframes from uploads
    nas <- c("NA", "N/A", "Unknown", "Missing", "None", "")
    df_upload_fisheries_1per <- reactive({
        req(input$upload_fisheries_1per)
        read_excel(input$upload_fisheries_1per$datapath, sheet = 1, na = nas)
    })
    df_upload_fisheries_multiper1 <- reactive({
        req(input$upload_fisheries_multiper1)
        read_excel(input$upload_fisheries_multiper1$datapath, sheet = 1, na = nas)
    })
    df_upload_fisheries_multiper2 <- reactive({
        req(input$upload_fisheries_multiper2)
        read_excel(input$upload_fisheries_multiper2$datapath, sheet = 1, na = nas)
    })
    df_upload_fisheries_multiper3 <- reactive({
        req(input$upload_fisheries_multiper3)
        read_excel(input$upload_fisheries_multiper3$datapath, sheet = 1, na = nas)
    })
    df_upload_fisheries_multiper4 <- reactive({
        req(input$upload_fisheries_multiper4)
        read_excel(input$upload_fisheries_multiper4$datapath, sheet = 1, na = nas)
    })
    df_upload_fisher_1per <- reactive({
        req(input$upload_fisher_1per)
        file_path <- input$upload_fisher_1per$datapath
        datatype <- input$datatype_fisher_1per
        read_fisher_data(file_path, datatype)
    })
    df_upload_fisher_multiper1 <- reactive({
        req(input$upload_fisher_multiper1)
        file_path <- input$upload_fisher_multiper1$datapath
        datatype <- input$datatype_fisher_multiper
        read_fisher_data(file_path, datatype)
    })
    df_upload_fisher_multiper2 <- reactive({
        req(input$upload_fisher_multiper2)
        file_path <- input$upload_fisher_multiper2$datapath
        datatype <- input$datatype_fisher_multiper
        read_fisher_data(file_path, datatype)
    })
    df_upload_fisher_multiper3 <- reactive({
        req(input$upload_fisher_multiper3)
        file_path <- input$upload_fisher_multiper3$datapath
        datatype <- input$datatype_fisher_multiper
        read_fisher_data(file_path, datatype)
    })
    df_upload_fisher_multiper4 <- reactive({
        req(input$upload_fisher_multiper4)
        file_path <- input$upload_fisher_multiper4$datapath
        datatype <- input$datatype_fisher_multiper
        read_fisher_data(file_path, datatype)
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
        req(input$upload_lamp_multiper3)
        file_path <- input$upload_lamp_multiper3$datapath
        datatype <- input$datatype_lamp_multiper
        read_lamp_data(file_path, datatype)
    })
    df_upload_lamp_multiper4 <- reactive({
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
        req(input$upload_spag_multiper3)
        read_excel(input$upload_spag_multiper3$datapath, sheet = 1, na = nas)
    })
    df_upload_spag_multiper4 <- reactive({
        req(input$upload_spag_multiper4)
        read_excel(input$upload_spag_multiper4$datapath, sheet = 1, na = nas)
    })

    # Read and report year of datafile
    output$ui_upload_fisheries_1per <- renderUI({
        check_datafile_dates(df_upload_fisheries_1per(), "year", "fisheries_1per_", dud_flag, dud_flag)
    })
    output$ui_upload_fisheries_multiper1 <- renderUI({
        check_datafile_dates(df_upload_fisheries_multiper1(), "year", "fisheries_multiper1_", dud_flag, dud_flag)
    })
    output$ui_upload_fisheries_multiper2 <- renderUI({
        check_datafile_dates(df_upload_fisheries_multiper2(), "year", "fisheries_multiper2_", dud_flag, dud_flag)
    })
    output$ui_upload_fisheries_multiper3 <- renderUI({
        check_datafile_dates(df_upload_fisheries_multiper3(), "year", "fisheries_multiper3_", dud_flag, dud_flag)
    })
    output$ui_upload_fisheries_multiper4 <- renderUI({
        check_datafile_dates(df_upload_fisheries_multiper4(), "year", "fisheries_multiper4_", dud_flag, dud_flag)
    })
    output$ui_upload_fisher_1per <- renderUI({
        season_type <- switch(input$timeframe_fisher_1per,
            "Annual" = "year",
            if (input$datatype_fisher_1per == "Lobster") "lobster_season" else if (input$datatype_fisher_1per == "Conch") "conch_season" else if (input$datatype_fisher_1per == "Finfish") "finfish_season" else NULL
        )
        if (!is.null(season_type)) {
            check_datafiles_dates_fisher_1per(season_type, df_upload_fisher_1per())
        }
    })
    output$ui_upload_fisher_multiper1 <- renderUI({
        season_type <- switch(input$timeframe_fisher_multiper,
            "Annual" = "year",
            if (input$datatype_fisher_multiper == "Lobster") "lobster_season" else if (input$datatype_fisher_multiper == "Conch") "conch_season" else if (input$datatype_fisher_multiper == "Finfish") "finfish_season" else NULL
        )
        if (!is.null(season_type)) {
            check_datafiles_dates_fisher_multiper1(season_type, df_upload_fisher_multiper1())
        }
    })
    output$ui_upload_fisher_multiper2 <- renderUI({
        season_type <- switch(input$timeframe_fisher_multiper,
            "Annual" = "year",
            if (input$datatype_fisher_multiper == "Lobster") "lobster_season" else if (input$datatype_fisher_multiper == "Conch") "conch_season" else if (input$datatype_fisher_multiper == "Finfish") "finfish_season" else NULL
        )
        if (!is.null(season_type)) {
            check_datafiles_dates_fisher_multiper2(season_type, df_upload_fisher_multiper2())
        }
    })
    output$ui_upload_fisher_multiper3 <- renderUI({
        season_type <- switch(input$timeframe_fisher_multiper,
            "Annual" = "year",
            if (input$datatype_fisher_multiper == "Lobster") "lobster_season" else if (input$datatype_fisher_multiper == "Conch") "conch_season" else if (input$datatype_fisher_multiper == "Finfish") "finfish_season" else NULL
        )
        if (!is.null(season_type)) {
            check_datafiles_dates_fisher_multiper3(season_type, df_upload_fisher_multiper3())
        }
    })
    output$ui_upload_fisher_multiper4 <- renderUI({
        season_type <- switch(input$timeframe_fisher_multiper,
            "Annual" = "year",
            if (input$datatype_fisher_multiper == "Lobster") "lobster_season" else if (input$datatype_fisher_multiper == "Conch") "conch_season" else if (input$datatype_fisher_multiper == "Finfish") "finfish_season" else NULL
        )
        if (!is.null(season_type)) {
            check_datafiles_dates_fisher_multiper4(season_type, df_upload_fisher_multiper4())
        }
    })
    output$ui_upload_lamp_1per <- renderUI({
        if (input$datatype_lamp_1per == "Conch") {
            check_datafiles_dates(
                df_upload_lamp_1per(), "year", "lamp_1per_", lamp_1per_year_selection_flag, lamp_1per_period_selection_flag,
                dud_flag, dud_flag, dud_flag
            )
        } else {
            check_datafiles_dates(
                df_upload_lamp_1per(), "period", "lamp_1per_", lamp_1per_year_selection_flag, lamp_1per_period_selection_flag,
                dud_flag, dud_flag, dud_flag
            )
        }
    })
    output$ui_upload_lamp_multiper1 <- renderUI({
        if (input$datatype_lamp_multiper == "Conch") {
            check_datafiles_dates(
                df_upload_lamp_multiper1(), "year", "lamp_multiper1_", lamp_multiper1_year_selection_flag, lamp_multiper1_period_selection_flag,
                dud_flag, dud_flag, dud_flag
            )
        } else {
            check_datafiles_dates(
                df_upload_lamp_multiper1(), "period", "lamp_multiper1_", lamp_multiper1_year_selection_flag, lamp_multiper1_period_selection_flag,
                dud_flag, dud_flag, dud_flag
            )
        }
    })
    output$ui_upload_lamp_multiper2 <- renderUI({
        if (input$datatype_lamp_multiper == "Conch") {
            check_datafiles_dates(
                df_upload_lamp_multiper2(), "year", "lamp_multiper2_", lamp_multiper2_year_selection_flag, lamp_multiper2_period_selection_flag,
                dud_flag, dud_flag, dud_flag
            )
        } else {
            check_datafiles_dates(
                df_upload_lamp_multiper2(), "period", "lamp_multiper2_", lamp_multiper2_year_selection_flag, lamp_multiper2_period_selection_flag,
                dud_flag, dud_flag, dud_flag
            )
        }
    })
    output$ui_upload_lamp_multiper3 <- renderUI({
        if (lamp_multiper3_flag()) {
            if (input$datatype_lamp_multiper == "Conch") {
                check_datafiles_dates(
                    df_upload_lamp_multiper3(), "year", "lamp_multiper3_", lamp_multiper3_year_selection_flag, lamp_multiper3_period_selection_flag,
                    dud_flag, dud_flag, dud_flag
                )
            } else {
                check_datafiles_dates(
                    df_upload_lamp_multiper3(), "period", "lamp_multiper3_", lamp_multiper3_year_selection_flag, lamp_multiper3_period_selection_flag,
                    dud_flag, dud_flag, dud_flag
                )
            }
        }
    })
    output$ui_upload_lamp_multiper4 <- renderUI({
        if (lamp_multiper4_flag()) {
            if (input$datatype_lamp_multiper == "Conch") {
                check_datafiles_dates(
                    df_upload_lamp_multiper4(), "year", "lamp_multiper4_", lamp_multiper4_year_selection_flag, lamp_multiper4_period_selection_flag,
                    dud_flag, dud_flag, dud_flag
                )
            } else {
                check_datafiles_dates(
                    df_upload_lamp_multiper4(), "period", "lamp_multiper4_", lamp_multiper4_year_selection_flag, lamp_multiper4_period_selection_flag,
                    dud_flag, dud_flag, dud_flag
                )
            }
        }
    })
    output$ui_upload_spag_1per <- renderUI({
        check_datafile_dates(df_upload_spag_1per(), "period", "spag_1per_", dud_flag, dud_flag)
    })
    output$ui_upload_spag_multiper1 <- renderUI({
        check_datafile_dates(df_upload_spag_multiper1(), "period", "spag_multiper1_", dud_flag, dud_flag)
    })
    output$ui_upload_spag_multiper2 <- renderUI({
        check_datafile_dates(df_upload_spag_multiper2(), "period", "spag_multiper2_", dud_flag, dud_flag)
    })
    output$ui_upload_spag_multiper3 <- renderUI({
        check_datafile_dates(df_upload_spag_multiper3(), "period", "spag_multiper3_", dud_flag, dud_flag)
    })
    output$ui_upload_spag_multiper4 <- renderUI({
        check_datafile_dates(df_upload_spag_multiper4(), "period", "spag_multiper4_", dud_flag, dud_flag)
    })

    # Observe Upload
    observeEvent(input$upload_fisheries_1per, {
        if (!is.null(input$upload_fisheries_1per)) {
            enableValidate("fisheries_1per")
            disableCustomization("fisheries_1per")
        }
    })
    observeEvent(input$upload_fisher_1per, {
        if (!is.null(input$upload_fisher_1per)) {
            enableValidate("fisher_1per")
            disableCustomization("fisher_1per")
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

    # Observe Multiyear Upload
    observeEvent(input$upload_fisheries_multiper1, {
        enableUpload("fisheries_multiper2")
        disableCustomization("fisheries_multiper")
    })
    observeEvent(input$upload_fisheries_multiper2, {
        enableUpload("fisheries_multiper3")
        enableValidate("fisheries_multiper")
        disableCustomization("fisheries_multiper")
    })
    observeEvent(input$upload_fisheries_multiper3, {
        enableUpload("fisheries_multiper4")
        enableUploadRemoveBttn("fisheries_multiper3")
        disableCustomization("fisheries_multiper")
        fisheries_multiper3_flag(TRUE)
    })
    observeEvent(input$upload_fisheries_multiper4, {
        disableUploadRemoveBttn("fisheries_multiper3")
        enableUploadRemoveBttn("fisheries_multiper4")
        disableCustomization("fisheries_multiper")
        fisheries_multiper4_flag(TRUE)
    })
    observeEvent(input$upload_fisher_multiper1, {
        enableUpload("fisher_multiper2")
        disableCustomization("fisher_multiper")
    })
    observeEvent(input$upload_fisher_multiper2, {
        enableUpload("fisher_multiper3")
        enableValidate("fisher_multiper")
        disableCustomization("fisher_multiper")
    })
    observeEvent(input$upload_fisher_multiper3, {
        enableUpload("fisher_multiper4")
        enableUploadRemoveBttn("fisher_multiper3")
        disableCustomization("fisher_multiper")
        fisher_multiper3_flag(TRUE)
    })
    observeEvent(input$upload_fisher_multiper4, {
        disableUploadRemoveBttn("fisher_multiper3")
        enableUploadRemoveBttn("fisher_multiper4")
        disableCustomization("fisher_multiper")
        fisher_multiper4_flag(TRUE)
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
        lamp_multiper3_flag(TRUE)
    })
    observeEvent(input$upload_lamp_multiper4, {
        disableUploadRemoveBttn("lamp_multiper3")
        enableUploadRemoveBttn("lamp_multiper4")
        disableCustomization("lamp_multiper")
        lamp_multiper4_flag(TRUE)
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
        spag_multiper3_flag(TRUE)
    })
    observeEvent(input$upload_spag_multiper4, {
        disableUploadRemoveBttn("spag_multiper3")
        enableUploadRemoveBttn("spag_multiper4")
        disableCustomization("spag_multiper")
        spag_multiper4_flag(TRUE)
    })

    # Observe File Removal
    observeEvent(input$remove_fisheries_multiper3_bttn, {
        reset("upload_fisheries_multiper3")
        enableUpload("fisheries_multiper3")
        disableUpload("fisheries_multiper4")
        disableUploadRemoveBttn("fisheries_multiper3")
        enableUploadRemoveBttn("fisheries_multiper2")
        removeConfirmation("fisheries_multiper3")
        fisheries_multiper3_flag(FALSE)
    })
    observeEvent(input$remove_fisheries_multiper4_bttn, {
        reset("upload_fisheries_multiper4")
        enableUpload("fisheries_multiper4")
        disableUploadRemoveBttn("fisheries_multiper4")
        enableUploadRemoveBttn("fisheries_multiper3")
        removeConfirmation("fisheries_multiper4")
        fisheries_multiper4_flag(FALSE)
    })
    observeEvent(input$remove_fisher_multiper3_bttn, {
        reset("upload_fisher_multiper3")
        enableUpload("fisher_multiper3")
        disableUpload("fisher_multiper4")
        disableUploadRemoveBttn("fisher_multiper3")
        enableUploadRemoveBttn("fisher_multiper2")
        removeConfirmation("fisher_multiper3")
        fisher_multiper3_flag(FALSE)
    })
    observeEvent(input$remove_fisher_multiper4_bttn, {
        reset("upload_fisher_multiper4")
        enableUpload("fisher_multiper4")
        disableUploadRemoveBttn("fisher_multiper4")
        enableUploadRemoveBttn("fisher_multiper3")
        removeConfirmation("fisher_multiper4")
        fisher_multiper4_flag(FALSE)
    })
    observeEvent(input$remove_lamp_multiper3_bttn, {
        reset("upload_lamp_multiper3")
        enableUpload("lamp_multiper3")
        disableUpload("lamp_multiper4")
        disableUploadRemoveBttn("lamp_multiper3")
        removeConfirmation("lamp_multiper3")
        lamp_multiper3_flag(FALSE)
    })
    observeEvent(input$remove_lamp_multiper4_bttn, {
        reset("upload_lamp_multiper4")
        enableUpload("lamp_multiper4")
        disableUploadRemoveBttn("lamp_multiper4")
        enableUploadRemoveBttn("lamp_multiper3")
        removeConfirmation("lamp_multiper4")
        lamp_multiper4_flag(FALSE)
    })
    observeEvent(input$remove_spag_multiper3_bttn, {
        reset("upload_spag_multiper3")
        enableUpload("spag_multiper3")
        disableUpload("spag_multiper4")
        disableUploadRemoveBttn("spag_multiper3")
        enableUploadRemoveBttn("spag_multiper2")
        removeConfirmation("spag_multiper3")
        spag_multiper3_flag(FALSE)
    })
    observeEvent(input$remove_spag_multiper4_bttn, {
        reset("upload_spag_multiper4")
        enableUpload("spag_multiper4")
        disableUploadRemoveBttn("spag_multiper4")
        enableUploadRemoveBttn("spag_multiper3")
        removeConfirmation("spag_multiper4")
        spag_multiper4_flag(FALSE)
    })

    # Validate dataframes
    observeEvent(input$validate_fisheries_1per, {
        shinyalert("Notice!", "Validation has not been implemented for Fisheries Single Year as of yet!",
            confirmButtonText = "I Understand", confirmButtonCol = "#cde9f0", type = "info", size = "s"
        )
        enableCustomization("fisheries_1per")
    })
    observeEvent(input$validate_fisheries_multiper, {
        shinyalert("Notice!", "Validation has not been implemented for Fisheries Multi-Year as of yet!",
            confirmButtonText = "I Understand", confirmButtonCol = "#cde9f0", type = "info", size = "s"
        )
        enableCustomization("fisheries_multiper")
    })
    source("validation/validation_observers/validate_fisher_observer_1per.r", local = TRUE)
    observeEvent(input$validate_fisher_multiper, {
        shinyalert("Notice!", "Validation has not been implemented for Fisher Project Multi-Year as of yet!",
            confirmButtonText = "I Understand", confirmButtonCol = "#cde9f0", type = "info", size = "s"
        )
        enableCustomization("fisher_multiper")
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

    # Observe report generate buttons
    observeEvent(input$report_fisheries_1per,
        {
            if (nchar(input$fisheries_1per_name) >= 2) {
                click("report_fisheries_1per_hidden")
            }
        },
        ignoreInit = T
    )
    observeEvent(input$report_fisheries_multiper,
        {
            if (nchar(input$fisheries_multiper_name) >= 2) {
                click("report_fisheries_multiper_hidden")
            }
        },
        ignoreInit = T
    )
    observeEvent(input$report_fisher_1per,
        {
            if (nchar(input$fisher_1per_name) >= 2) {
                click("report_fisher_1per_hidden")
            }
        },
        ignoreInit = T
    )
    observeEvent(input$report_fisher_multiper,
        {
            if (nchar(input$fisher_multiper_name) >= 2) {
                click("report_fisher_multiper_hidden")
            }
        },
        ignoreInit = T
    )
    observeEvent(input$report_lamp_1per,
        {
            if (nchar(input$lamp_1per_name) >= 2) {
                click("report_lamp_1per_hidden")
            }
        },
        ignoreInit = T
    )
    observeEvent(input$report_lamp_multiper,
        {
            if (nchar(input$lamp_multiper_name) >= 2) {
                click("report_lamp_multiper_hidden")
            }
        },
        ignoreInit = T
    )
    observeEvent(input$report_spag_1per,
        {
            if (nchar(input$spag_1per_name) >= 2) {
                click("report_spag_1per_hidden")
            }
        },
        ignoreInit = T
    )
    observeEvent(input$report_spag_multiper,
        {
            if (nchar(input$spag_multiper_name) >= 2) {
                click("report_spag_multiper_hidden")
            }
        },
        ignoreInit = T
    )

    # Observe Name Text Inputs
    observeEvent(input$fisheries_1per_name, {
        nameLengthCheck(input$fisheries_1per_name, "fisheries_1per")
    })
    observeEvent(input$fisheries_multiper_name, {
        nameLengthCheck(input$fisheries_multiper_name, "fisheries_multiper")
    })
    observeEvent(input$fisher_1per_name, {
        nameLengthCheck(input$fisher_1per_name, "fisher_1per")
    })
    observeEvent(input$fisher_multiper_name, {
        nameLengthCheck(input$fisher_multiper_name, "fisher_multiper")
    })
    observeEvent(input$lamp_1per_name, {
        nameLengthCheck(input$lamp_1per_name, "lamp_1per")
    })
    observeEvent(input$lamp_multiper_name, {
        nameLengthCheck(input$lamp_multiper_name, "lamp_multiper")
    })
    observeEvent(input$spag_1per_name, {
        nameLengthCheck(input$spag_1per_name, "spag_1per")
    })
    observeEvent(input$spag_multiper_name, {
        nameLengthCheck(input$spag_multiper_name, "spag_multiper")
    })

    # Download figures
    source("figures/figures_fisher_1per.r", local = TRUE)
    source("figures/figures_fisheries_1per.r", local = TRUE)
    source("figures/figures_lamp_1per.r", local = TRUE)
    source("figures/figures_spag_1per.r", local = TRUE)
    source("figures/figures_fisher_multiper.r", local = TRUE)
    source("figures/figures_fisheries_multiper.r", local = TRUE)
    source("figures/figures_lamp_multiper.r", local = TRUE)
    source("figures/figures_spag_multiper.r", local = TRUE)

    # Create reports
    output$report_fisheries_1per_hidden <- downloadHandler(
        content = function(file) {
            showLoaderBar("fisheries_1per", session)
            report_file <- "report_fisheries_1per.Rmd"
            src <- normalizePath(c(
                paste0("reports/", report_file),
                "reports/report_template.docx",
                "www/images/TASA_logo_full_color.png"
            ))
            owd <- setwd(tempdir())
            on.exit(setwd(owd))
            file.copy(src, c(report_file, "report_template.docx", "TASA_logo_full_color.png"), overwrite = TRUE)
            out <- tryCatch(
                {
                    render(
                        report_file,
                        params = list(user_name = input$fisheries_1per_name, datafile = df_upload_fisheries_1per(), release = release_version),
                        envir = new.env(parent = globalenv())
                    )
                },
                error = function(e) {}
            )
            hideLoaderBar("fisheries_1per", session)
            file.rename(out, file)
        },
        filename = function() "report_fisheries_1per.docx"
    )
    output$report_fisheries_multiper_hidden <- downloadHandler(
        filename = function() "report_fisheries_multiper.docx",
        content = function(file) {
            showLoaderBar("fisheries_multiper", session)
            report_file <- "report_fisheries_multiper.Rmd"
            src <- normalizePath(c(
                paste0("reports/", report_file),
                "reports/report_template.docx",
                "www/images/TASA_logo_full_color.png"
            ))
            owd <- setwd(tempdir())
            on.exit(setwd(owd))
            file.copy(src, c(report_file, "report_template.docx", "TASA_logo_full_color.png"), overwrite = TRUE)
            out <- tryCatch(
                {
                    render(
                        report_file,
                        params = list(user_name = input$fisheries_multiper_name, datafile = df_upload_fisheries_multiper1(), release = release_version),
                        envir = new.env(parent = globalenv())
                    )
                },
                error = function(e) {}
            )
            hideLoaderBar("fisheries_multiper", session)
            file.rename(out, file)
        }
    )
    output$report_fisher_1per_hidden <- downloadHandler(
        filename = function() {
            report_file <- switch(input$datatype_fisher_1per,
                "Conch" = "report_fisher_conch_1per.Rmd",
                "Lobster" = "report_fisher_lobster_1per.Rmd",
                "Finfish" = "report_fisher_finfish_1per.Rmd"
            )
            gsub(".Rmd", ".docx", report_file)
        },
        content = function(file) {
            showLoaderBar("fisher_1per", session)
            report_file <- switch(input$datatype_fisher_1per,
                "Conch" = "report_fisher_conch_1per.Rmd",
                "Lobster" = "report_fisher_lobster_1per.Rmd",
                "Finfish" = "report_fisher_finfish_1per.Rmd"
            )
            shapefiles <- list.files("shapefiles", full.names = TRUE)
            normalized_shapefiles <- normalizePath(shapefiles)
            src <- normalizePath(c(
                paste0("reports/", report_file),
                "reports/report_template.docx",
                "www/images/TASA_logo_full_color.png",
                "theme.r", "map.r",
                normalized_shapefiles
            ))
            owd <- setwd(tempdir())
            on.exit(setwd(owd))
            file.copy(src, c(report_file, "report_template.docx", "TASA_logo_full_color.png", "theme.r", "map.r", basename(shapefiles)), overwrite = TRUE)
            out <- tryCatch(
                {
                    render(
                        report_file,
                        params = list(
                            release = release_version,
                            user_name = input$fisher_1per_name,
                            datafile_name = input$upload_fisher_1per$name,
                            datafile = df_upload_fisher_1per(),
                            fisher_1per_timeframe = input$timeframe_fisher_1per,
                            fisher_1per_year_selection = if (fisher_1per_year_selection_flag()) input[["fisher_1per_year_selection"]] else "None",
                            fisher_1per_period_selection = if (fisher_1per_period_selection_flag()) input[["fisher_1per_period_selection"]] else "None",
                            fisher_1per_lobster_season_selection = if (fisher_1per_lobster_season_selection_flag()) input[["fisher_1per_lobster_season_selection"]] else "None",
                            fisher_1per_conch_season_selection = if (fisher_1per_conch_season_selection_flag()) input[["fisher_1per_conch_season_selection"]] else "None",
                            fisher_1per_finfish_season_selection = if (fisher_1per_finfish_season_selection_flag()) input[["fisher_1per_finfish_season_selection"]] else "None"
                        ),
                        envir = new.env(parent = globalenv())
                    )
                },
                error = function(e) {}
            )
            hideLoaderBar("fisher_1per", session)
            file.rename(out, file)
        }
    )
    output$report_fisher_multiper_hidden <- downloadHandler(
        filename = function() {
            report_file <- switch(input$datatype_fisher_multiper,
                "Conch" = "report_fisher_conch_multiper.Rmd",
                "Lobster" = "report_fisher_lobster_multiper.Rmd",
                "Finfish" = "report_fisher_finfish_multiper.Rmd"
            )
            gsub(".Rmd", ".docx", report_file)
        },
        content = function(file) {
            showLoaderBar("fisher_multiper", session)
            report_file <- switch(input$datatype_fisher_multiper,
                "Conch" = "report_fisher_conch_multiper.Rmd",
                "Lobster" = "report_fisher_lobster_multiper.Rmd",
                "Finfish" = "report_fisher_finfish_multiper.Rmd"
            )
            shapefiles <- list.files("shapefiles", full.names = TRUE)
            normalized_shapefiles <- normalizePath(shapefiles)
            src <- normalizePath(c(
                paste0("reports/", report_file),
                "reports/report_template.docx",
                "www/images/TASA_logo_full_color.png",
                "theme.r", "map.r",
                normalized_shapefiles
            ))
            owd <- setwd(tempdir())
            on.exit(setwd(owd))
            file.copy(src, c(report_file, "report_template.docx", "TASA_logo_full_color.png", "theme.r", "map.r", basename(shapefiles)), overwrite = TRUE)
            params_list <- list(
                release = release_version,
                user_name = input$fisher_multiper_name,
                fisher_multiper_timeframe = input$timeframe_fisher_multiper,
                datafile1_name = input$upload_fisher_multiper1$name,
                datafile1 = df_upload_fisher_multiper1(),
                datafile2_name = input$upload_fisher_multiper2$name,
                datafile2 = df_upload_fisher_multiper2(),
                fisher_multiper1_year_selection = if (fisher_multiper1_year_selection_flag()) input[["fisher_multiper1_year_selection"]] else "None",
                fisher_multiper1_period_selection = if (fisher_multiper1_period_selection_flag()) input[["fisher_multiper1_period_selection"]] else "None",
                fisher_multiper1_lobster_season_selection = if (fisher_multiper1_lobster_season_selection_flag()) input[["fisher_multiper1_lobster_season_selection"]] else "None",
                fisher_multiper1_conch_season_selection = if (fisher_multiper1_conch_season_selection_flag()) input[["fisher_multiper1_conch_season_selection"]] else "None",
                fisher_multiper1_finfish_season_selection = if (fisher_multiper1_finfish_season_selection_flag()) input[["fisher_multiper1_finfish_season_selection"]] else "None",
                fisher_multiper2_year_selection = if (fisher_multiper2_year_selection_flag()) input[["fisher_multiper2_year_selection"]] else "None",
                fisher_multiper2_period_selection = if (fisher_multiper2_period_selection_flag()) input[["fisher_multiper2_period_selection"]] else "None",
                fisher_multiper2_lobster_season_selection = if (fisher_multiper2_lobster_season_selection_flag()) input[["fisher_multiper2_lobster_season_selection"]] else "None",
                fisher_multiper2_conch_season_selection = if (fisher_multiper2_conch_season_selection_flag()) input[["fisher_multiper2_conch_season_selection"]] else "None",
                fisher_multiper2_finfish_season_selection = if (fisher_multiper2_finfish_season_selection_flag()) input[["fisher_multiper2_finfish_season_selection"]] else "None"
            )
            if (!is.null(input$upload_fisher_multiper3)) {
                params_list$datafile3_name <- input$upload_fisher_multiper3$name
                params_list$datafile3 <- df_upload_fisher_multiper3()
                params_list$fisher_multiper3_year_selection <- if (fisher_multiper3_year_selection_flag()) input[["fisher_multiper3_year_selection"]] else "None"
                params_list$fisher_multiper3_period_selection <- if (fisher_multiper3_period_selection_flag()) input[["fisher_multiper3_period_selection"]] else "None"
                params_list$fisher_multiper3_lobster_season_selection <- if (fisher_multiper3_lobster_season_selection_flag()) input[["fisher_multiper3_lobster_season_selection"]] else "None"
                params_list$fisher_multiper3_conch_season_selection <- if (fisher_multiper3_conch_season_selection_flag()) input[["fisher_multiper3_conch_season_selection"]] else "None"
                params_list$fisher_multiper3_finfish_season_selection <- if (fisher_multiper3_finfish_season_selection_flag()) input[["fisher_multiper3_finfish_season_selection"]] else "None"
            }
            if (!is.null(input$upload_fisher_multiper4)) {
                params_list$datafile4_name <- input$upload_fisher_multiper4$name
                params_list$datafile4 <- df_upload_fisher_multiper4()
                params_list$fisher_multiper4_year_selection <- if (fisher_multiper4_year_selection_flag()) input[["fisher_multiper4_year_selection"]] else "None"
                params_list$fisher_multiper4_period_selection <- if (fisher_multiper4_period_selection_flag()) input[["fisher_multiper4_period_selection"]] else "None"
                params_list$fisher_multiper4_lobster_season_selection <- if (fisher_multiper4_lobster_season_selection_flag()) input[["fisher_multiper4_lobster_season_selection"]] else "None"
                params_list$fisher_multiper4_conch_season_selection <- if (fisher_multiper4_conch_season_selection_flag()) input[["fisher_multiper4_conch_season_selection"]] else "None"
                params_list$fisher_multiper4_finfish_season_selection <- if (fisher_multiper4_finfish_season_selection_flag()) input[["fisher_multiper4_finfish_season_selection"]] else "None"
            }
            out <- tryCatch(
                {
                    render(
                        report_file,
                        params = params_list,
                        envir = new.env(parent = globalenv())
                    )
                },
                error = function(e) {}
            )
            hideLoaderBar("fisher_multiper", session)
            file.rename(out, file)
        }
    )
    output$report_lamp_1per_hidden <- downloadHandler(
        filename = function() {
            report_file <- switch(input$datatype_lamp_1per,
                "Conch" = "report_lampconch_1per.Rmd",
                "General LAMP" = "report_lampgen_1per.Rmd",
            )
            gsub(".Rmd", ".docx", report_file)
        },
        content = function(file) {
            showLoaderBar("lamp_1per", session)
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
            out <- tryCatch(
                {
                    render(
                        report_file,
                        params = list(
                            release = release_version,
                            user_name = input$lamp_1per_name,
                            datafile_name = input$upload_lamp_1per$name,
                            datafile = df_upload_lamp_1per(),
                            lamp_1per_year_selection = if (lamp_1per_year_selection_flag()) input[["lamp_1per_year_selection"]] else "None",
                            lamp_1per_period_selection = if (lamp_1per_period_selection_flag()) input[["lamp_1per_period_selection"]] else "None"
                        ),
                        envir = new.env(parent = globalenv())
                    )
                },
                error = function(e) {}
            )
            hideLoaderBar("lamp_1per", session)
            file.rename(out, file)
        }
    )
    output$report_lamp_multiper_hidden <- downloadHandler(
        filename = function() {
            report_file <- switch(input$datatype_lamp_multiper,
                "Conch" = "report_lampconch_multiper.Rmd",
                "General LAMP" = "report_lampgen_multiper.Rmd",
            )
            gsub(".Rmd", ".docx", report_file)
        },
        content = function(file) {
            showLoaderBar("lamp_multiper", session)
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
                release = release_version,
                user_name = input$lamp_multiper_name,
                datafile1_name = input$upload_lamp_multiper1$name,
                datafile1 = df_upload_lamp_multiper1(),
                datafile2_name = input$upload_lamp_multiper2$name,
                datafile2 = df_upload_lamp_multiper2(),
                lamp_multiper1_year_selection = if (lamp_multiper1_year_selection_flag()) input[["lamp_multiper1_year_selection"]] else "None",
                lamp_multiper1_period_selection = if (lamp_multiper1_period_selection_flag()) input[["lamp_multiper1_period_selection"]] else "None",
                lamp_multiper2_year_selection = if (lamp_multiper2_year_selection_flag()) input[["lamp_multiper2_year_selection"]] else "None",
                lamp_multiper2_period_selection = if (lamp_multiper2_period_selection_flag()) input[["lamp_multiper2_period_selection"]] else "None"
            )
            if (!is.null(input$upload_lamp_multiper3)) {
                params_list$datafile3_name <- input$upload_lamp_multiper3$name
                params_list$datafile3 <- df_upload_lamp_multiper3()
                params_list$lamp_multiper3_year_selection <- if (lamp_multiper3_year_selection_flag()) input[["lamp_multiper3_year_selection"]] else "None"
                params_list$lamp_multiper3_period_selection <- if (lamp_multiper3_period_selection_flag()) input[["lamp_multiper3_period_selection"]] else "None"
            }
            if (!is.null(input$upload_lamp_multiper4)) {
                params_list$datafile4_name <- input$upload_lamp_multiper4$name
                params_list$datafile4 <- df_upload_lamp_multiper4()
                params_list$lamp_multiper4_year_selection <- if (lamp_multiper4_year_selection_flag()) input[["lamp_multiper4_year_selection"]] else "None"
                params_list$lamp_multiper4_period_selection <- if (lamp_multiper4_period_selection_flag()) input[["lamp_multiper4_period_selection"]] else "None"
            }
            out <- tryCatch(
                {
                    render(
                        report_file,
                        params = params_list,
                        envir = new.env(parent = globalenv())
                    )
                },
                error = function(e) {}
            )
            hideLoaderBar("lamp_multiper", session)
            file.rename(out, file)
        }
    )
    output$report_spag_1per_hidden <- downloadHandler(
        filename = function() {
            report_file <- switch(input$datatype_spag_1per,
                "Visual Census" = "report_spagvis_1per.Rmd",
                "Laser" = "report_spaglaser_1per.Rmd"
            )
            gsub(".Rmd", ".docx", report_file)
        },
        content = function(file) {
            showLoaderBar("spag_1per", session)
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
                params = list(release = release_version, user_name = input$spag_name, datafile = df_upload_spag_1per()),
                envir = new.env(parent = globalenv())
            )
            hideLoaderBar("spag_1per", session)
            file.rename(out, file)
        }
    )
    output$report_spag_multiper_hidden <- downloadHandler(
        filename = function() {
            report_file <- switch(input$datatype_spag_multiper,
                "Visual Census" = "report_spagvis_multiper.Rmd",
                "Laser" = "report_spaglaser_multiper.Rmd"
            )
            gsub(".Rmd", ".docx", report_file)
        },
        content = function(file) {
            showLoaderBar("spag_multiper", session)
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
                params = list(release = release_version, user_name = input$spag_name, datafile = df_upload_spag_multiper1()),
                envir = new.env(parent = globalenv())
            )
            hideLoaderBar("spag_multiper", session)
            file.rename(out, file)
        }
    )

    # Observe Datatype
    observeEvent(input$datatype_fisheries_1per, {
        disableCustomization("fisheries_1per")
        session$sendCustomMessage("triggerChangeFisheriesImg", list(dataType = input$datatype_fisheries_1per, isMulti = FALSE))
    })
    observeEvent(input$datatype_fisheries_multiper, {
        disableCustomization("fisheries_multiper")
        session$sendCustomMessage("triggerChangeFisheriesImg", list(dataType = input$datatype_fisheries_multiper, isMulti = TRUE))
    })
    observeEvent(input$datatype_fisher_1per, {
        if (input$datatype_fisher_1per == "Finfish") {
            updatePrettyRadioButtons(
                session = session,
                inputId = "timeframe_fisher_1per",
                label = NULL,
                choices = c("Annual"),
                selected = "Annual",
                inline = TRUE
            )
        } else {
            selected <- input$timeframe_fisher_1per
            updatePrettyRadioButtons(
                session = session,
                inputId = "timeframe_fisher_1per",
                label = NULL,
                choices = c("Seasonal", "Annual"),
                selected = selected,
                inline = TRUE
            )
        }
        disableCustomization("fisher_1per")
        flag_types <- c("year", "period", "lobster_season", "conch_season", "finfish_season")
        lapply(flag_types, function(ft) set_flags_1per("fisher", ft, FALSE))
        session$sendCustomMessage("triggerChangeFisherImg", list(dataType = input$datatype_fisher_1per, isMulti = FALSE))
    })
    observeEvent(input$datatype_fisher_multiper, {
        if (input$datatype_fisher_multiper == "Finfish") {
            updatePrettyRadioButtons(
                session = session,
                inputId = "timeframe_fisher_multiper",
                label = NULL,
                choices = c("Annual"),
                selected = "Annual",
                inline = TRUE
            )
        } else {
            selected <- input$timeframe_fisher_multiper
            updatePrettyRadioButtons(
                session = session,
                inputId = "timeframe_fisher_multiper",
                label = NULL,
                choices = c("Seasonal", "Annual"),
                selected = selected,
                inline = TRUE
            )
        }
        disableCustomization("fisher_multiper")
        flag_types <- c("year", "period", "lobster_season", "conch_season", "finfish_season")
        lapply(flag_types, function(ft) set_flags_multiper("fisher", ft, FALSE))
        session$sendCustomMessage("triggerChangeFisherImg", list(dataType = input$datatype_fisher_multiper, isMulti = TRUE))
    })
    observeEvent(input$timeframe_fisher_1per, {
        disableCustomization("fisher_1per")
        flag_types <- c("year", "period", "lobster_season", "conch_season", "finfish_season")
        lapply(flag_types, function(ft) set_flags_1per("fisher", ft, FALSE))
    })
    observeEvent(input$timeframe_fisher_multiper, {
        disableCustomization("fisher_multiper")
        flag_types <- c("year", "period", "lobster_season", "conch_season", "finfish_season")
        lapply(flag_types, function(ft) set_flags_multiper("fisher", ft, FALSE))
    })
    observeEvent(input$datatype_lamp_1per, {
        is_conch <- input$datatype_lamp_1per == "Conch"
        disableCustomization("lamp_1per")
        set_flags_1per("lamp", "period", FALSE)
        set_flags_1per("lamp", "year", FALSE)
        session$sendCustomMessage("triggerChangeLampImg", list(isConch = is_conch, isMulti = FALSE))
    })
    observeEvent(input$datatype_lamp_multiper, {
        is_conch <- input$datatype_lamp_multiper == "Conch"
        disableCustomization("lamp_multiper")
        set_flags_multiper("lamp", "year", FALSE)
        set_flags_multiper("lamp", "period", FALSE)
        session$sendCustomMessage("triggerChangeLampImg", list(isConch = is_conch, isMulti = TRUE))
    })
    observeEvent(input$datatype_lamp_1per, {
        is_conch <- input$datatype_lamp_1per == "Conch"
        disableCustomization("lamp_1per")
        set_flags_1per("lamp", "period", FALSE)
        set_flags_1per("lamp", "year", FALSE)
        session$sendCustomMessage("triggerChangeLampImg", list(isConch = is_conch, isMulti = FALSE))
    })
    observeEvent(input$datatype_lamp_multiper, {
        is_conch <- input$datatype_lamp_multiper == "Conch"
        disableCustomization("lamp_multiper")
        set_flags_multiper("lamp", "year", FALSE)
        set_flags_multiper("lamp", "period", FALSE)
        session$sendCustomMessage("triggerChangeLampImg", list(isConch = is_conch, isMulti = TRUE))
    })
    observeEvent(input$datatype_spag_1per, {
        is_visual <- input$datatype_spag_1per != "Laser"
        disableCustomization("spag_1per")
        session$sendCustomMessage("triggerChangeSpagImg", list(isVisual = is_visual, isMulti = FALSE))
    })
    observeEvent(input$datatype_spag_multiper, {
        is_visual <- input$datatype_spag_multiper != "Laser"
        disableCustomization("spag_multiper")
        session$sendCustomMessage("triggerChangeSpagImg", list(isVisual = is_visual, isMulti = TRUE))
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
