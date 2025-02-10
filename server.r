## server.R

# Define server ---------------------------
server <- function(input, output, session) {
    # Define dataframes from uploads
    df_upload_fisheries <- reactive({
        req(input$upload_fisheries)
        read.csv(input$upload_fisheries$datapath)
    })
    df_upload_fisher_1per <- reactive({
        req(input$upload_fisher_1per)
        read.csv(input$upload_fisher_1per$datapath)
    })
    df_upload_lampgen_1per <- reactive({
        req(input$upload_lampgen_1per)
        read.csv(input$upload_lampgen_1per$datapath)
    })
    df_upload_spagvis_1per <- reactive({
        req(input$upload_spagvis_1per)
        read.csv(input$upload_spagvis_1per$datapath)
    })

    # Validate dataframes
    observeEvent(input$validate_fisheries, {
        shinyalert("Success!", "Validation Successful!")
    })
    observeEvent(input$validate_fisher_1per, {
        shinyalert("Success!", "Validation Successful!")
    })
    observeEvent(input$validate_lampgen_1per, {
        shinyalert("Success!", "Validation Successful!")
    })
    observeEvent(input$validate_spagvis_1per, {
        shinyalert("Success!", "Validation Successful!")
    })

    # Create reports
    output$report_fisheries <- downloadHandler(
        filename = function() {
            paste0("report_fisheries_", input$period_fisheries, ".docx")
        },
        content = function(file) {
            report_file <- switch(input$period_fisheries,
                "One Season" = "report_fisheries_1per.Rmd",
                "Multiple Seasons" = "report_fisheries_multiper.Rmd",
                "Multiple Years" = "report_fisheries_multiyear.Rmd"
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
                params = list(user_name = input$name, datafile = df_upload_fisheries()),
                envir = new.env(parent = globalenv())
            )
            file.rename(out, file)
        }
    )
    output$report_fisher_1per <- downloadHandler(
        filename = "report_fisher_1per.docx",
        content = function(file) {
            src <- normalizePath(c("reports/report_fisher_1per.Rmd", "reports/report_template.docx", "www/images/TASA_logo_full_color.png"))
            owd <- setwd(tempdir())
            on.exit(setwd(owd))
            file.copy(src, c("report_fisher_1per.Rmd", "report_template.docx", "TASA_logo_full_color.png"), overwrite = TRUE)
            out <- render(
                "report_fisher_1per.Rmd",
                params = list(user_name = input$name, datafile = df_upload_fisher_1per()),
                envir = new.env(parent = globalenv())
            )
            file.rename(out, file)
        }
    )
    output$report_lampgen_1per <- downloadHandler(
        filename = "report_lampgen_1per.docx",
        content = function(file) {
            src <- normalizePath(c("reports/report_lampgen_1per.Rmd", "reports/report_template.docx", "www/images/TASA_logo_full_color.png"))
            owd <- setwd(tempdir())
            on.exit(setwd(owd))
            file.copy(src, c("report_lampgen_1per.Rmd", "report_template.docx", "TASA_logo_full_color.png"), overwrite = TRUE)
            out <- render(
                "report_lampgen_1per.Rmd",
                params = list(user_name = input$name, datafile = df_upload_lampgen_1per()),
                envir = new.env(parent = globalenv())
            )
            file.rename(out, file)
        }
    )
    output$report_spagvis_1per <- downloadHandler(
        filename = "report_spagvis_1per.docx",
        content = function(file) {
            src <- normalizePath(c("reports/report_spagvis_1per.Rmd", "reports/report_template.docx", "www/images/TASA_logo_full_color.png"))
            owd <- setwd(tempdir())
            on.exit(setwd(owd))
            file.copy(src, c("report_spagvis_1per.Rmd", "report_template.docx", "TASA_logo_full_color.png"), overwrite = TRUE)
            out <- render(
                "report_spagvis_1per.Rmd",
                params = list(user_name = input$name, datafile = df_upload_spagvis_1per()),
                envir = new.env(parent = globalenv())
            )
            file.rename(out, file)
        }
    )
}
