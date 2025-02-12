## server.R
link_text <- paste(readLines("text/links.txt"))

# Define server ---------------------------
server <- function(input, output, session) {
    # Define dataframes from uploads
    df_upload_fisheries <- reactive({
        req(input$upload_fisheries)
        read.csv(input$upload_fisheries$datapath)
    })
    df_upload_fisher <- reactive({
        req(input$upload_fisher)
        read.csv(input$upload_fisher$datapath)
    })
    df_upload_lamp <- reactive({
        req(input$upload_lamp)
        read.csv(input$upload_lamp$datapath)
    })
    df_upload_spag <- reactive({
        req(input$upload_spag)
        read.csv(input$upload_spag$datapath)
    })

    # Validate dataframes
    observeEvent(input$validate_fisheries, {
        shinyalert("Success!", "Validation Successful!")
    })
    observeEvent(input$validate_fisher, {
        shinyalert("Success!", "Validation Successful!")
    })
    observeEvent(input$validate_lamp, {
        shinyalert("Success!", "Validation Successful!")
    })
    observeEvent(input$validate_spag, {
        shinyalert("Success!", "Validation Successful!")
    })

    # Create reports
    output$report_fisheries <- downloadHandler(
        filename = function() {
            report_file <- switch(input$period_fisheries,
                "One Season" = "report_fisheries_1per.Rmd",
                "Multiple Seasons" = "report_fisheries_multiper.Rmd",
                "Multiple Years" = "report_fisheries_multiyear.Rmd"
            )
            gsub(".Rmd", ".docx", report_file)
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

    output$report_fisher <- downloadHandler(
        filename = function() {
            report_file <- switch(input$period_fisher,
                "One Season" = "report_fisher_1per.Rmd",
                "Multiple Seasons" = "report_fisher_multiper.Rmd",
                "Multiple Years" = "report_fisher_multiyear.Rmd"
            )
            gsub(".Rmd", ".docx", report_file)
        },
        content = function(file) {
            report_file <- switch(input$period_fisher,
                "One Season" = "report_fisher_1per.Rmd",
                "Multiple Seasons" = "report_fisher_multiper.Rmd",
                "Multiple Years" = "report_fisher_multiyear.Rmd"
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
                params = list(user_name = input$name, datafile = df_upload_fisher()),
                envir = new.env(parent = globalenv())
            )
            file.rename(out, file)
        }
    )

    output$report_lamp <- downloadHandler(
        filename = function() {
            report_file <- switch(paste(input$datatype_lamp, input$period_lamp, sep = "_"),
                "Conch_One Period" = "report_lampconch_1per.Rmd",
                "Conch_Multiple Periods" = "report_lampconch_multiper.Rmd",
                "General LAMP_One Period" = "report_lampgen_1per.Rmd",
                "General LAMP_Multiple Periods" = "report_lampgen_multiper.Rmd"
            )
            gsub(".Rmd", ".docx", report_file)
        },
        content = function(file) {
            report_file <- switch(paste(input$datatype_lamp, input$period_lamp, sep = "_"),
                "Conch_One Period" = "report_lampconch_1per.Rmd",
                "Conch_Multiple Periods" = "report_lampconch_multiper.Rmd",
                "General LAMP_One Period" = "report_lampgen_1per.Rmd",
                "General LAMP_Multiple Periods" = "report_lampgen_multiper.Rmd"
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
                params = list(user_name = input$name, datafile = df_upload_lamp()),
                envir = new.env(parent = globalenv())
            )
            file.rename(out, file)
        }
    )

    output$report_spag <- downloadHandler(
        filename = function() {
            report_file <- switch(paste(input$datatype_spag, input$period_spag, sep = "_"),
                "Visual Census_One Year" = "report_spagvis_1per.Rmd",
                "Visual Census_Multiple Years" = "report_spagvis_multiper.Rmd",
                "Laser_One Year" = "report_spaglaser_1per.Rmd",
                "Laser_Multiple Years" = "report_spaglaser_multiper.Rmd"
            )
            gsub(".Rmd", ".docx", report_file)
        },
        content = function(file) {
            report_file <- switch(paste(input$datatype_spag, input$period_spag, sep = "_"),
                "Visual Census_One Year" = "report_spagvis_1per.Rmd",
                "Visual Census_Multiple Years" = "report_spagvis_multiper.Rmd",
                "Laser_One Year" = "report_spaglaser_1per.Rmd",
                "Laser_Multiple Years" = "report_spaglaser_multiper.Rmd"
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
                params = list(user_name = input$name, datafile = df_upload_spag()),
                envir = new.env(parent = globalenv())
            )
            file.rename(out, file)
        }
    )

    output$template_list_table <- renderUI({
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
