## server.R

# Define server ---------------------------
server <- function(input, output, session) {
    # Define dataframes from uploads
    df_upload_catch_1per <- reactive({
        req(input$upload_catch_1per)
        read.csv(input$upload_catch_1per$datapath)
    })
    df_upload_lamp_1per <- reactive({
        req(input$upload_lamp_1per)
        read.csv(input$upload_lamp_1per$datapath)
    })
    df_upload_spag_1per <- reactive({
        req(input$upload_spag_1per)
        read.csv(input$upload_spag_1per$datapath)
    })

    # Create single period (1per) reports
    output$report_catch_1per <- downloadHandler(
        filename = "report_catch_1per.docx",
        content = function(file) {
            src <- normalizePath(c("reports/report_catch_1per.Rmd", "reports/report_template.docx", "www/images/TASA_logo_full_color.png"))
            owd <- setwd(tempdir())
            on.exit(setwd(owd))
            file.copy(src, c("report_catch_1per.Rmd", "report_template.docx", "TASA_logo_full_color.png"), overwrite = TRUE)
            out <- render(
                "report_catch_1per.Rmd",
                params = list(user_name = input$name, datafile = df_upload_catch_1per()),
                envir = new.env(parent = globalenv())
            )
            file.rename(out, file)
        }
    )
    output$report_lamp_1per <- downloadHandler(
        filename = "report_lamp_1per.docx",
        content = function(file) {
            src <- normalizePath(c("reports/report_lamp_1per.Rmd", "reports/report_template.docx", "www/images/TASA_logo_full_color.png"))
            owd <- setwd(tempdir())
            on.exit(setwd(owd))
            file.copy(src, c("report_lamp_1per.Rmd", "report_template.docx", "TASA_logo_full_color.png"), overwrite = TRUE)
            out <- render(
                "report_lamp_1per.Rmd",
                params = list(user_name = input$name, datafile = df_upload_lamp_1per()),
                envir = new.env(parent = globalenv())
            )
            file.rename(out, file)
        }
    )
    output$report_spag_1per <- downloadHandler(
        filename = "report_spag_1per.docx",
        content = function(file) {
            src <- normalizePath(c("reports/report_spag_1per.Rmd", "reports/report_template.docx", "www/images/TASA_logo_full_color.png"))
            owd <- setwd(tempdir())
            on.exit(setwd(owd))
            file.copy(src, c("report_spag_1per.Rmd", "report_template.docx", "TASA_logo_full_color.png"), overwrite = TRUE)
            out <- render(
                "report_spag_1per.Rmd",
                params = list(user_name = input$name, datafile = df_upload_spag_1per()),
                envir = new.env(parent = globalenv())
            )
            file.rename(out, file)
        }
    )
}
