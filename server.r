## server.R

# Define server ---------------------------
server <- function(input, output, session) {
    # Upload datafile from system
    df <- reactive({
        req(input$catch_data)
        read.csv(input$catch_data$datapath)
    })

    # Create demo report
    output$report_test <- downloadHandler(
        filename = "report.docx",
        content = function(file) {
            src <- normalizePath(c("report.Rmd", "report_template.docx", "www/images/TASA_logo_full_color.png"))

            owd <- setwd(tempdir())
            on.exit(setwd(owd))
            file.copy(src, c("report.Rmd", "report_template.docx", "TASA_logo_full_color.png"), overwrite = TRUE)

            out <- render(
                "report.Rmd",
                params = list(user_name = input$name, datafile = df()),
                envir = new.env(parent = globalenv())
            )

            file.rename(out, file)
        }
    )
}
