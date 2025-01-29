## server.R

# Define server ---------------------------
server <- function(input, output, session) {
    # Create demo report
    output$report_test <- downloadHandler(
        filename = "report.docx",
        content = function(file) {
            src <- normalizePath(c("report.Rmd", "report_template.docx", "www/images/TASA_logo.png"))

            owd <- setwd(tempdir())
            on.exit(setwd(owd))
            file.copy(src, c("report.Rmd", "report_template.docx", "TASA_logo.png"), overwrite = TRUE)

            out <- render(
                "report.Rmd",
                params = list(user_name = input$name),
                envir = new.env(parent = globalenv())
            )

            file.rename(out, file)
        }
    )
}
