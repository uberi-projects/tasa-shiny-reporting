## ui.R

# Load packages ---------------------------
library(shiny)
library(rmarkdown)

# Source Objects ---------------------------
dummy_text <- paste(readLines("text/dummy.txt"))

# Define ui ---------------------------
ui <- navbarPage(
    title = "Turneffe Reef-Monitoring Data Reporting Tool",
    header = tags$div(
        includeCSS("www/modern-normalize.css"),
        includeCSS("www/styles.css"),
        class = "header", checked = NA,
        list(
            tags$img(class = "background-image", src = "images/ocean.jpg"),
            tags$div(class = "top-border"),
            tags$div(class = "header-title", tags$h1("Turneffe Reef-Monitoring Data Reporting Tool")),
            tags$div(img(class = "header-logo", src = "images/TASA_logo_full_color.png", alt = "Logo"))
        )
    ),
    tabPanel(
        "Home",
        tags$div(
            class = "first-body-element content-box",
            p("Home Page Content"),
            p(dummy_text[1]),
            p(dummy_text[2])
        ),
        tags$div(
            class = "content-box",
            p(dummy_text[3])
        )
    ),
    navbarMenu(
        "Create Reports",
        tabPanel(
            "Fisheries Catch",
            tags$div(
                class = "first-body-element content-box",
                p("Fisheries Catch Reports"),
                p(dummy_text[1])
            ),
            tags$div(
                class = "generator-box",
                tags$div(
                    class = "input-box",
                    p("Upload Data")
                ),
                tags$div(
                    class = "input-box",
                    textInput("name", "Your Name: ", value = ""),
                    downloadButton("report_test", "Generate report")
                )
            )
        ),
        tabPanel(
            "LAMP",
            tags$div(
                class = "first-body-element content-box",
                p("LAMP Reports"),
                p(dummy_text[2])
            ),
            tags$div(
                class = "generator-box",
                tags$div(
                    class = "input-box",
                    p("Upload Data")
                ),
                tags$div(
                    class = "input-box",
                    textInput("name", "Your Name: ", value = ""),
                    downloadButton("report", "Generate report")
                )
            )
        ),
        tabPanel(
            "SPAG",
            tags$div(
                class = "first-body-element content-box",
                p("SPAG Reports"),
                p(dummy_text[3])
            ),
            tags$div(
                class = "generator-box",
                tags$div(
                    class = "input-box",
                    p("Upload Data")
                ),
                tags$div(
                    class = "input-box",
                    textInput("name", "Your Name: ", value = ""),
                    downloadButton("report", "Generate report")
                )
            )
        )
    ),
    tabPanel("Manual", tags$div(
        class = "first-body-element content-box",
        p("Manual"),
        p(dummy_text[4])
    )),
    tabPanel("Data Templates", tags$div(
        class = "first-body-element content-box",
        p("Templates"),
        p(dummy_text[1])
    )),
    tabPanel("Source Code", tags$div(
        class = "first-body-element content-box",
        p("Source Code"),
        p(dummy_text[2])
    ))
)
