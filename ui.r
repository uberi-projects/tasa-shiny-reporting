## ui.R

# Load packages ---------------------------
library(shiny)
library(rmarkdown)

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
    tabPanel("Home", tags$div(class = "content-box", tags$p("Welcome to Home"))),
    navbarMenu(
        "Create Reports",
        tabPanel(
            "Fisheries Catch",
            tags$div(
                class = "content-box",
                tags$p("Fisheries Catch Reports")
            )
        ),
        tabPanel(
            "LAMP",
            tags$div(
                class = "content-box",
                tags$p("LAMP Reports")
            )
        ),
        tabPanel(
            "SPAG",
            tags$div(
                class = "content-box",
                tags$p("SPAG Reports")
            )
        )
    ),
    tabPanel("Manual", tags$div(
        class = "content-box",
        tags$p("Manual")
    )),
    tabPanel("Data Templates", tags$div(
        class = "content-box",
        tags$p("Templates")
    )),
    tabPanel("Source Code", tags$div(
        class = "content-box",
        tags$p("Source Code")
    ))
)
