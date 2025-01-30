## ui.R

# Load packages ---------------------------
library(shiny)
library(rmarkdown)

# Source Objects ---------------------------
dummy_text <- paste(readLines("text/dummy.txt"))

# Define ui ---------------------------
ui <- navbarPage(
    title = "Turneffe Reef-Monitoring Data Reporting Tool",
    header = div(
        includeCSS("www/modern-normalize.css"),
        includeCSS("www/styles.css"),
        class = "header", checked = NA,
        list(
            img(class = "background-image", src = "images/ocean.jpg"),
            div(class = "top-border fixed-header"),
            div(class = "header-title fixed-header", h1("Turneffe Reef-Monitoring Data Reporting Tool")),
            div(img(class = "header-logo fixed-header", src = "images/TASA_logo_full_color.png", alt = "Logo"))
        )
    ),
    tabPanel(
        "Home",
        div(
            class = "first-body-element content-container",
            div(
                class = "content-box",
                p()("Welcome"),
                p(dummy_text[1]),
                p(dummy_text[2])
            ),
            div(
                class = "content-box",
                p("Using this App"),
                p(dummy_text[3])
            )
        )
    ),
    navbarMenu(
        "Create Reports",
        tabPanel(
            "Fisheries Catch",
            div(
                class = "first-body-element content-container",
                div(
                    class = "content-box-fullpage",
                    p("Fisheries Catch Reports"),
                    p(dummy_text[1])
                )
            ),
            div(
                class = "content-container",
                div(
                    class = "generator-box",
                    div(
                        class = "input-box",
                        p("Upload Data")
                    ),
                    div(
                        class = "input-box",
                        textInput("name", "Your Name: ", value = ""),
                        downloadButton("report_test", "Generate report")
                    )
                )
            )
        ),
        tabPanel(
            "LAMP",
            div(
                class = "first-body-element content-container",
                div(
                    class = "content-box-fullpage",
                    p("LAMP Reports"),
                    p(dummy_text[2])
                )
            ),
            div(
                class = "content-container",
                div(
                    class = "generator-box",
                    div(
                        class = "input-box",
                        p("Upload Data")
                    ),
                    div(
                        class = "input-box",
                        textInput("name", "Your Name: ", value = ""),
                        downloadButton("report2", "Generate report")
                    )
                )
            )
        ),
        tabPanel(
            "SPAG",
            div(
                class = "first-body-element content-container",
                div(
                    class = "content-box-fullpage",
                    p("SPAG Reports"),
                    p(dummy_text[3])
                )
            ),
            div(
                class = "content-container",
                div(
                    class = "generator-box",
                    div(
                        class = "input-box",
                        p("Upload Data")
                    ),
                    div(
                        class = "input-box",
                        textInput("name", "Your Name: ", value = ""),
                        downloadButton("report3", "Generate report")
                    )
                )
            )
        )
    ),
    tabPanel(
        "Manual",
        div(
            class = "first-body-element content-container",
            div(
                class = "content-box-fullpage",
                p("Manual"),
                p(dummy_text[4])
            )
        )
    ),
    tabPanel(
        "Data Templates",
        div(
            class = "first-body-element content-container",
            div(
                class = "content-box-fullpage",
                p("Data Templates"),
                p(dummy_text[3])
            )
        )
    ),
    tabPanel(
        "Source Code",
        div(
            class = "first-body-element content-container",
            div(
                class = "content-box-fullpage",
                p("Source Code"),
                p(dummy_text[3])
            )
        )
    )
)
