## ui.R

# Load packages ---------------------------
library(shiny)
library(rmarkdown)

# Source Objects ---------------------------
dummy_text <- paste(readLines("text/dummy.txt"))

# Define ui ---------------------------
ui <- navbarPage(
    title = "New TASA Demo Title",
    header = div(
        includeCSS("www/modern-normalize.css"),
        includeCSS("www/styles.css"),
        class = "header", checked = NA,
        list(
            div(
                class = "header-container",
                div(class = "border-top"),
                div(class = "header-title", h1("New TASA Demo Title")),
                div(img(class = "header-logo", src = "images/TASA_logo_full_color.png", alt = "Logo"))
            )
        )
    ),
    tabPanel(
        "Home",
        div(
            class = "content-container",
            div(
                class = "content-box",
                h2("Welcome"),
                p(dummy_text[1]),
                p(dummy_text[2])
            ),
            div(
                class = "content-box",
                h2("Using this App"),
                p(dummy_text[3])
            )
        )
    ),
    navbarMenu(
        "Create Reports",
        tabPanel(
            "Fisheries Catch",
            div(
                class = "content-container",
                div(
                    class = "content-box-fullpage",
                    h2("Fisheries Catch Reports"),
                    p(dummy_text[1])
                ),
                div(
                    class = "content-container",
                    div(
                        class = "input-box",
                        h3("Upload Data"),
                        fileInput(
                            "catch_data",
                            "Choose CSV File",
                            multiple = FALSE,
                            accept = c(
                                "text/csv",
                                "text/comma-separated-values,text/plain",
                                ".csv"
                            )
                        )
                    ),
                    div(
                        class = "input-box",
                        h3("Customize Report"),
                        textInput("name", "Your Name: ", value = ""),
                        downloadButton("report_test", "Generate report")
                    )
                )
            )
        ),
        tabPanel(
            "LAMP",
            div(
                class = "content-container",
                div(
                    class = "content-box-fullpage",
                    h2("LAMP Reports"),
                    p(dummy_text[2])
                ),
                div(
                    class = "content-container",
                    div(
                        class = "input-box",
                        h3("Upload Data"),
                        fileInput(
                            "lamp_data",
                            "Choose CSV File",
                            multiple = FALSE,
                            accept = c(
                                "text/csv",
                                "text/comma-separated-values,text/plain",
                                ".csv"
                            )
                        )
                    ),
                    div(
                        class = "input-box",
                        h3("Customize Report"),
                        textInput("name", "Your Name: ", value = ""),
                        downloadButton("report2", "Generate report")
                    )
                )
            )
        ),
        tabPanel(
            "SPAG",
            div(
                class = "content-container",
                div(
                    class = "content-box-fullpage",
                    h2("SPAG Reports"),
                    p(dummy_text[3])
                ),
                div(
                    class = "content-container",
                    div(
                        class = "input-box",
                        h3("Upload Data"),
                        fileInput(
                            "spag_data",
                            "Choose CSV File",
                            multiple = FALSE,
                            accept = c(
                                "text/csv",
                                "text/comma-separated-values,text/plain",
                                ".csv"
                            )
                        )
                    ),
                    div(
                        class = "input-box",
                        h3("Customize Report"),
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
            class = "content-container",
            div(
                class = "content-box-fullpage",
                h2("Manual"),
                p(dummy_text[4])
            )
        )
    ),
    tabPanel(
        "Data Templates",
        div(
            class = "content-container",
            div(
                class = "content-box-fullpage",
                h2("Data Templates"),
                p(dummy_text[3])
            )
        )
    )
)
