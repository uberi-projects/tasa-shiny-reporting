## ui.R

# Load packages ---------------------------
library(shiny)
library(rmarkdown)

# Source Objects ---------------------------
dummy_text <- paste(readLines("text/dummy.txt"))
home_text <- paste(readLines("text/home.txt"))

# Define ui ---------------------------
ui <- navbarPage(
    title = "Turneffe Reef-Monitoring Data Reporting Tool",
    header = div(
        includeCSS("www/modern-normalize.css"),
        includeCSS("www/styles.css"),
        class = "header", checked = NA,
        list(
            div(
                class = "header-container",
                div(class = "border-top"),
                div(class = "header-title", h1("Turneffe Reef-Monitoring Data Reporting Tool")),
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
                p(home_text[1]),
                br(),
                p(home_text[2])
            ),
            div(
                class = "content-box",
                h2("Using this App"),
                p(home_text[3]),
                br(),
                p(home_text[4]),
                br(),
                p(home_text[5])
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
                            "upload_catch_1per",
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
                        downloadButton("report_catch_1per", "Generate report")
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
                            "upload_lamp_1per",
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
                        downloadButton("report_lamp_1per", "Generate report")
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
                            "upload_spag_1per",
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
                        downloadButton("report_spag_1per", "Generate report")
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
