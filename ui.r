## ui.R

# Load packages ---------------------------
library(shiny)
library(shinyWidgets)
library(shinyalert)
library(rmarkdown)

# Source Objects ---------------------------
dummy_text <- paste(readLines("text/dummy.txt"))
home_text <- paste(readLines("text/home.txt"))
datatype_text <- paste(readLines("text/datatype.txt"))
link_text <- paste(readLines("text/links.txt"))

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
                div(a(href = link_text[1], target = "_blank", img(class = "header-logo", src = "images/TASA_logo_full_color.png", alt = "Logo")))
            )
        )
    ),
    tabPanel(
        "Home",
        div(
            class = "content-container-parent",
            div(
                class = "content-container-grid grid-home",
                div(
                    class = "content-box",
                    h2("Welcome"),
                    p(home_text[1]),
                    br(),
                    div(img(class = "pipeline", src = "images/pipeline.png", alt = "Pipeline from field to data to reporting")),
                    br(),
                    p(home_text[2]),
                    br(),
                    p(strong(home_text[3]))
                ),
                div(
                    class = "content-box",
                    h2("Using this App"),
                    tags$ol(
                        class = "content-list",
                        tags$li(home_text[4]),
                        tags$li(home_text[5]),
                        tags$li(home_text[6]),
                        tags$li(home_text[7]),
                        tags$li(home_text[8])
                    ),
                    br(),
                    p(strong(home_text[9]))
                )
            )
        )
    ),
    navbarMenu(
        "Create Reports",
        tabPanel(
            "Fisheries Catch",
            div(
                class = "content-container-parent",
                div(
                    class = "content-container-grid grid-report",
                    div(
                        class = "content-box",
                        h2("Fisheries Catch Reports"),
                        p(datatype_text[1])
                    ),
                    div(
                        class = "content-container-grid grid-generator",
                        div(
                            class = "input-box",
                            h3("Upload Data"),
                            tags$ol(
                                class = "content-list",
                                tags$li(
                                    class = "input-list",
                                    "Choose Time Period",
                                    div(
                                        class = "input-list-content",
                                        prettyRadioButtons("period_catch",
                                            label = NULL,
                                            choices = c("One Season", "One Year", "Multiple Seasons", "Multiple Years"), selected = "One Season", inline = TRUE
                                        )
                                    )
                                ),
                                hr(),
                                tags$li(
                                    class = "input-list",
                                    "Choose CSV File",
                                    div(
                                        class = "input-list-content",
                                        fileInput(
                                            "upload_catch_1per",
                                            label = NULL,
                                            multiple = FALSE,
                                            accept = c(
                                                "text/csv",
                                                "text/comma-separated-values,text/plain",
                                                ".csv"
                                            )
                                        )
                                    )
                                ),
                                hr(),
                                tags$li(
                                    class = "input-list",
                                    "Validate Data",
                                    div(
                                        class = "input-list-content",
                                        actionButton("validate_catch_1per", "Perform Validation")
                                    )
                                )
                            )
                        ),
                        div(
                            class = "input-box",
                            h3("Customize Report"),
                            tags$ol(
                                class = "content-list",
                                tags$li(
                                    class = "input-list",
                                    "Add Inputs",
                                    div(
                                        class = "input-list-content",
                                        textInput("name", "Your Name: ", value = "")
                                    )
                                ),
                                hr(),
                                tags$li(
                                    class = "input-list",
                                    "Generate Report",
                                    div(
                                        class = "input-list-content",
                                        downloadButton("report_catch_1per", "Generate")
                                    )
                                )
                            )
                        )
                    )
                )
            )
        ),
        tabPanel(
            "LAMP",
            div(
                class = "content-container-parent",
                div(
                    class = "content-container-grid grid-report",
                    div(
                        class = "content-box",
                        h2("LAMP Reports"),
                        p(datatype_text[2])
                    ),
                    div(
                        class = "content-container-grid grid-generator",
                        div(
                            class = "input-box",
                            h3("Upload Data"),
                            tags$ol(
                                class = "content-list",
                                tags$li(
                                    class = "input-list",
                                    "Choose Datatype",
                                    div(
                                        class = "input-list-content",
                                        prettyRadioButtons("datatype_lamp", label = NULL, choices = c("Conch", "General LAMP"), selected = "Conch", inline = TRUE)
                                    )
                                ),
                                hr(),
                                tags$li(
                                    class = "input-list",
                                    "Choose Time Period",
                                    div(
                                        class = "input-list-content",
                                        prettyRadioButtons("period_lamp",
                                            label = NULL,
                                            choices = c("One Period", "Multiple Periods"), selected = "One Period", inline = TRUE
                                        )
                                    )
                                ),
                                hr(),
                                tags$li(
                                    class = "input-list",
                                    "Choose CSV File",
                                    div(
                                        class = "input-list-content",
                                        fileInput(
                                            "upload_lamp_1per",
                                            label = NULL,
                                            multiple = FALSE,
                                            accept = c(
                                                "text/csv",
                                                "text/comma-separated-values,text/plain",
                                                ".csv"
                                            )
                                        )
                                    )
                                ),
                                hr(),
                                tags$li(
                                    class = "input-list",
                                    "Validate Data",
                                    div(
                                        class = "input-list-content",
                                        actionButton("validate_lamp_1per", "Perform Validation")
                                    )
                                )
                            )
                        ),
                        div(
                            class = "input-box",
                            h3("Customize Report"),
                            tags$ol(
                                class = "content-list",
                                tags$li(
                                    class = "input-list",
                                    "Add Inputs",
                                    div(
                                        class = "input-list-content",
                                        textInput("name", "Your Name: ", value = "")
                                    )
                                ),
                                hr(),
                                tags$li(
                                    class = "input-list",
                                    "Generate Report",
                                    div(
                                        class = "input-list-content",
                                        downloadButton("report_lamp_1per", "Generate")
                                    )
                                )
                            )
                        )
                    )
                )
            )
        ),
        tabPanel(
            "SPAG",
            div(
                class = "content-container-parent",
                div(
                    class = "content-container-grid grid-report",
                    div(
                        class = "content-box",
                        h2("SPAG Reports"),
                        p(datatype_text[3])
                    ),
                    div(
                        class = "content-container-grid grid-generator",
                        div(
                            class = "input-box",
                            h3("Upload Data"),
                            tags$ol(
                                class = "content-list",
                                tags$li(
                                    class = "input-list",
                                    "Choose Datatype",
                                    div(
                                        class = "input-list-content",
                                        prettyRadioButtons("datatype_spag", label = NULL, choices = c("Visual Census", "Laser Data"), selected = "Visual Census", inline = TRUE)
                                    )
                                ),
                                hr(),
                                tags$li(
                                    class = "input-list",
                                    "Choose Time Period",
                                    div(
                                        class = "input-list-content",
                                        prettyRadioButtons("period_spag", label = NULL, choices = c("One Year", "Multiple Years"), selected = "One Year", inline = TRUE)
                                    )
                                ),
                                hr(),
                                tags$li(
                                    class = "input-list",
                                    "Choose CSV File",
                                    div(
                                        class = "input-list-content",
                                        fileInput(
                                            "upload_spag_1per",
                                            label = NULL,
                                            multiple = FALSE,
                                            accept = c(
                                                "text/csv",
                                                "text/comma-separated-values,text/plain",
                                                ".csv"
                                            )
                                        )
                                    )
                                ),
                                hr(),
                                tags$li(
                                    class = "input-list",
                                    "Validate Data",
                                    div(
                                        class = "input-list-content",
                                        actionButton("validate_spag_1per", "Perform Validation")
                                    )
                                )
                            )
                        ),
                        div(
                            class = "input-box",
                            h3("Customize Report"),
                            tags$ol(
                                class = "content-list",
                                tags$li(
                                    class = "input-list",
                                    "Add Inputs",
                                    div(
                                        class = "input-list-content",
                                        textInput("name", "Your Name: ", value = "")
                                    )
                                ),
                                hr(),
                                tags$li(
                                    class = "input-list",
                                    "Generate Report",
                                    div(
                                        class = "input-list-content",
                                        downloadButton("report_spag_1per", "Generate")
                                    )
                                )
                            )
                        )
                    )
                )
            )
        )
    ),
    tabPanel(
        "Manual",
        div(
            class = "content-container-parent",
            div(
                class = "content-container-grid",
                div(
                    class = "content-box",
                    h2("Manual"),
                    p(dummy_text[4])
                )
            )
        )
    ),
    tabPanel(
        "Data Templates",
        div(
            class = "content-container-parent",
            div(
                class = "content-container-grid",
                div(
                    class = "content-box",
                    h2("Data Templates"),
                    p(dummy_text[3])
                )
            )
        )
    )
)
