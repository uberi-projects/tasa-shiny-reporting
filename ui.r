## ui.R

# Check for required packages ---------------------------
options(repos = c(CRAN = "https://cran.rstudio.com/"))
required_packages <- c(
    "shiny", "shinyjs", "shinyWidgets", "shinyalert", "rmarkdown", "knitr", "moments", "tidyverse",
    "readxl", "tidyverse", "ggpubr", "sf", "ggspatial", "ggnewscale", "officedown", "markdown", "zip"
)
install_if_missing <- function(package) {
    if (!requireNamespace(package, quietly = TRUE)) {
        install.packages(package)
    }
}
invisible(lapply(required_packages, install_if_missing))

# Attach packages ---------------------------
library(shiny)
library(shinyjs)
library(shinyWidgets)
library(shinyalert)
library(rmarkdown)
library(zip)
library(ggplot2)
library(ggpubr)
library(readxl)

# Source Objects ---------------------------
dummy_text <- paste(readLines("text/dummy.txt"))
home_text <- paste(readLines("text/home.txt"))
datatype_text <- paste(readLines("text/datatype.txt"))
link_text <- paste(readLines("text/links.txt"))
tab_text <- paste(readLines("text/tabinfo.txt"))

# Define ui ---------------------------
ui <- navbarPage(
    id = "navbar_page",
    title = "Turneffe Reef-Monitoring Data Reporting Tool",
    header = div(
        useShinyjs(),
        includeCSS("www/modern-normalize.css"),
        includeCSS("www/styles.css"),
        class = "header", checked = NA,
        list(
            div(
                class = "header-container",
                div(class = "border-top"),
                div(p(class = "version-header", "Version 1.3.0")),
                div(class = "header-title", h1("Turneffe Reef-Monitoring Data Reporting Tool")),
                span(a(href = link_text[1], target = "_blank", img(class = "header-logo", src = "images/TASA_logo.png", alt = "Logo"))),
                tags$svg(
                    class = "wave-decal",
                    viewBox = "0 0 283.5 30",
                    style = "width: 150%; height: auto;",
                    tags$path(
                        d = "M 283.5 9.7 c 0 0 -7.3 4.3 -14 4.6 c -6.8 0.3 -12.6 0 -20.9 -1.5 c -11.3 -2 -33.1 -10.1 -44.7 -5.7 s -12.1 4.6 -18 7.4 c -6.6 3.2 -20 9.6 -36.6 9.3 C 131.6 23.5 99.5 7.2 86.3 8 c -1.4 0.1 -6.6 0.8 -10.5 2 c -3.8 1.2 -9.4 3.8 -17 4.7 c -3.2 0.4 -8.3 1.1 -14.2 0.9 c -1.5 -0.1 -6.3 -0.4 -12 -1.6 c -5.7 -1.2 -11 -3.1 -15.8 -3.7 C 6.5 9.2 0 10.8 0 10.8 V 0 h 283.5 V 9.7 Z M 260.8 11.3 c -0.7 -1 -2 -0.4 -4.3 -0.4 c -2.3 0 -6.1 -1.2 -5.8 -1.1 c 0.3 0.1 3.1 1.5 6 1.9 C 259.7 12.2 261.4 12.3 260.8 11.3 Z M 242.4 8.6 c 0 0 -2.4 -0.2 -5.6 -0.9 c -3.2 -0.8 -10.3 -2.8 -15.1 -3.5 c -8.2 -1.1 -15.8 0 -15.1 0.1 c 0.8 0.1 9.6 -0.6 17.6 1.1 c 3.3 0.7 9.3 2.2 12.4 2.7 C 239.9 8.7 242.4 8.6 242.4 8.6 Z M 185.2 8.5 c 1.7 -0.7 -13.3 4.7 -18.5 6.1 c -2.1 0.6 -6.2 1.6 -10 2 c -3.9 0.4 -8.9 0.4 -8.8 0.5 c 0 0.2 5.8 0.8 11.2 0 c 5.4 -0.8 5.2 -1.1 7.6 -1.6 C 170.5 14.7 183.5 9.2 185.2 8.5 Z M 199.1 6.9 c 0.2 0 -0.8 -0.4 -4.8 1.1 c -4 1.5 -6.7 3.5 -6.9 3.7 c -0.2 0.1 3.5 -1.8 6.6 -3 C 197 7.5 199 6.9 199.1 6.9 Z M 283 6 c -0.1 0.1 -1.9 1.1 -4.8 2.5 s -6.9 2.8 -6.7 2.7 c 0.2 0 3.5 -0.6 7.4 -2.5 C 282.8 6.8 283.1 5.9 283 6 Z M 31.3 11.6 c 0.1 -0.2 -1.9 -0.2 -4.5 -1.2 s -5.4 -1.6 -7.8 -2 C 15 7.6 7.3 8.5 7.7 8.6 C 8 8.7 15.9 8.3 20.2 9.3 c 2.2 0.5 2.4 0.5 5.7 1.6 S 31.2 11.9 31.3 11.6 Z M 73 9.2 c 0.4 -0.1 3.5 -1.6 8.4 -2.6 c 4.9 -1.1 8.9 -0.5 8.9 -0.8 c 0 -0.3 -1 -0.9 -6.2 -0.3 S 72.6 9.3 73 9.2 Z M 71.6 6.7 C 71.8 6.8 75 5.4 77.3 5 c 2.3 -0.3 1.9 -0.5 1.9 -0.6 c 0 -0.1 -1.1 -0.2 -2.7 0.2 C 74.8 5.1 71.4 6.6 71.6 6.7 Z M 93.6 4.4 c 0.1 0.2 3.5 0.8 5.6 1.8 c 2.1 1 1.8 0.6 1.9 0.5 c 0.1 -0.1 -0.8 -0.8 -2.4 -1.3 C 97.1 4.8 93.5 4.2 93.6 4.4 Z M 65.4 11.1 c -0.1 0.3 0.3 0.5 1.9 -0.2 s 2.6 -1.3 2.2 -1.2 s -0.9 0.4 -2.5 0.8 C 65.3 10.9 65.5 10.8 65.4 11.1 Z M 34.5 12.4 c -0.2 0 2.1 0.8 3.3 0.9 c 1.2 0.1 2 0.1 2 -0.2 c 0 -0.3 -0.1 -0.5 -1.6 -0.4 C 36.6 12.8 34.7 12.4 34.5 12.4 Z M 152.2 21.1 c -0.1 0.1 -2.4 -0.3 -7.5 -0.3 c -5 0 -13.6 -2.4 -17.2 -3.5 c -3.6 -1.1 10 3.9 16.5 4.1 C 150.5 21.6 152.3 21 152.2 21.1 Z"
                    )
                )
            )
        ),
        tags$head(tags$script(src = "functions.js"))
    ),
    tabPanel(
        "Home",
        div(
            class = "content-container-parent",
            div(
                class = "content-container-grid",
                div(
                    class = "content-box",
                    h2("Welcome"),
                    p(home_text[1]),
                    br(),
                    br(),
                    h2("Using this App"),
                    tags$ol(
                        class = "content-list",
                        tags$li(home_text[4]),
                        tags$li(home_text[5]),
                        tags$li(home_text[6]),
                        tags$li(home_text[7]),
                        tags$li(home_text[8])
                    )
                ),
                img(class = "report-home-img", src = "images/photos/photo_lamp_lobster_05.jpeg")
            )
        )
    ),
    navbarMenu(
        "Create Reports",
        tabPanel(
            "Fisheries Catch",
            tabsetPanel(
                tabPanel(
                    "Single Period",
                    tags$script(
                        HTML(
                            "$(document).ready(function(){
                        $('#fisheries_1per_name').prop('disabled', true);
                        $('#validate_fisheries_1per').prop('disabled', true);
                    });
                    $(function () {
                        $('[data-toggle=tooltip]').tooltip({container: 'body'})
                    })"
                        )
                    ),
                    div( # Temporary cover for LAMP and SPAG reports.
                        style = "position: relative; left: -15px; top:195px;",
                        div(
                            class = "temporary-cover",
                            style = "
                                position: fixed;
                                height: 100vh;
                                width: 100vw;
                                background-color: rgba(200, 200, 200, 0.5);
                                z-index: 3;
                                margin-left: 0px;
                            ",
                            div(
                                style = "
                                    position: relative;
                                    width: 25%;
                                    height: 25%;
                                    background-color: rgba(255,255,255,0.6);
                                    margin: auto;
                                    top: 160px;
                                    border-radius: 10px;
                                    ",
                                h1(
                                    "Coming Soon!",
                                    style = "
                                    position: absolute;
                                    text-align: center;
                                    top: 50%;
                                    left: 50%;
                                    margin: 0;
                                    transform: translate(-50%, -50%);
                                    font-size: 2em;
                                    font-weight: 800;
                                    color: black;
                                    "
                                )
                            )
                        ),
                    ),
                    div(
                        class = "content-container-parent",
                        div(
                            class = "content-container-grid grid-report",
                            div(
                                class = "content-box report-content-box",
                                h2("Single Period Fisheries Catch Reports"),
                                p(datatype_text[1])
                            ),
                            div(
                                class = "content-container grid-generator",
                                div(
                                    class = "input-box",
                                    h3("Upload Data"),
                                    tags$ol(
                                        class = "content-list",
                                        tags$li(
                                            class = "input-list input-list-topbox",
                                            "Choose Datatype",
                                            div(
                                                class = "input-list-content",
                                                prettyRadioButtons("datatype_fisheries_1per", label = NULL, choices = c("Lobster", "Conch", "Finfish"), selected = "Lobster", inline = TRUE)
                                            )
                                        ),
                                        tags$li(
                                            class = "input-list input-list-topbox",
                                            "Choose Timeframe",
                                            div(
                                                class = "input-list-content",
                                                prettyRadioButtons("timeframe_fisheries_1per", label = NULL, choices = c("Seasonal", "Annual"), selected = "Seasonal", inline = TRUE)
                                            )
                                        ),
                                        tags$li(
                                            class = "input-list",
                                            "Choose File",
                                            span(
                                                `data-toggle` = "tooltip", `data-placement` = "right",
                                                title = home_text[11],
                                                icon("question-circle")
                                            ),
                                            div(
                                                class = "input-list-content",
                                                fileInput(
                                                    "upload_fisheries_1per",
                                                    label = NULL,
                                                    multiple = FALSE,
                                                    accept = c(
                                                        ".xls",
                                                        ".xlsx"
                                                    )
                                                ),
                                                uiOutput("ui_upload_fisheries_1per")
                                            )
                                        ),
                                        hr(),
                                        tags$li(
                                            class = "input-list",
                                            "Validate Data",
                                            span(
                                                `data-toggle` = "tooltip", `data-placement` = "right",
                                                title = home_text[10],
                                                icon("question-circle")
                                            ),
                                            div(
                                                class = "validation-box-cover",
                                                id = "fisheries_1per_validation_box_cover"
                                            ),
                                            div(
                                                class = "input-list-content",
                                                actionButton("validate_fisheries_1per", "Perform Validation")
                                            )
                                        )
                                    )
                                ),
                                div(
                                    class = "input-box",
                                    div(
                                        class = "input-box-cover",
                                        id = "fisheries_1per_input_box_cover"
                                    ),
                                    h3("Customize Report"),
                                    tags$ol(
                                        class = "content-list",
                                        tags$li(
                                            class = "input-list input-list-topbox",
                                            "Add Inputs",
                                            div(
                                                class = "input-list-content",
                                                div(
                                                    class = "input-block",
                                                    style = "display:flex;",
                                                    textInput("fisheries_1per_name", "Your Name: ", value = ""),
                                                    span(
                                                        class = "input-tooltip",
                                                        `data-toggle` = "tooltip", `data-placement` = "right",
                                                        title = home_text[12],
                                                        icon("question-circle")
                                                    ),
                                                )
                                            )
                                        ),
                                        hr(),
                                        tags$li(
                                            class = "input-list ",
                                            "Generate Outputs",
                                            div(
                                                class = "dl-bttn-container",
                                                div(
                                                    class = "input-list-content",
                                                    actionButton("report_fisheries_1per", "Download Report", icon = icon("download")),
                                                    downloadButton("report_fisheries_1per_hidden", "", class = "hidden_download")
                                                ),
                                                div(
                                                    class = "input-list-content",
                                                    actionButton("figures_fisheries_1per", "Download Figures", icon = icon("download")),
                                                    downloadButton("figures_fisheries_1per_hidden", "", class = "hidden_download")
                                                )
                                            ),
                                            div(
                                                id = "fisheries_1per_loader",
                                                class = "custom-loader",
                                                style = "display:none;",
                                                div(
                                                    id = "fisheries_1per_loader_bar",
                                                    class = "custom-loader-bar"
                                                )
                                            ),
                                            div(
                                                class = "input-box-cover",
                                                id = "fisheries_1per_generate_cover"
                                            )
                                        ),
                                        img(src = "images/photos/photo_lamp_lobster_05.jpeg", id = "datatype-fisheries-1per-img", class = "datatype-img")
                                    )
                                )
                            )
                        )
                    )
                ),
                tabPanel(
                    "Multiple Periods",
                    tags$script(
                        HTML(
                            "$(document).ready(function(){
                        $('#fisheries_multiper_name').prop('disabled', true);
                        $('#validate_fisheries_multiper').prop('disabled', true);
                        $('#upload_fisheries_multiper2').prop('disabled', true);
                        $('#upload_fisheries_multiper3').prop('disabled', true);
                        $('#upload_fisheries_multiper4').prop('disabled', true);
                    });
                    $(function () {
                        $('[data-toggle=tooltip]').tooltip({container: 'body'})
                    })"
                        )
                    ),
                    div( # Temporary cover for LAMP and SPAG reports.
                        style = "position: relative; left: -15px; top:195px;",
                        div(
                            class = "temporary-cover",
                            style = "
                                position: fixed;
                                height: 100vh;
                                width: 100vw;
                                background-color: rgba(200, 200, 200, 0.5);
                                z-index: 3;
                                margin-left: 0px;
                            ",
                            div(
                                style = "
                                    position: relative;
                                    width: 25%;
                                    height: 25%;
                                    background-color: rgba(255,255,255,0.6);
                                    margin: auto;
                                    top: 160px;
                                    border-radius: 10px;
                                    ",
                                h1(
                                    "Coming Soon!",
                                    style = "
                                    position: absolute;
                                    text-align: center;
                                    top: 50%;
                                    left: 50%;
                                    margin: 0;
                                    transform: translate(-50%, -50%);
                                    font-size: 2em;
                                    font-weight: 800;
                                    color: black;
                                    "
                                )
                            )
                        ),
                    ),
                    div(
                        class = "content-container-parent",
                        div(
                            class = "content-container-grid grid-report",
                            div(
                                class = "content-box report-content-box",
                                h2("Multi-Period Fisheries Catch Reports"),
                                p(datatype_text[1])
                            ),
                            div(
                                class = "content-container grid-generator",
                                div(
                                    class = "input-box",
                                    h3(
                                        "Upload Data"
                                    ),
                                    tags$ol(
                                        class = "content-list",
                                        tags$li(
                                            class = "input-list input-list-topbox",
                                            "Choose Datatype",
                                            div(
                                                class = "input-list-content",
                                                prettyRadioButtons("datatype_fisheries_multiper", label = NULL, choices = c("Lobster", "Conch", "Finfish"), selected = "Lobster", inline = TRUE)
                                            )
                                        ),
                                        tags$li(
                                            class = "input-list input-list-topbox",
                                            "Choose Timeframe",
                                            div(
                                                class = "input-list-content",
                                                prettyRadioButtons("timeframe_fisheries_multiper", label = NULL, choices = c("Seasonal", "Annual"), selected = "Seasonal", inline = TRUE)
                                            )
                                        ),
                                        tags$li(
                                            class = "input-list",
                                            "Choose File 1",
                                            span(
                                                `data-toggle` = "tooltip", `data-placement` = "right",
                                                title = home_text[11],
                                                icon("question-circle")
                                            ),
                                            div(
                                                class = "input-list-content",
                                                fileInput(
                                                    "upload_fisheries_multiper1",
                                                    label = NULL,
                                                    multiple = FALSE,
                                                    accept = c(
                                                        ".xls",
                                                        ".xlsx"
                                                    )
                                                ),
                                                uiOutput("ui_upload_fisheries_multiper1")
                                            )
                                        ),
                                        tags$li(
                                            class = "input-list",
                                            "Choose File 2",
                                            div(
                                                class = "input-list-content",
                                                fileInput(
                                                    "upload_fisheries_multiper2",
                                                    label = NULL,
                                                    multiple = FALSE,
                                                    accept = c(
                                                        ".xls",
                                                        ".xlsx"
                                                    )
                                                ),
                                                uiOutput("ui_upload_fisheries_multiper2")
                                            ),
                                            div(
                                                class = "validation-box-cover",
                                                id = "fisheries_multiper2_upload_box_cover"
                                            )
                                        ),
                                        tags$li(
                                            class = "input-list",
                                            "Choose File 3 (Optional)",
                                            div(
                                                class = "input-list-content",
                                                fileInput(
                                                    "upload_fisheries_multiper3",
                                                    label = NULL,
                                                    multiple = FALSE,
                                                    accept = c(
                                                        ".xls",
                                                        ".xlsx"
                                                    )
                                                ),
                                                uiOutput("ui_upload_fisheries_multiper3"),
                                                actionButton(
                                                    class = "remove-file-bttn",
                                                    style = "display:none;",
                                                    "remove_fisheries_multiper3_bttn",
                                                    "X"
                                                )
                                            ),
                                            div(
                                                class = "validation-box-cover",
                                                id = "fisheries_multiper3_upload_box_cover"
                                            )
                                        ),
                                        tags$li(
                                            class = "input-list",
                                            "Choose File 4 (Optional)",
                                            div(
                                                class = "input-list-content",
                                                fileInput(
                                                    "upload_fisheries_multiper4",
                                                    label = NULL,
                                                    multiple = FALSE,
                                                    accept = c(
                                                        ".xls",
                                                        ".xlsx"
                                                    )
                                                ),
                                                uiOutput("ui_upload_fisheries_multiper4"),
                                                actionButton(
                                                    class = "remove-file-bttn",
                                                    style = "display:none;",
                                                    "remove_fisheries_multiper4_bttn",
                                                    "X"
                                                )
                                            ),
                                            div(
                                                class = "validation-box-cover",
                                                id = "fisheries_multiper4_upload_box_cover"
                                            )
                                        ),
                                        hr(),
                                        tags$li(
                                            class = "input-list",
                                            "Validate Data",
                                            span(
                                                `data-toggle` = "tooltip", `data-placement` = "right",
                                                title = home_text[10],
                                                icon("question-circle")
                                            ),
                                            div(
                                                class = "validation-box-cover",
                                                id = "fisheries_multiper_validation_box_cover"
                                            ),
                                            div(
                                                class = "input-list-content",
                                                actionButton("validate_fisheries_multiper", "Perform Validation")
                                            )
                                        )
                                    )
                                ),
                                div(
                                    class = "input-box",
                                    div(
                                        class = "input-box-cover",
                                        id = "fisheries_multiper_input_box_cover"
                                    ),
                                    h3("Customize Report"),
                                    tags$ol(
                                        class = "content-list",
                                        tags$li(
                                            class = "input-list input-list-topbox",
                                            "Add Inputs",
                                            div(
                                                class = "input-list-content",
                                                div(
                                                    class = "input-block",
                                                    style = "display:flex;",
                                                    textInput("fisheries_multiper_name", "Your Name: ", value = ""),
                                                    span(
                                                        class = "input-tooltip",
                                                        `data-toggle` = "tooltip", `data-placement` = "right",
                                                        title = home_text[12],
                                                        icon("question-circle")
                                                    ),
                                                )
                                            )
                                        ),
                                        hr(),
                                        tags$li(
                                            class = "input-list ",
                                            "Generate Outputs",
                                            div(
                                                class = "input-list-content",
                                                actionButton("report_fisheries_multiper", "Download Report", icon = icon("download")),
                                                downloadButton("report_fisheries_multiper_hidden", "", class = "hidden_download")
                                            ),
                                            div(
                                                id = "fisheries_multiper_loader",
                                                class = "custom-loader",
                                                style = "display:none;",
                                                div(
                                                    id = "fisheries_multiper_loader_bar",
                                                    class = "custom-loader-bar"
                                                )
                                            ),
                                            div(
                                                class = "input-box-cover",
                                                id = "fisheries_multiper_generate_cover"
                                            )
                                        ),
                                        img(src = "images/photos/photo_lamp_lobster_05.jpeg", id = "datatype-fisheries-multi-img", class = "datatype-img")
                                    )
                                )
                            )
                        )
                    )
                )
            )
        ),
        tabPanel(
            "Fisher Catch",
            tabsetPanel(
                tabPanel(
                    "Single Period",
                    tags$script(
                        HTML(
                            "$(document).ready(function(){
                        $('#fisher_1per_name').prop('disabled', true);
                        $('#validate_fisher_1per').prop('disabled', true);
                    });
                    "
                        )
                    ),
                    div(
                        class = "content-container-parent",
                        div(
                            class = "content-container-grid grid-report",
                            div(
                                class = "content-box report-content-box",
                                h2("Single Period Fisher Catch Project Reports"),
                                p(tab_text[6])
                            ),
                            div(
                                class = "content-container grid-generator",
                                div(
                                    class = "input-box",
                                    h3(
                                        "Upload Data"
                                    ),
                                    tags$ol(
                                        class = "content-list",
                                        tags$li(
                                            class = "input-list input-list-topbox",
                                            "Choose Datatype",
                                            div(
                                                class = "input-list-content",
                                                prettyRadioButtons("datatype_fisher_1per", label = NULL, choices = c("Lobster", "Conch", "Finfish"), selected = "Lobster", inline = TRUE)
                                            )
                                        ),
                                        tags$li(
                                            class = "input-list input-list-topbox",
                                            "Choose Timeframe",
                                            div(
                                                class = "input-list-content",
                                                prettyRadioButtons("timeframe_fisher_1per", label = NULL, choices = c("Seasonal", "Annual"), selected = "Seasonal", inline = TRUE)
                                            )
                                        ),
                                        tags$li(
                                            class = "input-list",
                                            "Choose File",
                                            span(
                                                `data-toggle` = "tooltip", `data-placement` = "right",
                                                title = home_text[11],
                                                icon("question-circle")
                                            ),
                                            div(
                                                class = "input-list-content",
                                                fileInput(
                                                    "upload_fisher_1per",
                                                    label = NULL,
                                                    multiple = FALSE,
                                                    accept = c(
                                                        ".xls",
                                                        ".xlsx"
                                                    )
                                                ),
                                                uiOutput("ui_upload_fisher_1per")
                                            )
                                        ),
                                        hr(),
                                        tags$li(
                                            class = "input-list",
                                            "Validate Data",
                                            span(
                                                `data-toggle` = "tooltip", `data-placement` = "right",
                                                title = home_text[10],
                                                icon("question-circle")
                                            ),
                                            div(
                                                class = "validation-box-cover",
                                                id = "fisher_1per_validation_box_cover"
                                            ),
                                            div(
                                                class = "input-list-content",
                                                actionButton("validate_fisher_1per", "Perform Validation")
                                            )
                                        )
                                    )
                                ),
                                div(
                                    class = "input-box",
                                    div(
                                        class = "input-box-cover",
                                        id = "fisher_1per_input_box_cover"
                                    ),
                                    h3("Customize Report"),
                                    tags$ol(
                                        class = "content-list",
                                        tags$li(
                                            class = "input-list input-list-topbox",
                                            "Add Inputs",
                                            div(
                                                class = "input-list-content",
                                                div(
                                                    class = "input-block",
                                                    style = "display:flex;",
                                                    textInput("fisher_1per_name", "Your Name: ", value = ""),
                                                    span(
                                                        class = "input-tooltip",
                                                        `data-toggle` = "tooltip", `data-placement` = "right",
                                                        title = home_text[11],
                                                        icon("question-circle")
                                                    ),
                                                )
                                            )
                                        ),
                                        hr(),
                                        tags$li(
                                            class = "input-list",
                                            "Generate Outputs",
                                            div(
                                                class = "dl-bttn-container",
                                                div(
                                                    class = "input-list-content",
                                                    actionButton("report_fisher_1per", "Download Report", icon = icon("download")),
                                                    downloadButton("report_fisher_1per_hidden", "", class = "hidden_download")
                                                ),
                                                div(
                                                    class = "input-list-content",
                                                    actionButton("figures_fisher_1per", "Download Figures", icon = icon("download")),
                                                    downloadButton("figures_fisher_1per_hidden", "", class = "hidden_download")
                                                )
                                            ),
                                            div(
                                                id = "fisher_1per_loader",
                                                class = "custom-loader",
                                                style = "display:none;",
                                                div(
                                                    id = "fisher_1per_loader_bar",
                                                    class = "custom-loader-bar"
                                                )
                                            ),
                                            div(
                                                class = "input-box-cover",
                                                id = "fisher_1per_generate_cover"
                                            )
                                        ),
                                        img(src = "images/photos/photo_catch_03.jpg", id = "datatype-fisher-1per-img", class = "datatype-img")
                                    )
                                )
                            )
                        )
                    )
                ),
                tabPanel(
                    "Multiple Periods",
                    tags$script(
                        HTML(
                            "$(document).ready(function(){
                        $('#fisher_multiper_name').prop('disabled', true);
                        $('#validate_fisher_multiper').prop('disabled', true);
                        $('#upload_fisher_multiper2').prop('disabled', true);
                        $('#upload_fisher_multiper3').prop('disabled', true);
                        $('#upload_fisher_multiper4').prop('disabled', true);
                    });
                    "
                        )
                    ),
                    div( # Temporary cover for LAMP and SPAG reports.
                        style = "position: relative; left: -15px; top:195px;",
                        div(
                            class = "temporary-cover",
                            style = "
                                position: fixed;
                                height: 100vh;
                                width: 100vw;
                                background-color: rgba(200, 200, 200, 0.5);
                                z-index: 3;
                                margin-left: 0px;
                            ",
                            div(
                                style = "
                                    position: relative;
                                    width: 25%;
                                    height: 25%;
                                    background-color: rgba(255,255,255,0.6);
                                    margin: auto;
                                    top: 160px;
                                    border-radius: 10px;
                                    ",
                                h1(
                                    "Coming Soon!",
                                    style = "
                                    position: absolute;
                                    text-align: center;
                                    top: 50%;
                                    left: 50%;
                                    margin: 0;
                                    transform: translate(-50%, -50%);
                                    font-size: 2em;
                                    font-weight: 800;
                                    color: black;
                                    "
                                )
                            )
                        ),
                    ),
                    div(
                        class = "content-container-parent",
                        div(
                            class = "content-container-grid grid-report",
                            div(
                                class = "content-box report-content-box",
                                h2("Multi-Period Fisher Catch Project Reports"),
                                p(tab_text[6])
                            ),
                            div(
                                class = "content-container grid-generator",
                                div(
                                    class = "input-box",
                                    h3(
                                        "Upload Data"
                                    ),
                                    tags$ol(
                                        class = "content-list",
                                        tags$li(
                                            class = "input-list input-list-topbox",
                                            "Choose Datatype",
                                            div(
                                                class = "input-list-content",
                                                prettyRadioButtons("datatype_fisher_multiper", label = NULL, choices = c("Lobster", "Conch", "Finfish"), selected = "Lobster", inline = TRUE)
                                            )
                                        ),
                                        tags$li(
                                            class = "input-list input-list-topbox",
                                            "Choose Timeframe",
                                            div(
                                                class = "input-list-content",
                                                prettyRadioButtons("timeframe_fisher_multiper", label = NULL, choices = c("Seasonal", "Annual"), selected = "Seasonal", inline = TRUE)
                                            )
                                        ),
                                        tags$li(
                                            class = "input-list",
                                            "Choose File 1",
                                            span(
                                                `data-toggle` = "tooltip", `data-placement` = "right",
                                                title = home_text[11],
                                                icon("question-circle")
                                            ),
                                            div(
                                                class = "input-list-content",
                                                fileInput(
                                                    "upload_fisher_multiper1",
                                                    label = NULL,
                                                    multiple = FALSE,
                                                    accept = c(
                                                        ".xls",
                                                        ".xlsx"
                                                    )
                                                ),
                                                uiOutput("ui_upload_fisher_multiper1")
                                            )
                                        ),
                                        tags$li(
                                            class = "input-list",
                                            "Choose File 2",
                                            div(
                                                class = "input-list-content",
                                                fileInput(
                                                    "upload_fisher_multiper2",
                                                    label = NULL,
                                                    multiple = FALSE,
                                                    accept = c(
                                                        ".xls",
                                                        ".xlsx"
                                                    )
                                                ),
                                                uiOutput("ui_upload_fisher_multiper2")
                                            ),
                                            div(
                                                class = "validation-box-cover",
                                                id = "fisher_multiper2_upload_box_cover"
                                            )
                                        ),
                                        tags$li(
                                            class = "input-list",
                                            "Choose File 3 (Optional)",
                                            div(
                                                class = "input-list-content",
                                                fileInput(
                                                    "upload_fisher_multiper3",
                                                    label = NULL,
                                                    multiple = FALSE,
                                                    accept = c(
                                                        ".xls",
                                                        ".xlsx"
                                                    )
                                                ),
                                                uiOutput("ui_upload_fisher_multiper3"),
                                                actionButton(
                                                    class = "remove-file-bttn",
                                                    style = "display:none;",
                                                    "remove_fisher_multiper3_bttn",
                                                    "X"
                                                )
                                            ),
                                            div(
                                                class = "validation-box-cover",
                                                id = "fisher_multiper3_upload_box_cover"
                                            )
                                        ),
                                        tags$li(
                                            class = "input-list",
                                            "Choose File 4 (Optional)",
                                            div(
                                                class = "input-list-content",
                                                fileInput(
                                                    "upload_fisher_multiper4",
                                                    label = NULL,
                                                    multiple = FALSE,
                                                    accept = c(
                                                        ".xls",
                                                        ".xlsx"
                                                    )
                                                ),
                                                uiOutput("ui_upload_fisher_multiper4"),
                                                actionButton(
                                                    class = "remove-file-bttn",
                                                    style = "display:none;",
                                                    "remove_fisher_multiper4_bttn",
                                                    "X"
                                                )
                                            ),
                                            div(
                                                class = "validation-box-cover",
                                                id = "fisher_multiper4_upload_box_cover"
                                            )
                                        ),
                                        hr(),
                                        tags$li(
                                            class = "input-list",
                                            "Validate Data",
                                            span(
                                                `data-toggle` = "tooltip", `data-placement` = "right",
                                                title = home_text[10],
                                                icon("question-circle")
                                            ),
                                            div(
                                                class = "validation-box-cover",
                                                id = "fisher_multiper_validation_box_cover"
                                            ),
                                            div(
                                                class = "input-list-content",
                                                actionButton("validate_fisher_multiper", "Perform Validation")
                                            )
                                        )
                                    )
                                ),
                                div(
                                    class = "input-box",
                                    div(
                                        class = "input-box-cover",
                                        id = "fisher_multiper_input_box_cover"
                                    ),
                                    h3("Customize Report"),
                                    tags$ol(
                                        class = "content-list",
                                        tags$li(
                                            class = "input-list input-list-topbox",
                                            "Add Inputs",
                                            div(
                                                class = "input-list-content",
                                                div(
                                                    class = "input-block",
                                                    style = "display:flex;",
                                                    textInput("fisher_multiper_name", "Your Name: ", value = ""),
                                                    span(
                                                        class = "input-tooltip",
                                                        `data-toggle` = "tooltip", `data-placement` = "right",
                                                        title = home_text[12],
                                                        icon("question-circle")
                                                    ),
                                                )
                                            )
                                        ),
                                        hr(),
                                        tags$li(
                                            class = "input-list",
                                            "Generate Outputs",
                                            div(
                                                class = "input-list-content",
                                                actionButton("report_fisher_multiper", "Download Report", icon = icon("download")),
                                                downloadButton("report_fisher_multiper_hidden", "", class = "hidden_download")
                                            ),
                                            div(
                                                id = "fisher_multiper_loader",
                                                class = "custom-loader",
                                                style = "display:none;",
                                                div(
                                                    id = "fisher_multiper_loader_bar",
                                                    class = "custom-loader-bar"
                                                )
                                            ),
                                            div(
                                                class = "input-box-cover",
                                                id = "fisher_multiper_generate_cover"
                                            )
                                        ),
                                        img(src = "images/photos/photo_catch_03.jpg", id = "datatype-fisher-multi-img", class = "datatype-img")
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
            tabsetPanel(
                tabPanel(
                    "Single Period",
                    tags$script(
                        HTML(
                            "$(document).ready(function(){
                                $('#lamp_1per_name').prop('disabled', true);
                                $('#validate_lamp_1per').prop('disabled', true);
                            });"
                        )
                    ),
                    div(
                        class = "content-container-parent",
                        div(
                            class = "content-container-grid grid-report",
                            div(
                                class = "content-box report-content-box",
                                h2("Single Period LAMP Reports"),
                                p(datatype_text[2])
                            ),
                            div(
                                class = "content-container grid-generator",
                                div(
                                    class = "input-box",
                                    h3(
                                        "Upload Data"
                                    ),
                                    tags$ol(
                                        class = "content-list",
                                        tags$li(
                                            class = "input-list input-list-topbox",
                                            "Choose Datatype",
                                            div(
                                                class = "input-list-content",
                                                prettyRadioButtons("datatype_lamp_1per", label = NULL, choices = c("Conch", "General LAMP"), selected = "Conch", inline = TRUE)
                                            )
                                        ),
                                        hr(),
                                        tags$li(
                                            class = "input-list",
                                            "Choose File",
                                            span(
                                                `data-toggle` = "tooltip", `data-placement` = "right",
                                                title = home_text[11],
                                                icon("question-circle")
                                            ),
                                            div(
                                                class = "input-list-content",
                                                fileInput(
                                                    "upload_lamp_1per",
                                                    label = NULL,
                                                    multiple = FALSE,
                                                    accept = c(
                                                        ".xls",
                                                        ".xlsx"
                                                    )
                                                ),
                                                uiOutput("ui_upload_lamp_1per")
                                            )
                                        ),
                                        hr(),
                                        tags$li(
                                            class = "input-list",
                                            "Validate Data",
                                            span(
                                                `data-toggle` = "tooltip", `data-placement` = "right",
                                                title = home_text[10],
                                                icon("question-circle")
                                            ),
                                            div(
                                                class = "validation-box-cover",
                                                id = "lamp_1per_validation_box_cover"
                                            ),
                                            div(
                                                class = "input-list-content",
                                                actionButton("validate_lamp_1per", "Perform Validation")
                                            )
                                        )
                                    )
                                ),
                                div(
                                    class = "input-box",
                                    div(
                                        class = "input-box-cover",
                                        id = "lamp_1per_input_box_cover"
                                    ),
                                    h3("Customize Report"),
                                    tags$ol(
                                        class = "content-list",
                                        tags$li(
                                            class = "input-list input-list-topbox",
                                            "Add Inputs",
                                            div(
                                                class = "input-list-content",
                                                div(
                                                    class = "input-block",
                                                    style = "display:flex;",
                                                    textInput("lamp_1per_name", "Your Name: ", value = ""),
                                                    span(
                                                        class = "input-tooltip",
                                                        `data-toggle` = "tooltip", `data-placement` = "right",
                                                        title = home_text[12],
                                                        icon("question-circle")
                                                    ),
                                                )
                                            )
                                        ),
                                        hr(),
                                        tags$li(
                                            class = "input-list",
                                            "Generate Outputs",
                                            div(
                                                class = "dl-bttn-container",
                                                div(
                                                    class = "input-list-content",
                                                    actionButton("report_lamp_1per", "Download Report", icon = icon("download")),
                                                    downloadButton("report_lamp_1per_hidden", "", class = "hidden_download")
                                                ),
                                                div(
                                                    class = "input-list-content",
                                                    actionButton("figures_lamp_1per", "Download Figures", icon = icon("download")),
                                                    downloadButton("figures_lamp_1per_hidden", "", class = "hidden_download")
                                                )
                                            ),
                                            div(
                                                id = "lamp_1per_loader",
                                                class = "custom-loader",
                                                style = "display:none;",
                                                div(
                                                    id = "lamp_1per_loader_bar",
                                                    class = "custom-loader-bar"
                                                )
                                            ),
                                            div(
                                                class = "input-box-cover",
                                                id = "lamp_1per_generate_cover"
                                            )
                                        ),
                                        img(src = "images/photos/photo_lamp_conch_03.jpeg", id = "datatype-lamp-1per-img", class = "datatype-img")
                                    )
                                )
                            )
                        )
                    )
                ),
                tabPanel(
                    "Multiple Periods",
                    tags$script(
                        HTML(
                            "$(document).ready(function(){
                                $('#lamp_multiper_name').prop('disabled', true);
                                $('#validate_lamp_multiper').prop('disabled', true);
                                $('#upload_lamp_multiper2').prop('disabled', true);
                                $('#upload_lamp_multiper3').prop('disabled', true);
                                $('#upload_lamp_multiper4').prop('disabled', true);
                            });"
                        )
                    ),
                    div(
                        class = "content-container-parent",
                        div(
                            class = "content-container-grid grid-report",
                            div(
                                class = "content-box report-content-box",
                                h2("Multi-Period LAMP Reports"),
                                p(datatype_text[2])
                            ),
                            div(
                                class = "content-container grid-generator",
                                div(
                                    class = "input-box",
                                    h3("Upload Data"),
                                    tags$ol(
                                        class = "content-list",
                                        tags$li(
                                            class = "input-list input-list-topbox",
                                            "Choose Datatype",
                                            div(
                                                class = "input-list-content",
                                                prettyRadioButtons("datatype_lamp_multiper", label = NULL, choices = c("Conch", "General LAMP"), selected = "Conch", inline = TRUE)
                                            )
                                        ),
                                        hr(),
                                        tags$li(
                                            class = "input-list",
                                            "Choose File 1",
                                            span(
                                                `data-toggle` = "tooltip", `data-placement` = "right",
                                                title = home_text[11],
                                                icon("question-circle")
                                            ),
                                            div(
                                                class = "input-list-content",
                                                fileInput(
                                                    "upload_lamp_multiper1",
                                                    label = NULL,
                                                    multiple = FALSE,
                                                    accept = c(
                                                        ".xls",
                                                        ".xlsx"
                                                    )
                                                ),
                                                uiOutput("ui_upload_lamp_multiper1")
                                            )
                                        ),
                                        tags$li(
                                            class = "input-list",
                                            "Choose File 2",
                                            div(
                                                class = "input-list-content",
                                                fileInput(
                                                    "upload_lamp_multiper2",
                                                    label = NULL,
                                                    multiple = FALSE,
                                                    accept = c(
                                                        ".xls",
                                                        ".xlsx"
                                                    )
                                                ),
                                                uiOutput("ui_upload_lamp_multiper2")
                                            ),
                                            div(
                                                class = "validation-box-cover",
                                                id = "lamp_multiper2_upload_box_cover"
                                            )
                                        ),
                                        tags$li(
                                            class = "input-list",
                                            "Choose File 3 (Optional)",
                                            div(
                                                class = "input-list-content",
                                                fileInput(
                                                    "upload_lamp_multiper3",
                                                    label = NULL,
                                                    multiple = FALSE,
                                                    accept = c(
                                                        ".xls",
                                                        ".xlsx"
                                                    )
                                                ),
                                                uiOutput("ui_upload_lamp_multiper3"),
                                                actionButton(
                                                    class = "remove-file-bttn",
                                                    style = "display:none;",
                                                    "remove_lamp_multiper3_bttn",
                                                    "X"
                                                )
                                            ),
                                            div(
                                                class = "validation-box-cover",
                                                id = "lamp_multiper3_upload_box_cover"
                                            )
                                        ),
                                        tags$li(
                                            class = "input-list",
                                            "Choose File 4 (Optional)",
                                            div(
                                                class = "input-list-content",
                                                fileInput(
                                                    "upload_lamp_multiper4",
                                                    label = NULL,
                                                    multiple = FALSE,
                                                    accept = c(
                                                        ".xls",
                                                        ".xlsx"
                                                    )
                                                ),
                                                uiOutput("ui_upload_lamp_multiper4"),
                                                actionButton(
                                                    class = "remove-file-bttn",
                                                    style = "display:none;",
                                                    "remove_lamp_multiper4_bttn",
                                                    "X"
                                                )
                                            ),
                                            div(
                                                class = "validation-box-cover",
                                                id = "lamp_multiper4_upload_box_cover"
                                            )
                                        ),
                                        hr(),
                                        tags$li(
                                            class = "input-list",
                                            "Validate Data",
                                            span(
                                                `data-toggle` = "tooltip", `data-placement` = "right",
                                                title = home_text[10],
                                                icon("question-circle")
                                            ),
                                            div(
                                                class = "validation-box-cover",
                                                id = "lamp_multiper_validation_box_cover"
                                            ),
                                            div(
                                                class = "input-list-content",
                                                actionButton("validate_lamp_multiper", "Perform Validation")
                                            )
                                        )
                                    )
                                ),
                                div(
                                    class = "input-box",
                                    div(
                                        class = "input-box-cover",
                                        id = "lamp_multiper_input_box_cover"
                                    ),
                                    h3("Customize Report"),
                                    tags$ol(
                                        class = "content-list",
                                        tags$li(
                                            class = "input-list input-list-topbox",
                                            "Add Inputs",
                                            div(
                                                class = "input-list-content",
                                                div(
                                                    class = "input-block",
                                                    style = "display:flex;",
                                                    textInput("lamp_multiper_name", "Your Name: ", value = ""),
                                                    span(
                                                        class = "input-tooltip",
                                                        `data-toggle` = "tooltip", `data-placement` = "right",
                                                        title = home_text[12],
                                                        icon("question-circle")
                                                    ),
                                                )
                                            )
                                        ),
                                        hr(),
                                        tags$li(
                                            class = "input-list",
                                            "Generate Outputs",
                                            div(
                                                class = "input-list-content",
                                                actionButton("report_lamp_multiper", "Download Report", icon = icon("download")),
                                                downloadButton("report_lamp_multiper_hidden", "", class = "hidden_download")
                                            ),
                                            div(
                                                id = "lamp_multiper_loader",
                                                class = "custom-loader",
                                                style = "display:none;",
                                                div(
                                                    id = "lamp_multiper_loader_bar",
                                                    class = "custom-loader-bar"
                                                )
                                            ),
                                            div(
                                                class = "input-box-cover",
                                                id = "lamp_multiper_generate_cover"
                                            )
                                        ),
                                        img(src = "images/photos/photo_lamp_conch_03.jpeg", id = "datatype-lamp-multi-img", class = "datatype-img")
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
            tabsetPanel(
                tabPanel(
                    "Single Period",
                    tags$script(
                        HTML(
                            "$(document).ready(function(){
                        $('#spag_1per_name').prop('disabled', true);
                        $('#validate_spag_1per').prop('disabled', true);
                    });"
                        )
                    ),
                    div( # Temporary cover for LAMP and SPAG reports.
                        style = "position: relative; left: -15px; top:195px;",
                        div(
                            class = "temporary-cover",
                            style = "
                                position: fixed;
                                height: 100vh;
                                width: 100vw;
                                background-color: rgba(200, 200, 200, 0.5);
                                z-index: 3;
                                margin-left: 0px;
                            ",
                            div(
                                style = "
                                    position: relative;
                                    width: 25%;
                                    height: 25%;
                                    background-color: rgba(255,255,255,0.6);
                                    margin: auto;
                                    top: 160px;
                                    border-radius: 10px;
                                    ",
                                h1(
                                    "Coming Soon!",
                                    style = "
                                    position: absolute;
                                    text-align: center;
                                    top: 50%;
                                    left: 50%;
                                    margin: 0;
                                    transform: translate(-50%, -50%);
                                    font-size: 2em;
                                    font-weight: 800;
                                    color: black;
                                    "
                                )
                            )
                        ),
                    ),
                    div(
                        class = "content-container-parent",
                        div(
                            class = "content-container-grid grid-report",
                            div(
                                class = "content-box report-content-box",
                                h2("Single Period SPAG Reports"),
                                p(datatype_text[3])
                            ),
                            div(
                                class = "content-container grid-generator",
                                div(
                                    class = "input-box",
                                    h3(
                                        "Upload Data"
                                    ),
                                    tags$ol(
                                        class = "content-list",
                                        tags$li(
                                            class = "input-list input-list-topbox",
                                            "Choose Datatype",
                                            div(
                                                class = "input-list-content",
                                                prettyRadioButtons("datatype_spag_1per", label = NULL, choices = c("Visual Census", "Laser"), selected = "Visual Census", inline = TRUE)
                                            )
                                        ),
                                        hr(),
                                        tags$li(
                                            class = "input-list",
                                            "Choose File",
                                            span(
                                                `data-toggle` = "tooltip", `data-placement` = "right",
                                                title = home_text[11],
                                                icon("question-circle")
                                            ),
                                            div(
                                                class = "input-list-content",
                                                fileInput(
                                                    "upload_spag_1per",
                                                    label = NULL,
                                                    multiple = FALSE,
                                                    accept = c(
                                                        ".xls",
                                                        ".xlsx"
                                                    )
                                                ),
                                                uiOutput("ui_upload_spag_1per")
                                            )
                                        ),
                                        hr(),
                                        tags$li(
                                            class = "input-list",
                                            "Validate Data",
                                            span(
                                                `data-toggle` = "tooltip", `data-placement` = "right",
                                                title = home_text[10],
                                                icon("question-circle")
                                            ),
                                            div(
                                                class = "validation-box-cover",
                                                id = "spag_1per_validation_box_cover"
                                            ),
                                            div(
                                                class = "input-list-content",
                                                actionButton("validate_spag_1per", "Perform Validation")
                                            )
                                        )
                                    )
                                ),
                                div(
                                    class = "input-box",
                                    div(
                                        class = "input-box-cover",
                                        id = "spag_1per_input_box_cover"
                                    ),
                                    h3("Customize Report"),
                                    tags$ol(
                                        class = "content-list",
                                        tags$li(
                                            class = "input-list input-list-topbox",
                                            "Add Inputs",
                                            div(
                                                class = "input-list-content",
                                                div(
                                                    class = "input-block",
                                                    style = "display:flex;",
                                                    textInput("spag_1per_name", "Your Name: ", value = ""),
                                                    span(
                                                        class = "input-tooltip",
                                                        `data-toggle` = "tooltip", `data-placement` = "right",
                                                        title = home_text[12],
                                                        icon("question-circle")
                                                    ),
                                                )
                                            )
                                        ),
                                        hr(),
                                        tags$li(
                                            class = "input-list",
                                            "Generate Outputs",
                                            div(
                                                class = "input-list-content",
                                                actionButton("report_spag_1per", "Download Report", icon = icon("download")),
                                                downloadButton("report_spag_1per_hidden", "", class = "hidden_download")
                                            ),
                                            div(
                                                id = "spag_1per_loader",
                                                class = "custom-loader",
                                                style = "display:none;",
                                                div(
                                                    id = "spag_1per_loader_bar",
                                                    class = "custom-loader-bar"
                                                )
                                            ),
                                            div(
                                                class = "input-box-cover",
                                                id = "spag_1per_generate_cover"
                                            )
                                        ),
                                        img(src = "images/photos/photo_spags_02.png", id = "datatype-spag-1per-img", class = "datatype-img")
                                    )
                                )
                            )
                        )
                    )
                ),
                tabPanel(
                    "Multiple Periods",
                    tags$script(
                        HTML(
                            "$(document).ready(function(){
                        $('#spag_multiper_name').prop('disabled', true);
                        $('#validate_spag_multiper').prop('disabled', true);
                        $('#upload_spag_multiper2').prop('disabled', true);
                        $('#upload_spag_multiper3').prop('disabled', true);
                        $('#upload_spag_multiper4').prop('disabled', true);
                    });"
                        )
                    ),
                    div( # Temporary cover for LAMP and SPAG reports.
                        style = "position: relative; left: -15px; top:195px;",
                        div(
                            class = "temporary-cover",
                            style = "
                                position: fixed;
                                height: 100vh;
                                width: 100vw;
                                background-color: rgba(200, 200, 200, 0.5);
                                z-index: 3;
                                margin-left: 0px;
                            ",
                            div(
                                style = "
                                    position: relative;
                                    width: 25%;
                                    height: 25%;
                                    background-color: rgba(255,255,255,0.6);
                                    margin: auto;
                                    top: 160px;
                                    border-radius: 10px;
                                    ",
                                h1(
                                    "Coming Soon!",
                                    style = "
                                    position: absolute;
                                    text-align: center;
                                    top: 50%;
                                    left: 50%;
                                    margin: 0;
                                    transform: translate(-50%, -50%);
                                    font-size: 2em;
                                    font-weight: 800;
                                    color: black;
                                    "
                                )
                            )
                        ),
                    ),
                    div(
                        class = "content-container-parent",
                        div(
                            class = "content-container-grid grid-report",
                            div(
                                class = "content-box report-content-box",
                                h2("Multi-Period SPAG Reports"),
                                p(datatype_text[3])
                            ),
                            div(
                                class = "content-container grid-generator",
                                div(
                                    class = "input-box",
                                    h3("Upload Data"),
                                    tags$ol(
                                        class = "content-list",
                                        tags$li(
                                            class = "input-list input-list-topbox",
                                            "Choose Datatype",
                                            div(
                                                class = "input-list-content",
                                                prettyRadioButtons("datatype_spag_multiper", label = NULL, choices = c("Visual Census", "Laser"), selected = "Visual Census", inline = TRUE)
                                            )
                                        ),
                                        hr(),
                                        tags$li(
                                            class = "input-list",
                                            "Choose File 1",
                                            span(
                                                `data-toggle` = "tooltip", `data-placement` = "right",
                                                title = home_text[11],
                                                icon("question-circle")
                                            ),
                                            div(
                                                class = "input-list-content",
                                                fileInput(
                                                    "upload_spag_multiper1",
                                                    label = NULL,
                                                    multiple = FALSE,
                                                    accept = c(
                                                        ".xls",
                                                        ".xlsx"
                                                    )
                                                ),
                                                uiOutput("ui_upload_spag_multiper1")
                                            )
                                        ),
                                        tags$li(
                                            class = "input-list",
                                            "Choose File 2",
                                            div(
                                                class = "input-list-content",
                                                fileInput(
                                                    "upload_spag_multiper2",
                                                    label = NULL,
                                                    multiple = FALSE,
                                                    accept = c(
                                                        ".xls",
                                                        ".xlsx"
                                                    )
                                                ),
                                                uiOutput("ui_upload_spag_multiper2")
                                            ),
                                            div(
                                                class = "validation-box-cover",
                                                id = "spag_multiper2_upload_box_cover"
                                            )
                                        ),
                                        tags$li(
                                            class = "input-list",
                                            "Choose File 3 (Optional)",
                                            div(
                                                class = "input-list-content",
                                                fileInput(
                                                    "upload_spag_multiper3",
                                                    label = NULL,
                                                    multiple = FALSE,
                                                    accept = c(
                                                        ".xls",
                                                        ".xlsx"
                                                    )
                                                ),
                                                uiOutput("ui_upload_spag_multiper3"),
                                                actionButton(
                                                    class = "remove-file-bttn",
                                                    style = "display:none;",
                                                    "remove_spag_multiper3_bttn",
                                                    "X"
                                                )
                                            ),
                                            div(
                                                class = "validation-box-cover",
                                                id = "spag_multiper3_upload_box_cover"
                                            )
                                        ),
                                        tags$li(
                                            class = "input-list",
                                            "Choose File 4 (Optional)",
                                            div(
                                                class = "input-list-content",
                                                fileInput(
                                                    "upload_spag_multiper4",
                                                    label = NULL,
                                                    multiple = FALSE,
                                                    accept = c(
                                                        ".xls",
                                                        ".xlsx"
                                                    )
                                                ),
                                                uiOutput("ui_upload_spag_multiper4"),
                                                actionButton(
                                                    class = "remove-file-bttn",
                                                    style = "display:none;",
                                                    "remove_spag_multiper4_bttn",
                                                    "X"
                                                )
                                            ),
                                            div(
                                                class = "validation-box-cover",
                                                id = "spag_multiper4_upload_box_cover"
                                            )
                                        ),
                                        hr(),
                                        tags$li(
                                            class = "input-list",
                                            "Validate Data",
                                            span(
                                                `data-toggle` = "tooltip", `data-placement` = "right",
                                                title = home_text[10],
                                                icon("question-circle")
                                            ),
                                            div(
                                                class = "validation-box-cover",
                                                id = "spag_multiper_validation_box_cover"
                                            ),
                                            div(
                                                class = "input-list-content",
                                                actionButton("validate_spag_multiper", "Perform Validation")
                                            )
                                        )
                                    )
                                ),
                                div(
                                    class = "input-box",
                                    div(
                                        class = "input-box-cover",
                                        id = "spag_multiper_input_box_cover"
                                    ),
                                    h3("Customize Report"),
                                    tags$ol(
                                        class = "content-list",
                                        tags$li(
                                            class = "input-list input-list-topbox",
                                            "Add Inputs",
                                            div(
                                                class = "input-list-content",
                                                div(
                                                    class = "input-block",
                                                    style = "display:flex;",
                                                    textInput("spag_multiper_name", "Your Name: ", value = ""),
                                                    span(
                                                        class = "input-tooltip",
                                                        `data-toggle` = "tooltip", `data-placement` = "right",
                                                        title = home_text[12],
                                                        icon("question-circle")
                                                    ),
                                                )
                                            )
                                        ),
                                        hr(),
                                        tags$li(
                                            class = "input-list",
                                            "Generate Outputs",
                                            div(
                                                class = "input-list-content",
                                                actionButton("report_spag_multiper", "Download Report", icon = icon("download")),
                                                downloadButton("report_spag_multiper_hidden", "", class = "hidden_download")
                                            ),
                                            div(
                                                id = "spag_multiper_loader",
                                                class = "custom-loader",
                                                style = "display:none;",
                                                div(
                                                    id = "spag_multiper_loader_bar",
                                                    class = "custom-loader-bar"
                                                )
                                            ),
                                            div(
                                                class = "input-box-cover",
                                                id = "spag_multiper_generate_cover"
                                            )
                                        ),
                                        img(src = "images/photos/photo_spags_02.png", id = "datatype-spag-multi-img", class = "datatype-img")
                                    )
                                )
                            )
                        )
                    )
                )
            )
        )
    ),
    navbarMenu(
        "Data",
        tabPanel(
            "Templates",
            div(
                class = "content-container-parent",
                div(
                    class = "content-container-grid grid-report",
                    div(
                        class = "content-box",
                        h2("Data Templates"),
                        p(tab_text[1]),
                        br(),
                        p(tab_text[2]),
                        br(),
                        p(tab_text[3])
                    ),
                    div(
                        class = "data-template-table",
                        uiOutput("template_list_table")
                    )
                )
            )
        ),
        tabPanel(
            "Historical",
            div(
                class = "content-container-parent",
                div(
                    class = "content-container-grid grid-report",
                    div(
                        class = "content-box",
                        p("For a bank of existing standardized historic data, visit the Data Bank below:"),
                        br()
                    ),
                    div(
                        class = "content-container",
                        div(
                            class = "external-link-button",
                            a(
                                href = link_text[10],
                                target = "_blank",
                                h1("Data Bank")
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
                class = "content-container-grid  grid-report",
                div(
                    class = "content-box",
                    h2("Manual"),
                    p(tab_text[4]),
                    br(),
                    p(tab_text[5])
                ),
                div(
                    style = "display:flex;",
                    div(
                        class = "content-container",
                        div(
                            class = "external-link-button",
                            a(
                                href = link_text[8],
                                target = "_blank",
                                h1("Care Manual")
                            )
                        )
                    ),
                    div(
                        class = "content-container",
                        div(
                            class = "external-link-button",
                            a(
                                href = link_text[9],
                                target = "_blank",
                                h1("Data Validation Documentation")
                            )
                        )
                    )
                )
            )
        )
    )
)
