## ui.R

# Check for required packages ---------------------------
options(repos = c(CRAN = "https://cran.rstudio.com/"))
required_packages <- c(
    "shiny", "shinyjs", "shinyWidgets", "shinyalert", "rmarkdown", "knitr", "moments", "tidyverse",
    "readxl", "tidyverse", "ggpubr", "sf", "ggspatial", "ggnewscale", "officedown", "markdown"
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
                div(class = "header-title", h1("Turneffe Reef-Monitoring Data Reporting Tool")),
                div(a(href = link_text[1], target = "_blank", img(class = "header-logo", src = "images/TASA_logo_full_color.png", alt = "Logo")))
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
                    hr(class = "home-line"),
                    br(),
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
                    p(strong(
                        "For additional information on using and maintaining this app, please visit the ",
                        actionButton("go_to_manual_tab", "Manual"),
                        " page. "
                    ))
                )
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
                        style = "position: relative; left: -15px; top:230px;",
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
                                class = "content-box",
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
                                            "Generate Report",
                                            div(
                                                class = "input-list-content",
                                                actionButton("report_fisheries_1per", "Generate", icon = icon("download")),
                                                downloadButton("report_fisheries_1per_hidden", "", class = "hidden_download")
                                            ),
                                            div(
                                                id = "fisheries_1per_loader",
                                                class = "custom-loader",
                                                style = "display:none;",
                                                div(
                                                    id = "fisheries_1per_loader_bar",
                                                    class = "custom-loader-bar"
                                                )
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
                        style = "position: relative; left: -15px; top:230px;",
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
                                class = "content-box",
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
                                            "Generate Report",
                                            div(
                                                class = "input-list-content",
                                                actionButton("report_fisheries_multiper", "Generate", icon = icon("download")),
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
                                class = "content-box",
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
                                            "Generate Report",
                                            div(
                                                class = "input-list-content",
                                                actionButton("report_fisher_1per", "Generate", icon = icon("download")),
                                                downloadButton("report_fisher_1per_hidden", "", class = "hidden_download")
                                            ),
                                            div(
                                                id = "fisher_1per_loader",
                                                class = "custom-loader",
                                                style = "display:none;",
                                                div(
                                                    id = "fisher_1per_loader_bar",
                                                    class = "custom-loader-bar"
                                                )
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
                        style = "position: relative; left: -15px; top:230px;",
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
                                class = "content-box",
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
                                            "Generate Report",
                                            div(
                                                class = "input-list-content",
                                                actionButton("report_fisher_multiper", "Generate", icon = icon("download")),
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
                                class = "content-box",
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
                                            "Generate Report",
                                            div(
                                                class = "input-list-content",
                                                actionButton("report_lamp_1per", "Generate", icon = icon("download")),
                                                downloadButton("report_lamp_1per_hidden", "", class = "hidden_download")
                                            ),
                                            div(
                                                id = "lamp_1per_loader",
                                                class = "custom-loader",
                                                style = "display:none;",
                                                div(
                                                    id = "lamp_1per_loader_bar",
                                                    class = "custom-loader-bar"
                                                )
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
                                class = "content-box",
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
                                            "Generate Report",
                                            div(
                                                class = "input-list-content",
                                                actionButton("report_lamp_multiper", "Generate", icon = icon("download")),
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
                        style = "position: relative; left: -15px; top:230px;",
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
                                class = "content-box",
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
                                            "Generate Report",
                                            div(
                                                class = "input-list-content",
                                                actionButton("report_spag_1per", "Generate", icon = icon("download")),
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
                        style = "position: relative; left: -15px; top:230px;",
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
                                class = "content-box",
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
                                            "Generate Report",
                                            div(
                                                class = "input-list-content",
                                                actionButton("report_spag_multiper", "Generate", icon = icon("download")),
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
    tabPanel(
        "Manual",
        div(
            class = "content-container-parent",
            div(
                class = "content-container-grid",
                div(
                    class = "content-box",
                    h2("Manual"),
                    p(tab_text[4]),
                    br(),
                    p(tab_text[5])
                ),
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
                    p(tab_text[1]),
                    br(),
                    p(tab_text[2]),
                    br(),
                    p(tab_text[3])
                ),
                div(
                    class = "data-template-table",
                    uiOutput("template_list_table")
                ),
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
    ),
)
