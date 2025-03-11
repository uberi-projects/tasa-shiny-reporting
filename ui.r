## ui.R

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
        )
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
                    p(home_text[1]), p(strong(home_text[3])),
                    br(),
                    div(class = "pipeline-div", img(class = "pipeline", src = "images/pipeline.png", alt = "Pipeline from field to data to reporting")),
                    br()
                ),
                hr(),
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
                    p(strong(
                        "For additional information on using and maintaining this app, please visit the ",
                        actionButton("go_to_manual_tab", "Manual"),
                        " page. "
                    ))
                ),
                hr(),
                div(
                    actionButton("feedback_opn_bttn", "Report an Issue")
                ),
                div(
                    id = "feedback-content-box",
                    class = "feedback-content-box",
                    div(
                        class = "feedback-response-box",
                        actionButton("feedback_bttn", "Send Feedback"),
                        br(),
                        div(
                            class = "feedback-box",
                            textAreaInput("feedback_text", "Describe Issue", value = ""),
                            prettyRadioButtons("bug_severity",
                                label = "Severity",
                                choices = c("1", "2", "3", "4", "5"), inline = TRUE
                            )
                        )
                    )
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
                    "Single Year",
                    tags$script(
                        HTML(
                            "$(document).ready(function(){
                        $('#fisheries_1yr_name').prop('disabled', true);
                        $('#validate_fisheries_1yr').prop('disabled', true);
                    });
                    $(function () {
                        $('[data-toggle=tooltip]').tooltip({container: 'body'})
                    })"
                        )
                    ),
                    div(
                        class = "content-container-parent",
                        div(
                            class = "content-container-grid grid-report",
                            div(
                                class = "content-box",
                                h2("Single Year Fisheries Catch Reports"),
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
                                            class = "input-list",
                                            "Choose File",
                                            div(
                                                class = "input-list-content",
                                                fileInput(
                                                    "upload_fisheries_1yr",
                                                    label = NULL,
                                                    multiple = FALSE,
                                                    accept = c(
                                                        ".xls",
                                                        ".xlsx"
                                                    )
                                                )
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
                                                id = "fisheries_1yr_validation_box_cover"
                                            ),
                                            div(
                                                class = "input-list-content",
                                                actionButton("validate_fisheries_1yr", "Perform Validation")
                                            )
                                        )
                                    )
                                ),
                                div(
                                    class = "input-box shorter-input-box",
                                    div(
                                        class = "input-box-cover",
                                        id = "fisheries_1yr_input_box_cover"
                                    ),
                                    h3("Customize Report"),
                                    tags$ol(
                                        class = "content-list",
                                        tags$li(
                                            class = "input-list input-list-topbox",
                                            "Add Inputs",
                                            div(
                                                class = "input-list-content",
                                                textInput("fisheries_1yr_name", "Your Name: ", value = "")
                                            )
                                        ),
                                        hr(),
                                        tags$li(
                                            class = "input-list ",
                                            "Generate Report",
                                            div(
                                                class = "input-list-content",
                                                downloadButton("report_fisheries_1yr", "Generate")
                                            )
                                        )
                                    )
                                )
                            )
                        )
                    )
                ),
                tabPanel(
                    "Multiple Years",
                    tags$script(
                        HTML(
                            "$(document).ready(function(){
                        $('#fisheries_multiyr_name').prop('disabled', true);
                        $('#validate_fisheries_multiyr').prop('disabled', true);
                    });
                    $(function () {
                        $('[data-toggle=tooltip]').tooltip({container: 'body'})
                    })"
                        )
                    ),
                    div(
                        class = "content-container-parent",
                        div(
                            class = "content-container-grid grid-report",
                            div(
                                class = "content-box",
                                h2("Multi-Year Fisheries Catch Reports"),
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
                                            class = "input-list",
                                            "Choose File 1",
                                            div(
                                                class = "input-list-content",
                                                fileInput(
                                                    "upload_fisheries_multiyr1",
                                                    label = NULL,
                                                    multiple = FALSE,
                                                    accept = c(
                                                        ".xls",
                                                        ".xlsx"
                                                    )
                                                )
                                            )
                                        ),
                                        tags$li(
                                            class = "input-list",
                                            "Choose File 2",
                                            div(
                                                class = "input-list-content",
                                                fileInput(
                                                    "upload_fisheries_multiyr2",
                                                    label = NULL,
                                                    multiple = FALSE,
                                                    accept = c(
                                                        ".xls",
                                                        ".xlsx"
                                                    )
                                                )
                                            )
                                        ),
                                        tags$li(
                                            class = "input-list",
                                            "Choose File 3 (Optional)",
                                            div(
                                                class = "input-list-content",
                                                fileInput(
                                                    "upload_fisheries_multiyr3",
                                                    label = NULL,
                                                    multiple = FALSE,
                                                    accept = c(
                                                        ".xls",
                                                        ".xlsx"
                                                    )
                                                )
                                            )
                                        ),
                                        tags$li(
                                            class = "input-list",
                                            "Choose File 4 (Optional)",
                                            div(
                                                class = "input-list-content",
                                                fileInput(
                                                    "upload_fisheries_multiyr4",
                                                    label = NULL,
                                                    multiple = FALSE,
                                                    accept = c(
                                                        ".xls",
                                                        ".xlsx"
                                                    )
                                                )
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
                                                id = "fisheries_multiyr_validation_box_cover"
                                            ),
                                            div(
                                                class = "input-list-content",
                                                actionButton("validate_fisheries_multiyr", "Perform Validation")
                                            )
                                        )
                                    )
                                ),
                                div(
                                    class = "input-box shorter-input-box",
                                    div(
                                        class = "input-box-cover",
                                        id = "fisheries_multiyr_input_box_cover"
                                    ),
                                    h3("Customize Report"),
                                    tags$ol(
                                        class = "content-list",
                                        tags$li(
                                            class = "input-list input-list-topbox",
                                            "Add Inputs",
                                            div(
                                                class = "input-list-content",
                                                textInput("fisheries_multiyr_name", "Your Name: ", value = "")
                                            )
                                        ),
                                        hr(),
                                        tags$li(
                                            class = "input-list ",
                                            "Generate Report",
                                            div(
                                                class = "input-list-content",
                                                downloadButton("report_fisheries_multiyr", "Generate")
                                            )
                                        )
                                    )
                                )
                            )
                        )
                    )
                ),
                tabPanel(
                    "Single Season",
                    div(
                        class = "content-container-parent",
                        div(
                            class = "content-container-grid grid-report",
                            div(
                                class = "content-box",
                                h2("Single Season Fisheries Catch Reports"),
                                p(datatype_text[1])
                            )
                        )
                    )
                ),
                tabPanel(
                    "Multiple Seasons",
                    div(
                        class = "content-container-parent",
                        div(
                            class = "content-container-grid grid-report",
                            div(
                                class = "content-box",
                                h2("Multi-Season Fisheries Catch Reports"),
                                p(datatype_text[1])
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
                    "Single Year",
                    tags$script(
                        HTML(
                            "$(document).ready(function(){
                        $('#fisher_1yr_name').prop('disabled', true);
                        $('#validate_fisher_1yr').prop('disabled', true);
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
                                h2("Single Year Fisher Catch Project Reports"),
                                p(dummy_text[1])
                            ),
                            div(
                                class = "content-container grid-generator",
                                div(
                                    class = "input-box",
                                    h3("Upload Data"),
                                    tags$ol(
                                        class = "content-list",
                                        tags$li(
                                            class = "input-list",
                                            "Choose File",
                                            div(
                                                class = "input-list-content",
                                                fileInput(
                                                    "upload_fisher_1yr",
                                                    label = NULL,
                                                    multiple = FALSE,
                                                    accept = c(
                                                        ".xls",
                                                        ".xlsx"
                                                    )
                                                )
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
                                                id = "fisher_1yr_validation_box_cover"
                                            ),
                                            div(
                                                class = "input-list-content",
                                                actionButton("validate_fisher_1yr", "Perform Validation")
                                            )
                                        )
                                    )
                                ),
                                div(
                                    class = "input-box shorter-input-box",
                                    div(
                                        class = "input-box-cover",
                                        id = "fisher_1yr_input_box_cover"
                                    ),
                                    h3("Customize Report"),
                                    tags$ol(
                                        class = "content-list",
                                        tags$li(
                                            class = "input-list input-list-topbox",
                                            "Add Inputs",
                                            div(
                                                class = "input-list-content",
                                                textInput("fisher_1yr_name", "Your Name: ", value = "")
                                            )
                                        ),
                                        hr(),
                                        tags$li(
                                            class = "input-list",
                                            "Generate Report",
                                            div(
                                                class = "input-list-content",
                                                downloadButton("report_fisher_1yr", "Generate"),
                                            )
                                        )
                                    )
                                )
                            )
                        )
                    )
                ),
                tabPanel(
                    "Multiple Years",
                    tags$script(
                        HTML(
                            "$(document).ready(function(){
                        $('#fisher_multiyr_name').prop('disabled', true);
                        $('#validate_fisher_multiyr').prop('disabled', true);
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
                                h2("Multi-Year Fisher Catch Project Reports"),
                                p(dummy_text[1])
                            ),
                            div(
                                class = "content-container grid-generator",
                                div(
                                    class = "input-box",
                                    h3("Upload Data"),
                                    tags$ol(
                                        class = "content-list",
                                        tags$li(
                                            class = "input-list",
                                            "Choose File 1",
                                            div(
                                                class = "input-list-content",
                                                fileInput(
                                                    "upload_fisher_multiyr1",
                                                    label = NULL,
                                                    multiple = FALSE,
                                                    accept = c(
                                                        ".xls",
                                                        ".xlsx"
                                                    )
                                                )
                                            )
                                        ),
                                        tags$li(
                                            class = "input-list",
                                            "Choose File 2",
                                            div(
                                                class = "input-list-content",
                                                fileInput(
                                                    "upload_fisher_multiyr2",
                                                    label = NULL,
                                                    multiple = FALSE,
                                                    accept = c(
                                                        ".xls",
                                                        ".xlsx"
                                                    )
                                                )
                                            )
                                        ),
                                        tags$li(
                                            class = "input-list",
                                            "Choose File 3 (Optional)",
                                            div(
                                                class = "input-list-content",
                                                fileInput(
                                                    "upload_fisher_multiyr3",
                                                    label = NULL,
                                                    multiple = FALSE,
                                                    accept = c(
                                                        ".xls",
                                                        ".xlsx"
                                                    )
                                                )
                                            )
                                        ),
                                        tags$li(
                                            class = "input-list",
                                            "Choose File 4 (Optional)",
                                            div(
                                                class = "input-list-content",
                                                fileInput(
                                                    "upload_fisher_multiyr4",
                                                    label = NULL,
                                                    multiple = FALSE,
                                                    accept = c(
                                                        ".xls",
                                                        ".xlsx"
                                                    )
                                                )
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
                                                id = "fisher_multiyr_validation_box_cover"
                                            ),
                                            div(
                                                class = "input-list-content",
                                                actionButton("validate_fisher_multiyr", "Perform Validation")
                                            )
                                        )
                                    )
                                ),
                                div(
                                    class = "input-box shorter-input-box",
                                    div(
                                        class = "input-box-cover",
                                        id = "fisher_multiyr_input_box_cover"
                                    ),
                                    h3("Customize Report"),
                                    tags$ol(
                                        class = "content-list",
                                        tags$li(
                                            class = "input-list input-list-topbox",
                                            "Add Inputs",
                                            div(
                                                class = "input-list-content",
                                                textInput("fisher_multiyr_name", "Your Name: ", value = "")
                                            )
                                        ),
                                        hr(),
                                        tags$li(
                                            class = "input-list",
                                            "Generate Report",
                                            div(
                                                class = "input-list-content",
                                                downloadButton("report_fisher_multiyr", "Generate"),
                                            )
                                        )
                                    )
                                )
                            )
                        )
                    )
                ),
                tabPanel(
                    "Single Season",
                    div(
                        class = "content-container-parent",
                        div(
                            class = "content-container-grid grid-report",
                            div(
                                class = "content-box",
                                h2("Single Season Fisher Catch Project Reports"),
                                p(dummy_text[1])
                            )
                        )
                    )
                ),
                tabPanel(
                    "Multiple Seasons",
                    div(
                        class = "content-container-parent",
                        div(
                            class = "content-container-grid grid-report",
                            div(
                                class = "content-box",
                                h2("Multi-Season Fisher Catch Project Reports"),
                                p(dummy_text[1])
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
                    "Single Year",
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
                                h2("Single Year LAMP Reports"),
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
                                                prettyRadioButtons("datatype_lamp_1per", label = NULL, choices = c("Conch", "General LAMP"), selected = "Conch", inline = TRUE)
                                            )
                                        ),
                                        hr(),
                                        tags$li(
                                            class = "input-list",
                                            "Choose File",
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
                                                )
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
                                    class = "input-box shorter-input-box",
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
                                                textInput("lamp_1per_name", "Your Name: ", value = "")
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
                    "Multiple Years",
                    tags$script(
                        HTML(
                            "$(document).ready(function(){
                                $('#lamp_multiper_name').prop('disabled', true);
                                $('#validate_lamp_multiper').prop('disabled', true);
                            });"
                        )
                    ),
                    div(
                        class = "content-container-parent",
                        div(
                            class = "content-container-grid grid-report",
                            div(
                                class = "content-box",
                                h2("Multi-Year LAMP Reports"),
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
                                                )
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
                                                )
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
                                                )
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
                                                )
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
                                    class = "input-box shorter-input-box",
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
                                                textInput("lamp_multiper_name", "Your Name: ", value = "")
                                            )
                                        ),
                                        hr(),
                                        tags$li(
                                            class = "input-list",
                                            "Generate Report",
                                            div(
                                                class = "input-list-content",
                                                downloadButton("report_lamp_multiper", "Generate")
                                            )
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
            "SPAG",
            tabsetPanel(
                tabPanel(
                    "Single Year",
                    tags$script(
                        HTML(
                            "$(document).ready(function(){
                        $('#spag_1per_name').prop('disabled', true);
                        $('#validate_spag_1per').prop('disabled', true);
                    });"
                        )
                    ),
                    div(
                        class = "content-container-parent",
                        div(
                            class = "content-container-grid grid-report",
                            div(
                                class = "content-box",
                                h2("Single Year SPAG Reports"),
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
                                                prettyRadioButtons("datatype_spag_1per", label = NULL, choices = c("Visual Census", "Laser"), selected = "Visual Census", inline = TRUE)
                                            )
                                        ),
                                        hr(),
                                        tags$li(
                                            class = "input-list",
                                            "Choose File",
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
                                                )
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
                                    class = "input-box shorter-input-box",
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
                                                textInput("spag_1per_name", "Your Name: ", value = "")
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
                ),
                tabPanel(
                    "Multiple Years",
                    tags$script(
                        HTML(
                            "$(document).ready(function(){
                        $('#spag_multiper_name').prop('disabled', true);
                        $('#validate_spag_multiper').prop('disabled', true);
                    });"
                        )
                    ),
                    div(
                        class = "content-container-parent",
                        div(
                            class = "content-container-grid grid-report",
                            div(
                                class = "content-box",
                                h2("Multi-Year SPAG Reports"),
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
                                                )
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
                                                )
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
                                                )
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
                                                )
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
                                    class = "input-box shorter-input-box",
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
                                                textInput("spag_multiper_name", "Your Name: ", value = "")
                                            )
                                        ),
                                        hr(),
                                        tags$li(
                                            class = "input-list",
                                            "Generate Report",
                                            div(
                                                class = "input-list-content",
                                                downloadButton("report_spag_multiper", "Generate")
                                            )
                                        )
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
                ),
                div(
                    class = "content-container",
                    div(
                        class = "external-link-button",
                        a(
                            href = link_text[8],
                            target = "_blank",
                            h1("Visit Manual")
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
                    p(dummy_text[3])
                ),
                div(
                    class = "data-template-table",
                    uiOutput("template_list_table")
                )
            )
        )
    ),
)
