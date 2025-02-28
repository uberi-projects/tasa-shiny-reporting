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
                )
            )
        )
    ),
    navbarMenu(
        "Create Reports",
        tabPanel(
            "Fisheries Catch",
            tags$script(
                HTML(
                    "$(document).ready(function(){
                        $('#fisheries_name').prop('disabled', true);
                        $('#validate_fisheries').prop('disabled', true);
                    });
                    $(function () {
                        $('[data-toggle=tooltip]').tooltip()
                    })"
                )
            ),
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
                        class = "content-container grid-generator",
                        div(
                            class = "input-box",
                            h3("Upload Data"),
                            tags$ol(
                                class = "content-list",
                                tags$li(
                                    class = "input-list input-list-topbox",
                                    "Choose Time Period",
                                    div(
                                        class = "input-list-content",
                                        prettyRadioButtons("period_fisheries",
                                            label = NULL,
                                            choices = c("One Season", "Multiple Seasons", "Multiple Years"), selected = "One Season", inline = TRUE
                                        )
                                    )
                                ),
                                hr(),
                                tags$li(
                                    class = "input-list",
                                    "Choose File",
                                    div(
                                        class = "input-list-content",
                                        fileInput(
                                            "upload_fisheries",
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
                                    span(
                                        `data-toggle` = "tooltip", `data-placement` = "right",
                                        title = home_text[10],
                                        icon("question-circle")
                                    ),
                                    div(
                                        class = "validation-box-cover",
                                        id = "fisheries_validation_box_cover"
                                    ),
                                    div(
                                        class = "input-list-content",
                                        actionButton("validate_fisheries", "Perform Validation")
                                    )
                                )
                            )
                        ),
                        div(
                            class = "input-box shorter-input-box",
                            div(
                                class = "input-box-cover",
                                id = "fisheries_input_box_cover"
                            ),
                            h3("Customize Report"),
                            tags$ol(
                                class = "content-list",
                                tags$li(
                                    class = "input-list input-list-topbox",
                                    "Add Inputs",
                                    div(
                                        class = "input-list-content",
                                        textInput("fisheries_name", "Your Name: ", value = "")
                                    )
                                ),
                                hr(),
                                tags$li(
                                    class = "input-list ",
                                    "Generate Report",
                                    div(
                                        class = "input-list-content",
                                        downloadButton("report_fisheries", "Generate")
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
            tags$script(
                HTML(
                    "$(document).ready(function(){
                        $('#fisher_name').prop('disabled', true);
                        $('#validate_fisher').prop('disabled', true);
                    });"
                )
            ),
            div(
                class = "content-container-parent",
                div(
                    class = "content-container-grid grid-report",
                    div(
                        class = "content-box",
                        h2("Fisher Catch Project Reports"),
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
                                    class = "input-list input-list-topbox",
                                    "Choose Time Period",
                                    div(
                                        class = "input-list-content",
                                        prettyRadioButtons("period_fisher",
                                            label = NULL,
                                            choices = c("One Season", "Multiple Seasons", "Multiple Years"), selected = "One Season", inline = TRUE
                                        )
                                    )
                                ),
                                hr(),
                                tags$li(
                                    class = "input-list",
                                    "Choose File",
                                    div(
                                        class = "input-list-content",
                                        fileInput(
                                            "upload_fisher",
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
                                        class = "validation-box-cover",
                                        id = "fisher_validation_box_cover"
                                    ),
                                    div(
                                        class = "input-list-content",
                                        actionButton("validate_fisher", "Perform Validation")
                                    )
                                )
                            )
                        ),
                        div(
                            class = "input-box shorter-input-box",
                            div(
                                class = "input-box-cover",
                                id = "fisher_input_box_cover"
                            ),
                            h3("Customize Report"),
                            tags$ol(
                                class = "content-list",
                                tags$li(
                                    class = "input-list input-list-topbox",
                                    "Add Inputs",
                                    div(
                                        class = "input-list-content",
                                        textInput("fisher_name", "Your Name: ", value = "")
                                    )
                                ),
                                hr(),
                                tags$li(
                                    class = "input-list",
                                    "Generate Report",
                                    div(
                                        class = "input-list-content",
                                        downloadButton("report_fisher", "Generate"),
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
            tags$script(
                HTML(
                    "$(document).ready(function(){
                        $('#lamp_name').prop('disabled', true);
                        $('#validate_lamp').prop('disabled', true);
                    });"
                )
            ),
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
                                    "Choose File",
                                    div(
                                        class = "input-list-content",
                                        fileInput(
                                            "upload_lamp",
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
                                    div(
                                        class = "validation-box-cover",
                                        id = "lamp_validation_box_cover"
                                    ),
                                    div(
                                        class = "input-list-content",
                                        actionButton("validate_lamp", "Perform Validation")
                                    )
                                )
                            )
                        ),
                        div(
                            class = "input-box shorter-input-box",
                            div(
                                class = "input-box-cover",
                                id = "lamp_input_box_cover"
                            ),
                            h3("Customize Report"),
                            tags$ol(
                                class = "content-list",
                                tags$li(
                                    class = "input-list input-list-topbox",
                                    "Add Inputs",
                                    div(
                                        class = "input-list-content",
                                        textInput("lamp_name", "Your Name: ", value = "")
                                    )
                                ),
                                hr(),
                                tags$li(
                                    class = "input-list",
                                    "Generate Report",
                                    div(
                                        class = "input-list-content",
                                        downloadButton("report_lamp", "Generate")
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
            tags$script(
                HTML(
                    "$(document).ready(function(){
                        $('#spag_name').prop('disabled', true);
                        $('#validate_spag').prop('disabled', true);
                    });"
                )
            ),
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
                                        prettyRadioButtons("datatype_spag", label = NULL, choices = c("Visual Census", "Laser"), selected = "Visual Census", inline = TRUE)
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
                                    "Choose File",
                                    div(
                                        class = "input-list-content",
                                        fileInput(
                                            "upload_spag",
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
                                        class = "validation-box-cover",
                                        id = "spag_validation_box_cover"
                                    ),
                                    div(
                                        class = "input-list-content",
                                        actionButton("validate_spag", "Perform Validation")
                                    )
                                )
                            )
                        ),
                        div(
                            class = "input-box shorter-input-box",
                            div(
                                class = "input-box-cover",
                                id = "spag_input_box_cover"
                            ),
                            h3("Customize Report"),
                            tags$ol(
                                class = "content-list",
                                tags$li(
                                    class = "input-list input-list-topbox",
                                    "Add Inputs",
                                    div(
                                        class = "input-list-content",
                                        textInput("spag_name", "Your Name: ", value = "")
                                    )
                                ),
                                hr(),
                                tags$li(
                                    class = "input-list",
                                    "Generate Report",
                                    div(
                                        class = "input-list-content",
                                        downloadButton("report_spag", "Generate")
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
