## ui.R

# Load packages ---------------------------
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(rmarkdown)

# Source code ---------------------------
source("theme.r")

# Define ui ---------------------------
ui <- dashboardPage(
    dashboardHeader(
        title = tags$a(
            img(
                src = "images/TASA_logo_full_white.png", height = "50px"
            )
        ),
        tags$li(
            class = "dropdown",
            actionBttn(
                inputId = "button_header_1",
                label = "Source Code",
                style = "jelly",
                color = "primary"
            ),
            tags$style(HTML("
    #button_header_1 {
      margin-right: 20px;
    }
  "))
        ),
        tags$li(
            class = "dropdown",
            actionBttn(
                inputId = "button_header_2",
                label = "Manual",
                style = "jelly",
                color = "primary"
            ),
            tags$style(HTML("
    #button_header_2 {
      margin-right: 20px;
    }
  "))
        ),
        tags$li(
            class = "dropdown",
            actionBttn(
                inputId = "button_header_3",
                label = "Datasheet Templates",
                style = "jelly",
                color = "primary"
            ),
            tags$style(HTML("
    #button_header_3 {
      margin-right: 20px;
    }
  "))
        )
    ),
    dashboardSidebar(
        tags$style(".left-side, .main-sidebar {padding-top: 80px}"),
        sidebarMenu(
            id = "tabs",
            menuItem("Home", tabName = "home", icon = icon("home")),
            menuItem("Create Reports",
                icon = icon("list-alt"),
                menuSubItem("Queen Conch", tabName = "conch"),
                menuSubItem("LAMP", tabName = "lamp"),
                menuSubItem("SPAG", tabName = "spag"),
                menuSubItem("Fisheries Catch", tabName = "catch")
            )
        )
    ),
    dashboardBody(
        use_theme(theme_dashboard),
        includeCSS("www/custom.css"),
        tabItems(
            tabItem(
                "home",
                h1("Demo Report", align = "left"),
                textInput("name", "Your Name: ", value = ""),
                downloadButton("report", "Generate report"),
                column(width = 12, div(style = "height: 20px;"))
            )
        )
    )
)
