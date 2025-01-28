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
        title = tags$a(href = "#home", tags$img(src = "images/TASA_logo_full_white.png", alt = "Logo")),
        tags$li(class = "dropdown header_button", actionBttn(inputId = "button_header_1", label = "Source Code", style = "jelly", color = "primary")),
        tags$li(class = "dropdown header_button", actionBttn(inputId = "button_header_2", label = "Manual", style = "jelly", color = "primary")),
        tags$li(class = "dropdown header_button", actionBttn(inputId = "button_header_3", label = "Datasheet Templates", style = "jelly", color = "primary"))
    ),
    dashboardSidebar(
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
        includeCSS("www/modern-normalize.css"),
        includeCSS("www/styles.css"),
        tabItems(
            tabItem(
                "home",
                h1("Demo Report", align = "left"),
                textInput("name", "Your Name: ", value = ""),
                downloadButton("report", "Generate Report")
            )
        )
    )
)
