## ui.R

# Load packages ---------------------------
library(shiny)
library(rmarkdown)

# Define ui ---------------------------
ui <- fluidPage(
    textInput("name", "Your Name: ", value = ""),
    downloadButton("report", "Generate report")
)
