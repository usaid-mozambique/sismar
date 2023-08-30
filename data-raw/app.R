#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

#install.packages("shiny.i18n")
library(shiny.i18n)
library(shiny)
library(shinyjs)
library(shinythemes)
library(tidyverse)
library(glamr)
library(glitr)
library(sismar)
library(googledrive)
library(googlesheets4)
library(glue)


# Global Options ----


# Define UI
ui <- fluidPage(
  navbarPage(
    useShinyjs(),
    id = "mainMenu",
    fluid = TRUE,
    collapsible = TRUE,
    title = "TEST",
    position = "fixed-top",
    theme = shinytheme("flatly"),#cerulean, spacelab, yeti, sandstone
    #footer = titlePanel(title = app_title),
    tags$style(type="text/css",
               "body {padding-top: 70px;}
        .navbar-nav {float: right;}")),
  titlePanel("SISMA Data Processing App"),
  fileInput("file", "Choose a CSV file"),
  selectInput("type", "Select Export Type", choices = c("SMI-CPN", "SMI-MAT", "SMI-CCR", "ATS Result", "ATS History",
                                                        "ATS CI", "ATS SAAJ", "ATS CCSD", "ATS SMI", "ATS Auto", "HIV TARV",
                                                        "HIV PREP", "HIV APSS")),
  selectInput("language", "Select Language", choices = c("Portuguese", "English")),
  actionButton("processBtn", "Process Data"),
  downloadButton("downloadBtn", "Download Processed Data"),
  dataTableOutput("previewTable")  # Adding a table to show data preview

)

# Define Server Logic
server <- function(input, output) {


  processed_data <- reactiveVal(NULL)

  observeEvent(input$processBtn, {
    req(input$file)
    filepath <- input$file$datapath
    type <- input$type
    language <- input$language

    processed_data_df <- process_sisma_csv(filepath, type, language)
    processed_data(processed_data_df)
  })

  output$previewTable <- renderDataTable({
    processed_data()
  })

  output$downloadBtn <- downloadHandler(
    filename = function() {
      paste("processed_data_",input$type, "_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(processed_data(), file)
    }
  )


}

# Run the Shiny App
shinyApp(ui = ui, server = server)

