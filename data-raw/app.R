#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
#install.packages("jsonlite")
#install.packages("shiny.i18n")
#install.packages("googleLanguageR")
library(shiny.i18n)
library(shiny)
library(shinyjs)
library(shinythemes)
library(tidyverse)
library(glamr)
library(sismar)
library(googledrive)
library(googlesheets4)
library(glue)
library(jsonlite)


# Global Options ----
load_secrets()
i18n <- Translator$new(translation_json_path = "translations.json")
i18n$set_translation_language("pt")


# Define UI
ui <- fluidPage(
  usei18n(i18n),
  actionButton("en_trans", "Translate to English"),
 actionButton("port_trans", "Traduzir a Portuguese"),
  navbarPage(
    useShinyjs(),
    id = "mainMenu",
    fluid = TRUE,
    collapsible = TRUE,
    title = "MISAU: Processamento de Dados SISMA",
    position = "fixed-top",
    theme = shinytheme("flatly"),#cerulean, spacelab, yeti, sandstone
    #footer = titlePanel(title = app_title),
    tags$style(type="text/css",
               "body {padding-top: 70px;}
        .navbar-nav {float: right;}")),
 tabPanel(
   id = "menuHome",
   title = "Home",
   titlePanel(i18n$t("SISMA Data Processing App")),
   mainPanel(
     p(i18n$t("This R package was created for the Mozambique Ministry of Health (MISAU) to automate data processing and support analysis of SISMA programmatic data. This package is designed for the specific purpose of cleaning, reshaping and feature engineering provided standard SISMA DHIS2 tabular exports.")
   ))
 )
   ,
   # mainPanel(
   #   htmltools::includeMarkdown("sismar-guidance.md"),
   # )),
 #titlePanel(i18n$t("SISMA Data Processing App")),
  fileInput("file", i18n$t("Choose a CSV file")),
  selectInput("type", i18n$t("Select Export Type"), choices = c("SMI-CPN", "SMI-MAT", "SMI-CCR", "ATS Result", "ATS History",
                                                        "ATS CI", "ATS SAAJ", "ATS CCSD", "ATS SMI", "ATS Auto", "HIV TARV",
                                                        "HIV PREP", "HIV APSS")),
  selectInput("language", i18n$t("Select Language"), choices = c("portuguese", "english")),
  actionButton("processBtn", i18n$t("Process Data")),
  downloadButton("downloadBtn", i18n$t("Download Processed Data")),
  dataTableOutput("previewTable")  # Adding a table to show data preview

)

# Define Server Logic
server <- function(input, output, session) {

  observeEvent(input$en_trans,{
    update_lang("en", session)
  })

  observeEvent(input$port_trans,{
    update_lang("pt", session)
  })


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
      write_csv(processed_data(), file)
    }
  )


}

# Run the Shiny App
shinyApp(ui = ui, server = server)

