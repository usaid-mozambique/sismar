library(shiny)
library(tidyverse)
library(sismar)
library(janitor)
library(shinythemes)
library(shinyjs)

# Define UI for the Shiny app with a fixed-top navbar
ui <- fluidPage(
  useShinyjs(),

  tags$head(
    tags$style(HTML("body { padding-top: 80px !important; }
                     .navbar { background-color: #e9ecef !important; color: #495057 !important; display: flex; align-items: center; }
                     .navbar-header { flex-grow: 1; display: flex; align-items: center; }
                     .navbar-brand { color: #495057 !important; font-weight: bold; display: flex; align-items: center; justify-content: flex-start; width: 100%; }
                     .navbar-toggler { margin-left: auto !important; }
                     .dataTables_wrapper { font-size: 75% !important; }
                     h1 { font-size: 1.5rem !important; }
                     .navbar-brand:hover { text-decoration: none !important; cursor: default !important; }
                     .navbar-brand img { height: 50px !important; width: auto !important; margin-right: 15px !important; }
                     .navbar-nav { margin-left: auto !important; }")) # Adjust padding, navbar color, table font size, title size, and disable hover effect
  ),

  navbarPage(
    title = div(class = "navbar-header",
                img(src = "misau-picture.png", style = "vertical-align: middle; height: 50px; margin-right: 10px;"),
                span("RShiny Aplicativo", style = "display: flex; align-items: center;")
    ),
    theme = shinytheme("flatly"),
    position = "fixed-top",
    collapsible = TRUE,

    tabPanel(
      "Home",
      titlePanel("sismar::"),

      mainPanel(
        p("O pacote sismar e aplicativo 'Shiny' foram criados para o Ministério da Saúde de Moçambique (MISAU) para apoiar a automatização do processamento de dados programáticos (por exemplo, SISMA, DISA, etc.).", style = "font-size: 16px; margin-top: 20px;"),
      ),

      sidebarLayout(
        sidebarPanel(
          fileInput("csv_file", "Escolha ficheiro CSV", accept = ".csv"),
          actionButton("process", "Processar Dados"),
          downloadButton("download", "Descarregar Novo Ficheiro")
        ),
        mainPanel(
          dataTableOutput("preview")
        )
      )
    )
  )
)

# Define server logic
server <- function(input, output, session) {

  # Reactive value to store processed data
  processed_data <- reactiveVal(NULL)

  observeEvent(input$process, {
    req(input$csv_file)

    # Pass the file path directly to process_sisma_export
    processed <- process_sisma_export(input$csv_file$datapath)

    # Store processed data
    processed_data(processed)
  })

  # Preview the processed data
  output$preview <- renderDataTable({
    req(processed_data())
    processed_data()
  })

  # Download processed data
  output$download <- downloadHandler(
    filename = function() { "processed_file.csv" },
    content = function(file) {
      req(processed_data())
      write_csv(processed_data(), file)
    }
  )
}

# Run the application
shinyApp(ui = ui, server = server)
