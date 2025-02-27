library(shiny)
library(tidyverse)
library(sismar)
library(janitor)
library(shinythemes)
library(shinyjs)
library(DT)  # Ensure DT is loaded

# Define UI for the Shiny app
ui <- fluidPage(
  useShinyjs(),
  theme = shinytheme("flatly"),

  tags$head(
    # Load Google Fonts (Montserrat & Lato)
    tags$link(href="https://fonts.googleapis.com/css2?family=Montserrat:wght@300;400;700&family=Lato:wght@300;400;700&display=swap", rel="stylesheet"),

    tags$style(HTML("
      /* Apply Montserrat as the base font */
      body {
        font-family: 'Montserrat', sans-serif;
        padding-top: 110px;
        margin-left: 15px;
        margin-right: 15px;
      }

      /* Apply Lato for headings */
      h1, h2, h3, h4, h5, h6, .intro-heading {
        font-family: 'Lato', sans-serif;
      }

      /* Full-width fixed banner */
      .banner {
        background-color: #2c3e50;
        color: white;
        padding: 20px 25px;
        text-align: left;
        font-size: 60px;
        font-weight: bold;
        width: 100vw;
        height: 75px;
        position: fixed;
        top: 0;
        left: 0;
        z-index: 1000;
        display: flex;
        align-items: center;
        justify-content: space-between;
      }

      .banner img {
        height: 40px;
      }

      /* Icon-only Menu Button */
      .menu-container {
        position: relative;
      }

      .menu-button {
        background-color: transparent;
        color: white;
        border: none;
        font-size: 24px;
        cursor: pointer;
        padding: 5px 10px;
      }

      .menu-dropdown {
        display: none;
        position: absolute;
        right: 0;
        background-color: white;
        min-width: 120px;
        box-shadow: 0px 4px 8px rgba(0, 0, 0, 0.2);
        border-radius: 5px;
        z-index: 1001;
      }

      .menu-dropdown a {
        display: block;
        padding: 5px;
        color: #2c3e50;
        text-decoration: none;
        font-size: 12px;
      }

      .menu-dropdown a:hover {
        background-color: #f1f1f1;
      }

      .show-menu {
        display: block !important;
      }

      .intro-heading {
        font-size: 20px;
        font-weight: bold;
        margin-bottom: 5px;
      }

      .intro-text {
        font-size: 16px;
        margin-bottom: 15px;
        width: 95vw;
      }

      .btn-container {
        display: flex;
        gap: 10px;
        margin-top: 5px;
        margin-bottom: 25px;
      }

      .btn-primary { width: 180px; }
      .btn-secondary { width: 250px; }

      /* Make preview text smaller */
      .dataTables_wrapper { font-size: 75% !important; }

      .top-container {
        display: flex;
        align-items: center;
        gap: 15px;
        justify-content: flex-start;
      }

      .dataTables_length, .dataTables_filter {
        margin-bottom: 0 !important;
      }
    "))
  ),

  # Full-width fixed banner with logo + text + menu
  div(class = "banner",
      "sismar::",

      # Menu Button (Icon Only)
      div(class = "menu-container",
          actionButton("menu_button", "☰", class = "menu-button"),  # Icon Only
          div(id = "menu_dropdown", class = "menu-dropdown",
              a("Home", href = "#"),
              a("Reference", href = "#")
          )
      )
  ),

  # Stacked layout, left-aligned
  fluidRow(
    column(6,
           p("Aplicativo de Processamento de Dados MISAU", class = "intro-heading"),
           p("A análise eficiente de dados rectangulares exportados dos sistemas de informação de saúde do MISAU requer acções de processamento tais como pivotagem, eliminação/coerção de variáveis e engenharia de caraterísticas de dados úteis na análise. O pacote sismar e aplicativo 'Shiny' foram criados para o Ministério da Saúde de Moçambique (MISAU) para apoiar a automatização do processamento de dados programáticos.",
             class = "intro-text")
    )
  ),

  fluidRow(
    column(6,
           fileInput("csv_file", "Escolha ficheiro CSV", accept = ".csv"),

           # Unified button container with extra space below
           div(class = "btn-container",
               actionButton("process", "Processar Dados", class = "btn btn-primary"),
               downloadButton("download", "Descarregar Novo Ficheiro", class = "btn btn-secondary")
           )
    )
  ),

  # Data preview (hidden initially)
  fluidRow(
    column(10,
           div(id = "preview-container", style = "display: none;",
               dataTableOutput("preview")
           )
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  processed_data <- reactiveVal(NULL)

  observeEvent(input$process, {
    req(input$csv_file)
    processed <- process_sisma_export(input$csv_file$datapath)
    processed_data(processed)

    # Show preview container when data is available
    show("preview-container")
  })

  output$preview <- renderDataTable({
    req(processed_data())
    datatable(
      processed_data(),
      options = list(
        dom = '<"top-container"l f>rtip'
      )
    )
  })

  output$download <- downloadHandler(
    filename = function() { "processed_file.csv" },
    content = function(file) {
      req(processed_data())
      write_csv(processed_data(), file)
    }
  )

  # Toggle menu visibility when clicking the menu button
  observeEvent(input$menu_button, {
    session$sendCustomMessage("toggleMenu", "menu_dropdown")
  })
}

# JavaScript for toggling the menu visibility
jsCode <- "
Shiny.addCustomMessageHandler('toggleMenu', function(id) {
  var menu = document.getElementById(id);
  if (menu.classList.contains('show-menu')) {
    menu.classList.remove('show-menu');
  } else {
    menu.classList.add('show-menu');
  }
});
"

# Include JavaScript in UI
ui <- tagList(
  singleton(tags$head(tags$script(HTML(jsCode)))),
  ui
)

# Run the application
shinyApp(ui = ui, server = server)
