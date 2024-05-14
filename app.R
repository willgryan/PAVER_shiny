library(tidyverse)
library(shiny)
library(shinydashboard)
library(PAVER)
library(shinyjs)

# UI
ui <- dashboardPage(
  dashboardHeader(title = "PAVER"),
  dashboardSidebar(sidebarMenu(
    menuItem("About", tabName = "about", icon = icon("info-circle")),
    menuItem("Upload Data", tabName = "upload", icon = icon("upload")),
    menuItem("Settings", tabName = "settings", icon = icon("cogs")),
    menuItem("Results", tabName = "results", icon = icon("chart-bar"))
  )),
  dashboardBody(
    useShinyjs(),
    tabItems(
      tabItem(
        tabName = "about",
        h4("About PAVER"),
        p("PAVER (Pathway Analysis Visualization with Embedding Representations) is a bioinformatics tool to help interpret and visualize pathway analysis results."),
        p("To use the app, follow these steps:"),
        tags$ol(
          tags$li("Navigate to the 'Upload Data' tab to upload your CSV file or paste CSV data directly into the text area."),
          tags$li("Go to the 'Settings' tab to select your desired pre-computed embedding package and adjust clustering settings."),
          tags$li("Click the 'Submit' button to start the analysis."),
          tags$li("View and download the generated plots and clustering results from the 'Results' tab.")
        )
      ),
      tabItem(
        tabName = "upload",
        h4("Upload your own CSV file or paste CSV data below."),
        fileInput("file", NULL, accept = ".csv"),
        textAreaInput("csvText", NULL, rows = 10),
        actionButton(label = "Load example data", inputId = "loadExample")
      ),
      tabItem(
        tabName = "settings",
        selectInput("embeddingPackage", "Choose Embedding", choices = c("Gene Ontology", "KEGG")),
        sliderInput("minClusterSize", "minClusterSize", min = 3, max = 100, value = 3),
        sliderInput("maxCoreScatter", "maxCoreScatter", min = .1, max = .99, value = .33),
        actionButton("submit", "Submit")
      ),
      tabItem(
        tabName = "results",
        downloadButton("downloadReport", "Download Figures"),
        downloadButton("downloadResults", "Download Results"),
        uiOutput("plots")
      )
    )
  )
)

# Server
server <- function(input, output, session) {
  data <- reactive({
    if (input$csvText != "") read_csv(input$csvText)
  })
  
  shinyjs::disable("downloadReport")
  shinyjs::disable("downloadResults")
  
  observeEvent(input$loadExample, {
    example_data <- PAVER::gsea_example %>% filter(across(everything(), ~ . != 0))
    example_text <- capture.output(write.csv(example_data, row.names = FALSE, quote = FALSE))
    updateTextAreaInput(session, "csvText", value = paste(example_text, collapse = "\n"))
  })
  
  observeEvent(input$file, {
    req(input$file)
    file_data <- read_csv(input$file$datapath)
    file_text <- capture.output(write.csv(file_data, row.names = FALSE, quote = FALSE))
    updateTextAreaInput(session, "csvText", value = paste(file_text, collapse = "\n"))
  })
  
  analysis <- eventReactive(input$submit, {
    req(data())
    shinyjs::disable("submit")
    shinyjs::disable("downloadReport")
    shinyjs::disable("downloadResults")
    shinyjs::hide("plots")
    
    withProgress(message = 'Grabbing embeddings...', value = 0, {
      embeddings_url <- "https://github.com/willgryan/PAVER_embeddings/raw/main/2023-03-06/embeddings_2023-03-06.RDS"
      term2name_url <- "https://github.com/willgryan/PAVER_embeddings/raw/main/2023-03-06/term2name_2023-03-06.RDS"
      embeddings <- readRDS(url(embeddings_url))
      term2name <- readRDS(url(term2name_url))
      
      incProgress(0.2, detail = "Preparing data...")
      PAVER_result <- PAVER::prepare_data(data(), embeddings, term2name)
      
      incProgress(0.4, detail = "Generating themes...")
      minGap <- (1 - input$maxCoreScatter) * 3 / 4
      PAVER_result <- PAVER::generate_themes(PAVER_result, minClusterSize = input$minClusterSize, maxCoreScatter = input$maxCoreScatter, minGap = minGap)
      
      incProgress(0.6, detail = "Creating plots...")
      plots <- list(
        plot1 = PAVER_theme_plot(PAVER_result),
        plot2 = PAVER_interpretation_plot(PAVER_result),
        plot3 = PAVER_regulation_plot(PAVER_result),
        plot4 = PAVER_hunter_plot(PAVER_result)
      )
      
      incProgress(0.8, detail = "Exporting results...")
      clustered_input <- PAVER_export(PAVER_result)
      
      incProgress(1, detail = "Done")
      shinyjs::enable("downloadReport")
      shinyjs::enable("downloadResults")
      shinyjs::enable("submit")
      shinyjs::reset("csvText")
      shinyjs::reset("file")
      shinyjs::show("plots")
      
      list(plots = plots, clustered_input = clustered_input)
    })
  })
  
  output$plots <- renderUI({
    req(analysis())
    plot_output_list <- lapply(1:4, function(i) plotOutput(paste0("plot", i), height = 300))
    do.call(tagList, plot_output_list)
  })
  
  lapply(1:4, function(i) {
    output[[paste0("plot", i)]] <- renderPlot({
      req(analysis())
      analysis()$plots[[paste0("plot", i)]]
    })
  })
  
  output$downloadReport <- downloadHandler(
    filename = function() paste0("PAVER_analysis_report_", Sys.Date(), ".pdf"),
    content = function(file) {
      pdf(file)
      lapply(1:4, function(i) print(analysis()$plots[[paste0("plot", i)]]))
      dev.off()
    }
  )
  
  output$downloadResults <- downloadHandler(
    filename = function() paste0("PAVER_clustering_results_", Sys.Date(), ".csv"),
    content = function(file) write_csv(analysis()$clustered_input, file)
  )
}

shinyApp(ui = ui, server = server)