library(tidyverse)
library(shiny)
library(shinydashboard)
library(PAVER)
library(shinyjs)

set.seed(123)

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
        p("PAVER (Pathway Analysis Visualization with Embedding Representations) is a bioinformatics tool to help interpret and visualize pathway analysis results from omics studies."),
        p("To use the app, please follow these steps:"),
        tags$ol(
          tags$li("Navigate to the 'Upload Data' tab to upload your CSV file or paste CSV data directly into the text area."),
          tags$li("Go to the 'Settings' tab to select your desired pre-computed embedding package and adjust clustering settings."),
          tags$li("Click the 'Submit' button to start the analysis."),
          tags$li("View and download the generated plots and clustering results from the 'Results' tab.")
        ),
        p("For more detailed information on PAVER and its functionalities, please refer to the article on the",
          tags$a(href = "https://cdrl-ut.org/project/projects_and_posters/paver/", "Cognitive Disorders Research Laboratory (CDRL)"),
          " website.")
      ),
      tabItem(
        tabName = "upload",
        h4("Upload your own CSV file or paste CSV data below."),
        fileInput("file", NULL, accept = ".csv"),
        textAreaInput("csvText", NULL, rows = 10),
        actionButton(label = "Load example GSEA data", inputId = "loadExampleGSEA"),
        actionButton(label = "Load example KEGG data", inputId = "loadExampleKEGG")
        
      ),
      tabItem(
        tabName = "settings",
        p("Select the appropriate pre-computed embeddings for your pathways used for input."),
        selectInput("embeddingPackage", "Choose Embedding", choices = c("GO_OpenAI_2_14_24", "GO_anc2vec_2_14_24", "GO_anc2vec_3_06_23", "KEGG_OpenAI_8_25_23")),
        p("This controls the minimum number of pathways that can be considered a cluster."),
        sliderInput("minClusterSize", "minClusterSize", min = 3, max = 100, value = 3),
        p("This controls the granularity of the clustering. Smaller numbers will produce less, broader clusters while larger numbers will produce more, specific clusters."),
        sliderInput("maxCoreScatter", "maxCoreScatter", min = .1, max = .99, value = .55),
        p("After clicking submit, navigate to the 'Results' tab to view clustering results."),
        actionButton("submit", "Submit"),
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
  
  observe({
    if (input$csvText != "" | !is.null(input$file)) {
      shinyjs::enable("submit")
    } else {
      shinyjs::disable("submit")
    }
  })
  
  shinyjs::disable("downloadReport")
  shinyjs::disable("downloadResults")
  shinyjs::disable("submit")
  
  observeEvent(input$loadExampleGSEA, {
    example_data <- PAVER::gsea_example %>% filter(across(everything(), ~ . != 0))
    example_text <- capture.output(write.csv(example_data, row.names = FALSE, quote = FALSE))
    updateTextAreaInput(session, "csvText", value = paste(example_text, collapse = "\n"))
  })
  
  observeEvent(input$loadExampleKEGG, {
    example_data <- PAVER::kegg_example %>% filter(across(everything(), ~ . != 0))
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
      if(input$embeddingPackage == "GO_OpenAI_2_14_24") {
        embeddings_url <- "data/embeddings/go/GO_embeddings_text-embedding-3-large_2_14_24_pca200n.rds"
        term2name_url <- "data/embeddings/go/GO_embeddings_text-embedding-3-large_2_14_24_term2name.rds"
      }
      
      if(input$embeddingPackage == "GO_anc2vec_2_14_24") {
        embeddings_url <- "data/embeddings/go/anc2vec_2_14_24.rds"
        term2name_url <- "data/embeddings/go/anc2vec_2_14_24_term2name.rds"
      }
      
      if(input$embeddingPackage == "GO_anc2vec_3_06_23") {
        embeddings_url <- "data/embeddings/go/anc2vec_2023_03_06.rds"
        term2name_url <- "data/embeddings/go/anc2vec_term2name_2023-03-06.RDS"
      }
      
      if(input$embeddingPackage == "KEGG_OpenAI_8_25_23") {
        embeddings_url <- "data/embeddings/kegg/KEGG_embeddings_text-embedding-ada-002_8_25_23.rds"
        term2name_url <- "data/embeddings/kegg/KEGG_embeddings_text-embedding-ada-002_8_25_23_term2name.rds"
      }
      
      embeddings <- readRDS(embeddings_url)
      term2name <- readRDS(term2name_url)
      
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