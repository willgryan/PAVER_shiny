library(shiny)
library(shinydashboard)
library(PAVER)
library(ggplot2)

# UI
ui <- dashboardPage(
  dashboardHeader(title = "PAVER Analysis"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Upload Data", tabName = "upload", icon = icon("upload")),
      menuItem("Settings", tabName = "settings", icon = icon("cogs")),
      menuItem("Results", tabName = "results", icon = icon("chart-bar"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "upload",
              fileInput("file", "Upload CSV", accept = ".csv"),
              textAreaInput("csvText", "Or paste CSV data here", rows = 10)
      ),
      tabItem(tabName = "settings",
              selectInput("embeddingPackage", "Choose Embedding Package", choices = c("Package A", "Package B")),
              sliderInput("paramA", "Clustering Parameter A", min = 1, max = 10, value = 5),
              sliderInput("paramB", "Clustering Parameter B", min = 1, max = 10, value = 5),
              actionButton("submit", "Submit")
      ),
      tabItem(tabName = "results",
              downloadButton("downloadReport", "Download Report"),
              uiOutput("plots")
      )
    )
  )
)

# Server
server <- function(input, output, session) {
  data <- reactive({
    if (!is.null(input$file)) {
      read.csv(input$file$datapath, stringsAsFactors = FALSE)
    } else if (input$csvText != "") {
      read.csv(text = input$csvText, stringsAsFactors = FALSE)
    }
  })
  
  analysis <- eventReactive(input$submit, {
    req(data())
    # Placeholder for PAVER analysis function
    # Replace this with the actual PAVER analysis call
    # paverResult <- paver(data(), embedding_package = input$embeddingPackage, paramA = input$paramA, paramB = input$paramB)
    
    # For demonstration, creating dummy plots
    list(
      plot1 = ggplot(mtcars, aes(wt, mpg)) + geom_point(),
      plot2 = ggplot(mtcars, aes(wt, qsec)) + geom_point(),
      plot3 = ggplot(mtcars, aes(wt, hp)) + geom_point(),
      plot4 = ggplot(mtcars, aes(wt, drat)) + geom_point()
    )
  })
  
  output$plots <- renderUI({
    req(analysis())
    plot_output_list <- lapply(1:4, function(i) {
      plotname <- paste("plot", i, sep="")
      plotOutput(plotname, height = 300)
    })
    do.call(tagList, plot_output_list)
  })
  
  output$plot1 <- renderPlot({
    req(analysis())
    analysis()$plot1
  })
  output$plot2 <- renderPlot({
    req(analysis())
    analysis()$plot2
  })
  output$plot3 <- renderPlot({
    req(analysis())
    analysis()$plot3
  })
  output$plot4 <- renderPlot({
    req(analysis())
    analysis()$plot4
  })
  
  output$downloadReport <- downloadHandler(
    filename = function() {
      paste("PAVER_analysis_report", Sys.Date(), ".pdf", sep="")
    },
    content = function(file) {
      pdf(file)
      for (i in 1:4) {
        print(analysis()[[paste("plot", i, sep="")]])
      }
      dev.off()
    }
  )
}

# Run the application 
shinyApp(ui = ui, server = server)
