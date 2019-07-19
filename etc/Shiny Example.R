library(shiny)
library(DT)
ui <- pageWithSidebar(
  headerPanel('Shiny Example'),
  
  sidebarPanel(width=3,
               selectizeInput("x", "Select X", choices=names(iris)),
               selectizeInput("y", "Select y", choices=names(iris)),
               sliderInput("size", "Select Size", min=1, max=20, value=5, animate=T),
               sliderInput("alpha", "Select a", min=0, max=1, value=0.5),
               fileInput("file", label = h3("File input"))
  ),
  
  mainPanel(
    plotOutput('plot1', height="400px"),
    dataTableOutput("table")
  )  
)

server <- function(input, output, session) {
  
  output$plot1 <- renderPlot({
    library(ggplot2)
    ggplot(iris, aes_string(input$x, input$y, color = "Species")) +
      geom_point(size=input$size, alpha=input$alpha)
  })
  
  output$table <- renderDataTable({
    
    # print(input$file)
    # browser()
    data <- read.csv(input$file$datapath)
    
    datatable(data)
  })
}

shinyApp(ui, server)
