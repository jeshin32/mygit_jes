
library(shiny)

ui <- pageWithSidebar(
  headerPanel('Shiny Example'),
  
  sidebarPanel(width=3,
    selectizeInput("x", "Select X", choices=names(iris)),
    selectizeInput("y", "Select y", choices=names(iris)),
    
    sliderInput("pointSize", "Select Point Size", min=1, max=20, value=5),
    sliderInput("alpha", "Select Alpha", min=0, max=1, value=0.5)
    
  ),
  
  mainPanel(
    plotOutput('plot1', height="600px")
  )

  
)

server <- function(input, output, session) {
  
  output$plot1 <- renderPlot({
    library(ggplot2)
    ggplot(iris, aes_string(input$x, input$y, color = "Species")) +
      geom_point(size=input$pointSize, alpha=input$alpha)
  })
}

shinyApp(ui, server)
