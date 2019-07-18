library(shiny)
library(ggplot2)

ui <- pageWithSidebar(
  headerPanel('Shiny Example'),
  
  sidebarPanel(width=3,
     numericInput("mean", "Input Mean : ", 0),
     numericInput("sd", "Input sd : ", 1, min=0.1, max=2, step=0.1),
     
     checkboxInput("density", "Select density : ", TRUE)
     
  ),
  
  mainPanel(
    plotOutput('plot1', height="600px")
  )
)

server <- function(input, output, session) {
  
  output$plot1 <- renderPlot({
    
    # browser()
    d <- rnorm(1000, input$mean, input$sd)
    df <- data.frame(data = d)
    
    
    g <- ggplot(df, aes(data, ..density..)) +
      geom_histogram(color="white", fill="skyblue") +
      xlim(c(-10,10))
    
    if(input$density) {
      g <- g + geom_density(color="red")
    }
    g
    
  })
}

shinyApp(ui, server)
