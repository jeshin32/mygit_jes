
library(shiny)

ui <- pageWithSidebar(
  headerPanel('Shiny Exercise'),
  
  sidebarPanel(width=3,
    numericInput("Mean", "Input Mean : ", value = 0),
    numericInput("SD", "Input Standart Deviation : ", value = 1),
    
    checkboxInput("chkDensity", "Desity Plot Option : ", value = T)

  ),
  
  mainPanel(
    plotOutput('plot1', height="600px")
  )
)

server <- function(input, output, session) {
  
  output$plot1 <- renderPlot({
    library(ggplot2)
    
    df = data.frame(data = rnorm(5000, as.numeric(input$Mean), as.numeric(input$SD)))
    
    ggplot(df, aes(data)) +
      geom_histogram(bins = 15, fill = "skyblue", color = "blue") +
      xlim(c(-10, 10))
  })
}

shinyApp(ui, server)

# df = data.frame(data = rnorm(5000, as.numeric(input$Mean), as.numeric(input$SD)))
# 
# ggplot(df, aes(data)) +
#   geom_histogram(bins = 15, fill = "skyblue", color = "blue")
