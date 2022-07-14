library(shiny)


ui <- fluidPage(
    titlePanel("Linear Modeling Data"),
  sidebarLayout(
    sidebarPanel(
        actionButton("button", "Plot Linear Model"),
      fileInput("file1", "Choose CSV File",
        accept = c(
          "text/csv",
          "text/comma-separated-values,text/plain",
          ".csv")
                ),
       
    ),
      mainPanel(
          plotOutput("scatter"),
          plotOutput("linearmodel")
          tableOutput("modeltable")
         
          )
      )
    )


server <- function(input, output) {
    library(ggplot2)
    model <- lm(dataInput()$y~dataInput()$x, data=dataInput())
    
    dataInput <- reactive({
        req(input$file1)
        df <- read.csv(input$file1$datapath)
        return(df)})
    
  output$scatter <- renderPlot(
  ggplot() +
    geom_point(aes(x = dataInput()$x, y = dataInput()$y),
            colour = 'red') +
    ggtitle('X vs Y') +
    xlab('X') +
    ylab('Y'))
    
    observeEvent(input$button, {
    output$linearmodel <- renderPlot(
        ggplot(dataInput(), aes(x = dataInput()$x, y = dataInput()$y)) +
    geom_point(color = 'red') +
    stat_smooth(method = "lm", se=FALSE, color = "blue") +
        ggtitle('Linear Regression of X vs Y') +
        xlab('X') +
        ylab('Y')
        ),
        output$modeltable <- renderTable(
            summary(model))
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
