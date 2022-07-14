library(shiny)
library(ggplot2)


ui <- fluidPage(
    titlePanel("Linear Modeling Data"),
  sidebarLayout(
    sidebarPanel(
      fileInput("file1", "Choose CSV File",
        accept = c(
          "text/csv",
          "text/comma-separated-values,text/plain",
          ".csv")
                ),
        actionButton("button", "Plot Linear Model")
       
    ),
      mainPanel(
          plotOutput("scatter"),
          plotOutput("linearmodel")
         
          )
      )
    )


server <- function(input, output) {
#     model <- lm(dataInput()$y~dataInput()$x, data=dataInput())
#     model_summary <- summary(model)
    
    dataInput <- reactive({
            req(input$file1)
        df <- read.csv(input$file1$datapath)
        return(df)
    })

   output$scatter <- renderPlot({
      ggplot() +
     geom_point(aes(x = dataInput()$x, y = dataInput()$y),
            colour = 'red') +
    ggtitle('X vs Y') +
    xlab('X') +
    ylab('Y')
   })

    
    observeEvent(input$button, {

        output$linearmodel <- renderPlot({
            
            lmmodel <- lm(dataInput()$y ~ dataInput()$x, data = dataInput())
            model_summary <- summary(lmmodel)
            slope_value <- model_summary$coefficients[1,1]
            Label = paste("slope = ", slope_value)
            
            intercept_value <- model_summary$coefficients[2,1]
            Label2 = paste("b = ", intercept_value)
            
            R_squared <- model_summary$r.squared
            Label3 = paste("R squared =", R_squared)
            
        ggplot(dataInput(), aes(x = dataInput()$x, y = dataInput()$y)) +
        geom_point(color = 'red') +
        stat_smooth(method = "lm", se=FALSE, color = "blue") +
        ggtitle('Linear Regression of X vs Y') +
        xlab('X') +
        ylab('Y') +
            annotate("text", x = 20, y = 5, label = Label) +
            annotate("text", x = 20, y = 6, label = Label2) +
            annotate("text", x = 20, y = 4, label = Label3)
        })
        })
        
}
                 
# Run the application 
shinyApp(ui = ui, server = server)
