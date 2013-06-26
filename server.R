library(shiny)

priceData <- parts_df
# I don't think preprocesing has been done like log(price)... or factors 

# Define server logic required to plot various variables against price
shinyServer(function(input, output) {
  
  formulaText <- reactive({
    paste("price ~ ", input$variable)
  })
  
  output$caption <- renderText({
    paste("Price vs ", input$variable)
  })
  
  output$Plot <- renderPlot({
    # data <- subset(parts_df, select=input$variable)
    # some kind of if loop to filer? or use subset( , rsf == "F") or something
    X <- input$variable
    p <- ggplot() + geom_point(aes(x=parts_df[,as.character(X)], y=parts_df$price))#, alpha=.3)#+geom_abline(intercept=0, slope=0, colour="red") + ggtitle(paste("Price vs ", X)) + xlab(X) + ylab("Price")
    # plot(as.formula(formulaText()), data = priceData)
    print(p)
  })
  
})


# run the castings model app with the following code
# runApp("~/Documents/Intern_Model/Castings_Model")
