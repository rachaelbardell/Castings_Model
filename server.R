library(shiny)
library(ggplot2)

#priceData <- parts_df
# I don't think preprocesing has been done... like factors 
# log of price has already been taken
# how is a price zero? cannot take the log of these. Should these be taken out of the model?
# if we take the log of price in the model.... not all negative log(price) is bad... what if a part cost 2 cents
# so 1) there are negative price numbers in the data set being used to trai the model
# and 2) filtering (bounding) the training to non-negative is not right...

priceData <- subset(parts_df, price >= 0)
# cannot have negative price, when we took the log of price the parts with price = 0 became negative

# Define server logic required to plot various variables against price
shinyServer(function(input, output) {
  
#  formulaText <- reactive({
#    paste("price ~ ", input$variable)
#  })
  
  output$caption <- renderText({
    paste("Price ~ ", input$variable)
  })
  
  output$Plot <- renderPlot({
    # if statements for filtering. there is an example in this code http://www.rstudio.com/shiny/
    
    if(input$rsf != "Do Not Filter by RSF") { # case sensitive
      priceData <- subset(priceData, priceData$rsf == input$rsf) }
    
    # still need to filter by family... need fewer families - they seem redundant
    # can we combine some together?
    
    X <- as.character(input$variable)
    p <- ggplot() + geom_point(aes(x=priceData[, X], y=priceData$price), alpha=.3)+ ggtitle(paste("Price vs ", input$variable)) + xlab(paste0(input$variable, "\n\nNumber of Observations Plotted: ", nrow(priceData))) + ylab("Price")
    #plot(as.formula(formulaText()), data = priceData, xlab = paste0(input$variable, "\nNumber of Observations Plotted: ", nrow(priceData))) # this works...
    print(p)
    
    # plot does not change, it stays as boxvol (what I defined X to be outside of the server)
    # but the names change. and both use X...
    
  })
  
})


# run the castings model app with the following code
# runApp("~/Documents/Intern_Model/Shiny")