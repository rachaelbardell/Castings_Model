library(shiny)
library(ggplot2)

# Returns true/false for each column in the data frame
# For graphing purposes, if there are more than 200 unique values the variable 
# is treated as continuous, less than 200 will be treated as categorical
test <- apply(priceData[,1:ncol(priceData)], 2, function(X)length(unique(X)) < 200)

# Define server logic required to plot various variables against price
shinyServer(function(input, output) {
  
  dataInput <- reactive({
          # price cannot be negative
          priceData <- subset(parts_df, price >= 0)
          
          if(input$family != "Do Not Filter by Family") {
          priceData <- subset(priceData, priceData$family == input$family)}
          
          if(input$rsf != "Do Not Filter by RSF") { # case sensitive
          priceData <- subset(priceData, priceData$rsf == input$rsf) }
            
          if(input$logy) {priceData$price <- log(priceData$price)}
          
          if(input$logx) {
            ifelse(test[[input$variable]],
                   ,
                   priceData[,input$variable] <- log(priceData[, input$variable]+1))
                                }
          
          return(priceData)
                        })
  
  output$Plot <- renderPlot({
    priceData <- dataInput()
    # scatterplot vs price if the variable is continuous
    # bar graph with counts if the variable is categorical
    ifelse(test[[input$variable]],
            p <- ggplot(priceData) + geom_bar(aes_string(x = input$variable)) + ggtitle(input$variable) + xlab(paste0(input$variable, "\n\nNumber of Observations Plotted: ", nrow(priceData))) + ylab("Count"), # add binwidth = input$n to use binwidth slider        
            p <- ggplot(priceData) + geom_point(aes_string(x = input$variable, y = "price"), alpha = 0.3) + stat_smooth(aes_string(x = input$variable, y = "price"), color="red", se = "FALSE") + ggtitle(paste("Price vs ", input$variable)) + xlab(paste0(input$variable, "\n\nNumber of Observations Plotted: ", nrow(priceData))) + ylab("Price")
          )
    
    print(p)
                            })
  
  output$Table <- renderTable({
    priceData <- dataInput()
    # prints a table with all of the observations if the variable is continuous
    # and prints a table of the counts if the variable is categorical
    ifelse(test[[input$variable]],
            {tab <- table(priceData[, input$variable])
            return(tab)},
            {tab <- priceData[, input$variable]
            return(as.table(tab))})
  })
  
  output$caption <- renderText({
    paste0("Summary of ", input$variable)
                              })      
  
  output$Summary <- renderPrint({
    priceData <- dataInput()
    summary(priceData[, input$variable])
                                })
})

# run the castings model app with the following code
# runApp("~/Documents/Intern_Model/Shiny")