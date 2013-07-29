library(shiny)
library(ggplot2)
library(caret)

# Returns true/false for each column in the data frame
# For graphing purposes, if there are more than 200 unique values the variable 
# is treated as continuous, less than 200 will be treated as categorical
test <- apply(priceData[,1:ncol(priceData)], 2, function(X)length(unique(X)) < 200)

binSlider <- c("exgearcount", "pressuretestair", "balance", "drillholes", "ports", "pressuretestwater")

# Define server logic required to plot various variables against price
shinyServer(function(input, output) {
  
  dataInput <- reactive({
          
          priceData <- subset(parts.df, parts.df$inModel, price >= 0)
          
          if(input$family != "Do Not Filter by Family") {
          priceData <- subset(priceData, priceData$family == input$family)}
          
          if(input$rsf != "Do Not Filter by RSF") { # case sensitive
          priceData <- subset(priceData, priceData$rsf == input$rsf) }
                
          if(input$logy){priceData$price <- log(priceData$price)}
          
          return(priceData)
                        })
  
  output$Plot <- renderPlot({
    priceData <- dataInput()
    ifelse(test[[input$variable]],
           # if true (ie the variable is categorical) construct a bar graph with counts
           p <- ggplot(priceData) + geom_bar(aes_string(x = input$variable), binwidth = input$binwidth) + ggtitle(input$variable) + xlab(paste0(input$variable, "\n\nNumber of Observations Plotted: ", nrow(priceData))) + ylab("Count"), # add binwidth = input$n to use binwidth slider        
           # if false (ie the variable is continuous) construct a scatterplot
           {if(input$logx) priceData[, input$variable] <- log(priceData[, input$variable])
                            p <- ggplot(priceData) + geom_point(aes_string(x = input$variable, y = "price"), alpha = 0.3) + stat_smooth(aes_string(x = input$variable, y = "price"), color="red", se = "FALSE") + ggtitle(paste("Price vs ", input$variable)) + xlab(paste0(input$variable, "\n\nNumber of Observations Plotted: ", nrow(priceData))) + ylab("Price")}
          )
    
    print(p)
                            })
  
  output$Histogram <- renderPlot({
    priceData <- dataInput()
    p <- ggplot(priceData)+geom_histogram(aes_string(x=input$variable))+ggtitle(paste("Histogram of " ,input$variable))
    print(p)
    })
  
  output$Table <- renderTable({
    priceData <- dataInput()
    # prints a table with 10 of the observations if the variable is continuous
    # and prints a table of the counts of the factors if the variable is categorical
    ifelse(test[[input$variable]],
            {tab <- table(priceData[, input$variable])
            return(tab)},
            {tab <- priceData[1:10, input$variable]
            return(as.table(tab))})
  })
  
  output$binwidth <- renderUI({
            priceData <- dataInput()
            max <- max(priceData[, input$variable])
            min <- min(priceData[, input$variable])
            bin <- (max - min) / 30
            if(input$variable %in% binSlider) return(sliderInput("binwidth", "Binwidth", min = round(min), max = round(max/5), value = bin, step = round(bin/5)))
            if(!test[[input$variable]]) return(checkboxInput("logx", "Log Transformation of X", FALSE))
                                })
  
  output$caption <- renderText({paste("Summary of ", input$variable)})      
  
  output$Summary <- renderPrint({
    priceData <- dataInput()
    summary(priceData[, input$variable])
                                })
})

# run the castings model app with the following code
# runApp("~/Documents/Intern_Model/Castings_Model")