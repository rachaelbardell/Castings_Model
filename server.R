library(shiny)
library(ggplot2)
library(caret)

# Returns true/false for each column in the data frame
# For graphing purposes, if there are more than 200 unique values the variable 
# is treated as continuous, less than 200 will be treated as categorical
test <- apply(priceData[,1:ncol(priceData)], 2, function(X)length(unique(X)) < 200)

# list of categorical variables with numeric factors
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
  
  # histogram of the input variable
  output$Histogram <- renderPlot({
    priceData <- dataInput()
    if(input$logx) priceData[, input$variable] <- log(priceData[, input$variable])
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
            {if(input$logx) priceData[, input$variable] <- log(priceData[, input$variable])
             tab <- priceData[1:10, input$variable]
             return(as.table(tab))})
  })
  
  output$caption <- renderText({paste("Summary of ", input$variable)})      
  
  # summary of the input variable
  output$Summary <- renderPrint({
    priceData <- dataInput()
    if(input$logx) priceData[, input$variable] <- log(priceData[, input$variable])
    summary(priceData[, input$variable])
                                })
  
  # slider bar for binwidth if a bar graph is made (and the factors are numeric)
  # log(x) transformation checkbox if a scatterplot is made
  output$bin_or_logx <- renderUI({
    priceData <- dataInput()
    if(input$variable %in% binSlider) {
      max <- max(priceData[, input$variable])
      min <- min(priceData[, input$variable])
      bin <- (max - min) / 30
      return(sliderInput("binwidth", "Binwidth", min = round(min), max = round(max/5), value = round(bin, 0)))}#, step = round((max-min)/5, 1)))}
    if(!test[[input$variable]]) return(checkboxInput("logx", "Log Transformation of X", FALSE))
  })

})

# run the castings model app with the following code
# runApp("~/Documents/Intern_Model/Castings_Model")