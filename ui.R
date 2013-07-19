library(shiny)

# Define UI for castings model application
# have to have headerPanel(), sidebarPanel(), and mainPanel()

# all families with over 30 parts in that family to be displayed in a drop down menu
families <- table(priceData$family)
families <- families[families > 30]
families <- c("Do Not Filter by Family", names(families))

#test <- apply(priceData[,1:ncol(priceData)], 2, function(X)length(unique(X)) < 200)
# return a list of variables that have more than 200 unique outcomes
#transformx <- sapply(names(priceData), function(x)if(test[[x]]=="FALSE") x)

shinyUI(pageWithSidebar(
       headerPanel("Castings Model"),
        
        # Sidebar with controls to choose a variable to plot against price and filtering options
        sidebarPanel(
          selectInput("variable", "Variable:", list("Box Volume"="boxvol", # 9622
                                                    "Depth"="depth", # 6408
                                                    "Height"="height", # 7630
                                                    "Width"="width", # 7137
                                                    "Weight"="weight", # 8663
                                                    "Core Volume"="corevol", # 10073
                                                    "Number of Ports"="ports", # integer 13
                                                    "Port Volume"="portvol", # 210?
                                                    "Balance"="balance", # 19
                                                    "Surface Area"= "surfacea", # 10074
                                                    "Parting Line Perimeter"="partinglineperim", # 8724
                                                    "Drill Holes"= "drillholes", # integer 136
                                                    "Drill Hole Volume"="drillholevol", # 6318
                                                    "Assembly"="is_assembly", # logical 2
                                                    "Ex Gear Count"="exgearcount", # integer 6
                                                    "Water Pressure Test"="pressuretestwater", # integer 26
                                                    "Air Pressure Test" = "pressuretestair", # integer 40
                                                    "Mold Complexity" = "moldcomplexity", # 10690
                                                    "Heattreatspecbins" = "heattreatspecbins", # character 13
                                                    "Cleaning Spec"="cleaningspec", # character 20
                                                    "Brazed Welded Spec Bins"="brazedweldedspecbins", # character 2
                                                    "Matspecbins" = "matspecbins", # character 40
                                                    "Demand" = "transformed.demand") # 766                   
                                                    ),
          wellPanel(
          selectInput("family", "Family", families),
          
          selectInput("rsf", "Filter by RSF?", list("Do Not Filter by RSF", "F", "R")),
          
          checkboxInput("logy", "Log Transformation of Price", TRUE),
          
          #conditionalPanel(condition= "input.variable in transformx",
                           checkboxInput("logx", "Transformation of X", FALSE)#,
          #                 #sliderInput("n", "Bin Width", value = 2, min = 1, max = 100)
          #                 )
                    )),    
       
        mainPanel(
          tabsetPanel(
              tabPanel("Plot", plotOutput("Plot")),
              tabPanel("Summary", h5(textOutput("caption")), verbatimTextOutput("Summary"), tableOutput("Table"))
                  ))
))