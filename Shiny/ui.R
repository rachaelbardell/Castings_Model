library(shiny)

# Define UI for castings model application
# have to have headerPanel(), sidebarPanel(), and mainPanel()

priceData <- read.csv("/var/shiny-server/www/Shiny/castings_data.csv", header = TRUE, stringsAsFactors=FALSE)
priceData <- subset(priceData, priceData$inModel, price >= 0)

# all families with over 30 parts in that family to be displayed in a drop down menu
families <- table(priceData$family) # 105 families
families <- families[families > 30] # only 43 with over 30 observations
families <- c("Do Not Filter by Family", names(families))

medium <- table(priceData$medium)
medium <- c("Do Not Filter by Medium", names(medium))

shinyUI(pageWithSidebar(
  headerPanel("Castings Model"),
  
  # Sidebar with controls to choose a variable to plot against price and filtering options
  sidebarPanel(
          selectInput("variable", "Variable:", list("Air Pressure Test" = "pressuretestair", # integer 40
                                                    "Assembly"="is_assembly", # logical 2
                                                    "Balance"="balance", # 19
                                                    "Box Volume"="boxvol", # 9622
                                                    "Brazed Welded Spec Bins"="brazedweldedspecbins", # character 2
                                                    "Cleaning Spec"="cleaningspec", # character 20
                                                    "Core Volume"="corevol", # 10073
                                                    "Demand" = "transformed.demand", # 766
                                                    "Depth"="depth", # 6408
                                                    "Drill Holes"= "drillholes", # integer 136
                                                    "Drill Hole Volume"="drillholevol", # 6318
                                                    "Ex Gear Count"="exgearcount", # integer 6
                                                    "Heattreatspecbins" = "heattreatspecbins", # character 13
                                                    "Height"="height", # 7630
                                                    "Matspecbins" = "matspecbins", # character 40
                                                    "Mold Complexity" = "moldcomplexity", # 10690
                                                    "Number of Ports"="ports", # integer 13
                                                    "Parting Line Perimeter"="partinglineperim", # 8724
                                                    "Port Volume"="portvol", # 210?
                                                    "Surface Area"= "surfacea", # 10074
                                                    "Water Pressure Test"="pressuretestwater", # integer 26
                                                    "Weight"="weight", # 8663
                                                    "Width"="width") # 7137
    ),
          wellPanel(
                h5("Filtering:"),
                selectInput("family", "Family", families),
      
                selectInput("medium", "Medium", medium),
      
                selectInput("rsf", "RSF", list("Do Not Filter by RSF", "F", "R"))),
    
          checkboxInput("logy", "Log Transformation of Price", TRUE),
    
          uiOutput("bin_or_logx") # slider bar for binwith or check box for log(x) transformation
  ),
  
  mainPanel(
          tabsetPanel(
          tabPanel("Plot", plotOutput("Plot"), verbatimTextOutput("lambda")),
          tabPanel("Diagnostics", h5("Correlation Matrix"), plotOutput("Correlation")),
          tabPanel("Summary", h5(textOutput("caption")), verbatimTextOutput("Summary"), plotOutput("Histogram"), tableOutput("Table"))
    ))
))
