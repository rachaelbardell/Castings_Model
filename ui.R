library(shiny)

# -----------
# Define UI for castings model application
# have to have headerPanel(), sidebarPanel(), and mainPanel()
shinyUI(pageWithSidebar(
        headerPanel("Castings Model"),
        
        # Sidebar with controls to choose a variable to plot against price and filtering options
        sidebarPanel(
          # only variables that are in the final model...
          selectInput("variable", "Variable:", list("Box Volume"="boxvol",
                                                    "Depth"="depth",
                                                    "Height"="height",
                                                    "Width"="width",
                                                    "Balance"="balance",
                                                    "Surface Area"= "surfacea",
                                                    "Parting Line Perimeter"="partinglineperim",
                                                    "Drill Holes"= "drillholes",
                                                    "Pressure Air Test" = "pressureairtest",
                                                    "Mold Complexity" = "moldcomplexity",
                                                    "Heattreatspecbins" = "heattreatspecbins",
                                                    "Matspecbins" = "matspecbins",
                                                    "Demand" = "transformed.demand")                   
                      ),
         
          selectInput("family", "Family", list("Adapters"="ADAPTERS",
                                               "Process Capable Casting" = "ANY PROCESS CAPABLE CASTING",
                                               "Bearing Cages", "BEARING CAGES",
                                               "Bearing Caps"= "BEARING CAPS",
                                               "Bodies"= "BODIES - INCLUDES PUMP BODIES AND PUMP MANIFOLDS",
                                               "Bonnets"="BONNETS",
                                               "Brackets, Braces, and Supports"= "BRACKETS, BRACES AND SUPPORTS",
                                               "Covers"= "COVERS - EXCEPT TRANSMISSION COVERS",
                                               "Carriers" = " DIFFERENTIAL CARRIER / PLANET CARRIEERS / CLUTH CARRIERS",
                                               "Block and Head Castings"= "FERROUS NONFERROUS BLOCK AND HEAD CASTINGS"
                                               # haven't finished these...
                                               )),
          
          checkboxInput("rsf", "Filter by RSF", value = TRUE)
          
        ),
        
        mainPanel( # something is wrong with this part I think, neither of these show up. and R has to terminate and restart every time... annoying. But there is not an error with the plot which is good
          h3(textOutput("caption")),
          plotOutput("Plot")
          )
))

# how to use the them (rsf and family) as filters...
# may have to go through familyname column and combine some in R??? 
