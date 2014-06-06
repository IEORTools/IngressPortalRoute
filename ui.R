library(shiny) 

# Define UI for dataset viewer application
shinyUI(pageWithSidebar(
  
  # Application title
  headerPanel("DFW Ingress Optimal Portal Route"),
  
  # Sidebar with controls to select a dataset and specify the number
  # of observations to view
  sidebarPanel(
  
    numericInput("lat", "latitude:", 32.986132),
    numericInput("lng", "longitude", -96.745909),
    
    sliderInput("maxPortals", "Max Number of Portals:",
                min=10, max=20, value=15, step=5)
  ),
  
  # Show a summary of the dataset and an HTML table with the requested
  # number of observations
  mainPanel(
    plotOutput("portalrouteMap"),
    tableOutput("portalrouteTbl")
  )
))