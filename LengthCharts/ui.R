library(shiny)
library(plotly)

# Define UI for application
ui <- navbarPage("Height Growth Charts",
                 mainPanel(
                   tabsetPanel(type="tab",
                               tabPanel("Boys",plotlyOutput("plotMale",height = "600")),
                               tabPanel("Girls",plotlyOutput("plotFemale",height="600"))
                               
                   ))
)