library(shiny)
library(plotly)

# Define UI for application
ui <- navbarPage("Growth Charts",
                 navbarMenu("Weight",
                            tabPanel("Boys",plotlyOutput("plotMaleWeight",height = "600")),
                            tabPanel("Girls",plotlyOutput("plotFemaleWeight",height="600"))),
                 navbarMenu("Height",
                            tabPanel("Boys",plotlyOutput("plotMaleHeight",height = "600")),
                            tabPanel("Girls",plotlyOutput("plotFemaleHeight",height="600")))
                 
                 
)
