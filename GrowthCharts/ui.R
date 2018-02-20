library(shiny)
library(plotly)

# Define UI for application
ui <- navbarPage("Growth Charts",
                 navbarMenu("Weight",
                            tabPanel("Boys",
                              
                              sidebarLayout(
                              sidebarPanel(
                                radioButtons("plotType", label = h3("Graph Type"),
                                             choices = list("Measurement" = 1, "Z-Score" = 2), 
                                             selected = 1)
                              ),
                              mainPanel(plotlyOutput("plotMaleWeight",height = "600"))
                              )),
                            tabPanel("Girls",plotlyOutput("plotFemaleWeight",height="600"))),
                 navbarMenu("Height",
                            tabPanel("Boys",plotlyOutput("plotMaleHeight",height = "600")),
                            tabPanel("Girls",plotlyOutput("plotFemaleHeight",height="600")))
                 
                 
)
