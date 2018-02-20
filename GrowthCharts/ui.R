library(shiny)
library(plotly)

# Define UI for application
ui <- navbarPage("Growth Charts",
                 navbarMenu("Weight",
                            tabPanel("Boys",
                              
                              sidebarLayout(
                              sidebarPanel(
                                radioButtons("plotType1", label = h3("Graph Type"),
                                             choices = list("Measurement" = 1, "Z-Score" = 2), 
                                             selected = 1)
                              ),
                              mainPanel(plotlyOutput("plotMaleWeight",height = "600"))
                              )),
                            tabPanel("Girls",
                                     sidebarLayout(
                                       sidebarPanel(
                                         radioButtons("plotType2", label = h3("Graph Type"),
                                                      choices = list("Measurement" = 1, "Z-Score" = 2), 
                                                      selected = 1)
                                       ),
                                       mainPanel(plotlyOutput("plotFemaleWeight",height="600"))))
                 ),
                 navbarMenu("Height",
                            tabPanel("Boys",
                                     sidebarLayout(
                                       sidebarPanel(
                                         radioButtons("plotType3", label = h3("Graph Type"),
                                                      choices = list("Measurement" = 1, "Z-Score" = 2), 
                                                      selected = 1)
                                       ),
                                       mainPanel(plotlyOutput("plotMaleHeight",height = "600")))),
                            tabPanel("Girls",
                                     sidebarLayout(
                                       sidebarPanel(
                                         radioButtons("plotType4", label = h3("Graph Type"),
                                                      choices = list("Measurement" = 1, "Z-Score" = 2), 
                                                      selected = 1)
                                       ),
                                     mainPanel(plotlyOutput("plotFemaleHeight",height="600"))))
                 )
    )
                 
                 

