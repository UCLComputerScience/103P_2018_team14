library(shiny)
library(plotly)
library(BH)
childrenData <- read.csv("ChildrenData.csv",header = TRUE)
childrenData <- subset(childrenData,Months<=60)
maleChildrenData <- subset(childrenData,sex==1)
femaleChildrenData <- subset(childrenData,sex==2)

# Define UI for application
ui <- navbarPage("Growth Charts",
                 navbarMenu("Weight",
                            tabPanel("Boys",
                              
                              sidebarLayout(
                              sidebarPanel(
                                radioButtons("plotType1", label = h3("Graph Type"),
                                             choices = list("Measurement" = 1, "Z-Score" = 2), 
                                             selected = 1),
                                selectInput("boysWT","Child ID",unique(c(maleChildrenData$id),selected=NULL, multiple = FALSE))
                              ),
                              mainPanel(plotlyOutput("plotMaleWeight",height = "600"))
                              )),
                            tabPanel("Girls",
                                     sidebarLayout(
                                       sidebarPanel(
                                         radioButtons("plotType2", label = h3("Graph Type"),
                                                      choices = list("Measurement" = 1, "Z-Score" = 2), 
                                                      selected = 1),
                                         selectInput("girlsWT","Child ID", unique(c(femaleChildrenData$id),selected=NULL, multiple = FALSE))
                                         
                                       ),
                                       mainPanel(plotlyOutput("plotFemaleWeight",height="600"))))
                 ),
                 navbarMenu("Height",
                            tabPanel("Boys",
                                     sidebarLayout(
                                       sidebarPanel(
                                         radioButtons("plotType3", label = h3("Graph Type"),
                                                      choices = list("Measurement" = 1, "Z-Score" = 2), 
                                                      selected = 1),
                                         selectInput("boysHT","Child ID", unique(c(maleChildrenData$id),selected=NULL, multiple = FALSE))
                                         
                                       ),
                                       mainPanel(plotlyOutput("plotMaleHeight",height = "600")))),
                            tabPanel("Girls",
                                     sidebarLayout(
                                       sidebarPanel(
                                         radioButtons("plotType4", label = h3("Graph Type"),
                                                      choices = list("Measurement" = 1, "Z-Score" = 2), 
                                                      selected = 1),
                                         selectInput("girlsHT","Child ID", unique(c(femaleChildrenData$id),selected=NULL, multiple = FALSE))
                                         
                                       ),
                                     mainPanel(plotlyOutput("plotFemaleHeight",height="600"))))
                 )
    )
                 
                 

