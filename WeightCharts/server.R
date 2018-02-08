library(plotly)
library(shiny)

maleData <- read.csv("WTAGEM.csv",header = TRUE)
femaleData <- read.csv("WTAGEF.csv",header = TRUE)

zValues <- c( -1.881, -1.645, -1.282, -0.674, 0, 0.674, 1.036, 1.282, 1.645, 1.881 )
pValues <- c("P3","P5","P10","P25","P50","P75","P85","P90","P95","P97")

lmsFunction <- function(l,m,s,z){
  m*((1+l*s*z)^(1/l))
}

server<- function(input, output) {
  output$plotMale <- renderPlotly({
    Agemos <- maleData$ï..Agemos 
    L<-maleData$L
    M<-maleData$M
    S<-maleData$S
    maleDF<-data.frame(Age=numeric(),Weight=numeric(),Centile=integer())
    
    for(i in 1:10)
    {
      weight<-lmsFunction(L,M,S,zValues[i])
      maleDF<-rbind(maleDF,data.frame(Age=Agemos,Weight=weight,Centile=pValues[i]))
    }
    
    maleDF$Centile <- factor(maleDF$Centile, levels = rev(levels(maleDF$Centile)))
    malePlot<-ggplot(maleDF,aes(x=Age,y=Weight))+geom_smooth(aes(colour=Centile),linetype='dotdash',se=FALSE)
    
    
    malePlot<- malePlot+labs(x="Age(Months)",y="Weight(kg)")+ scale_x_continuous(breaks=seq(0,36,2))+scale_y_continuous(breaks = seq(0,20,1))
    malePlot <- ggplotly(malePlot)
  })
  
  output$plotFemale <- renderPlotly({
    Agemos <- femaleData$ï..Agemos 
    L<-femaleData$L
    M<-femaleData$M
    S<-femaleData$S
    
    femaleDF<-data.frame(Age=numeric(),Weight=numeric(),Centile=character())
    
    for(i in 1:10)
    {
      weight<-lmsFunction(L,M,S,zValues[i])
      femaleDF<-rbind(femaleDF,data.frame(Age=Agemos,Weight=weight,Centile=pValues[i]))
    }
    
    femaleDF$Centile=factor(femaleDF$Centile, levels = rev(levels(femaleDF$Centile)))
    femalePlot<-ggplot(femaleDF,aes(x=Age,y=Weight))+geom_smooth(aes(colour=Centile),linetype='dotdash',se=FALSE)
    
    femalePlot<- femalePlot+labs(x="Age(Months)",y="Weight(kg)")+ scale_x_continuous(breaks=seq(0,36,2))+scale_y_continuous(breaks = seq(0,20,1))
    femalePlot <- ggplotly(femalePlot)
    
  })
  
  
  
}