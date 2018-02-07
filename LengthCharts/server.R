# Here i'm importing the libraries i'm gonna use in my code 
library(shiny) 
library(plotly)

# here i assign the tables for male and female heights along with their respective lms values to aptly named variables
maleData <- read.csv("maleLenTable.csv", header = T)
femaleData <- read.csv("femaleLenTable.csv", header = T)

# these next two variables I declare are the z values corresponding to certain percentiles
zVals <- c(-1.881, -1.645, -1.282, -0.674, 0, 0.674, 1.036, 1.282, 1.645, 1.881)
pVals <- c("P3","P5","P10","P25","P50","P75","P85","P90","P95","P97")

# this is a fucntion that calculates the measurement from the l, m, s and z values
calcMeasurement <- function(l,m,s,z){
  m*(1 + l*s*z)**(1/l)
}

server<- function(input, output) {
  output$plotMale <- renderPlotly({
    Agemos <- maleData$ï..Agemos 
    L<-maleData$L
    M<-maleData$M
    S<-maleData$S
    maleDF<-data.frame(Age=numeric(),Height=numeric(),Percentile=integer())
    
    for(i in 1:10)
    {
      height<-calcMeasurement(L,M,S,zVals[i])
      maleDF<-rbind(maleDF,data.frame(Age=Agemos,Height=height,Percentile=pVals[i]))
    }
    
    maleDF$Percentile <- factor(maleDF$Percentile)
    malePlot<-ggplot(maleDF,aes(x=Age,y=Height))+geom_smooth(aes(colour=Percentile),linetype='dotdash',se=FALSE)
    
    
    malePlot<- malePlot+labs(x="Age(Months)",y="Height (cm)")+ scale_x_continuous(breaks=seq(0,36,2))+scale_y_continuous(breaks = seq(0,20,0.5))
    malePlot <- ggplotly(malePlot)
  })
  
  output$plotFemale <- renderPlotly({
    Agemos <- femaleData$ï..Agemos 
    L<-femaleData$L
    M<-femaleData$M
    S<-femaleData$S
    
    femaleDF<-data.frame(Age=numeric(),Height=numeric(),Percentile=character())
    
    for(i in 1:10)
    {
      height<-calcMeasurement(L,M,S,zVals[i])
      femaleDF<-rbind(femaleDF,data.frame(Age=Agemos,Height=height,Percentile=pVals[i]))
    }
    
    femaleDF$Percentile=factor(femaleDF$Percentile)
    femalePlot<-ggplot(femaleDF,aes(x=Age,y=Height))+geom_smooth(aes(colour=Percentile),linetype='dotdash',se=FALSE)
    
    femalePlot<- femalePlot+labs(x="Age(Months)",y="Height(cm)")+ scale_x_continuous(breaks=seq(0,36,2))+scale_y_continuous(breaks = seq(0,20,0.5))
    femalePlot <- ggplotly(femalePlot)
    
  })
  
  
  
}

