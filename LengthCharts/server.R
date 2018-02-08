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
    maleDF<-data.frame(Age=numeric(),Height=numeric(),Centile=integer())
    
    for(i in 1:10)
    {
      height<-calcMeasurement(L,M,S,zVals[i])
      maleDF<-rbind(maleDF,data.frame(Age=Agemos,Height=height,Centile=pVals[i]))
    }
    
    maleDF$Centile <- factor(maleDF$Centile, levels=rev(levels(maleDF$Centile)))
    malePlot<-ggplot(maleDF,aes(x=Age,y=Height))+geom_smooth(aes(colour=Centile),linetype='dotdash',se=FALSE)
    
    
    malePlot<- malePlot+labs(x="Age(Months)",y="Height (cm)")+ scale_x_continuous(breaks=seq(0,36,2))+scale_y_continuous(breaks=seq(40,110,by=5), limits=c(40,110))
    malePlot <- ggplotly(malePlot)
  })
  
  output$plotFemale <- renderPlotly({
    Agemos <- femaleData$ï..Agemos 
    L<-femaleData$L
    M<-femaleData$M
    S<-femaleData$S
    
    femaleDF<-data.frame(Age=numeric(),Height=numeric(),Centile=character())
    
    for(i in 1:10)
    {
      height<-calcMeasurement(L,M,S,zVals[i])
      femaleDF<-rbind(femaleDF,data.frame(Age=Agemos,Height=height,Centile=pVals[i]))
    }
    
    femaleDF$Centile=factor(femaleDF$Centile, levels = rev(levels(femaleDF$Centile)))
    femalePlot<-ggplot(femaleDF,aes(x=Age,y=Height))+geom_smooth(aes(colour=Centile),linetype='dotdash',se=FALSE)
    
    femalePlot<- femalePlot+labs(x="Age(Months)",y="Height(cm)")+ scale_x_continuous(breaks=seq(0,36,2))+scale_y_continuous(breaks=seq(40,110,by=5), limits=c(40,110)) 
    femalePlot <- ggplotly(femalePlot)
    
  })
  
  
  
}

