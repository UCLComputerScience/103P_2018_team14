# Here i'm importing the libraries i'm gonna use in my code 
library(shiny) 
library(plotly)

# here i assign the tables for male and female heights along with their respective lms values to apply named variables
LMSData <- read.csv("LMSData.csv",header = TRUE)
maleData <- subset(LMSData,sex==1)
femaleData <- subset(LMSData,sex==2)


# these next two variables I declare are the z values corresponding to certain percentiles
zVals <- c(-2.652, -2.054, -1.341, -0.674, 0, 0.674, 1.341, 2.054, 2.652)
pVals <- c("0.4th", "2nd", "9th", "25th", "50th", "75th", "91st", "98th", "99.6th")

# this is a fucntion that calculates the measurement from the l, m, s and z values
calcMeasurement <- function(l,m,s,z){
  m*(1 + l*s*z)**(1/l)
}

server<- function(input, output) {
  output$plotMale <- renderPlotly({
    Agemos <- maleData$Months
    L<-maleData$L.ht
    M<-maleData$M.ht
    S<-maleData$S.ht
    maleDF<-data.frame(Age=numeric(),Height=numeric(),Centile=integer())
    
    for(i in 1:9)
    {
      height<-calcMeasurement(L,M,S,zVals[i])
      maleDF<-rbind(maleDF,data.frame(Age=Agemos,Height=height,Centile=pVals[i]))
    }
    
    maleDF$Centile <- factor(maleDF$Centile, levels=rev(levels(maleDF$Centile)))
    malePlot<-ggplot(maleDF,aes(x=Age,y=Height))+geom_smooth(aes(colour=Centile),linetype='dotdash',se=FALSE)
    
    
    malePlot<- malePlot+labs(x="Age (Months)",y="Height (cm)")+ scale_x_continuous(breaks=seq(0,60,2))+scale_y_continuous(breaks=seq(40,130,by=5), limits=c(40,max(maleDF$Height)))
    malePlot <- ggplotly(malePlot)
  })
  
  output$plotFemale <- renderPlotly({
    Agemos <- femaleData$Months 
    L<-femaleData$L.ht
    M<-femaleData$M.ht
    S<-femaleData$S.ht
    
    femaleDF<-data.frame(Age=numeric(),Height=numeric(),Centile=character())
    
    for(i in 1:9)
    {
      height<-calcMeasurement(L,M,S,zVals[i])
      femaleDF<-rbind(femaleDF,data.frame(Age=Agemos,Height=height,Centile=pVals[i]))
    }
    
    femaleDF$Centile=factor(femaleDF$Centile, levels = rev(levels(femaleDF$Centile)))
    femalePlot<-ggplot(femaleDF,aes(x=Age,y=Height))+geom_smooth(aes(colour=Centile),linetype='dotdash',se=FALSE)
    
    femalePlot<- femalePlot+labs(x="Age (Months)",y="Height (cm)")+ scale_x_continuous(breaks=seq(0,60,2))+scale_y_continuous(breaks=seq(40,130,by=5), limits=c(40,max(femaleDF$Height))) 
    femalePlot <- ggplotly(femalePlot)
    
  })
  
  
  
}

