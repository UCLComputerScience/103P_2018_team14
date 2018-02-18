library(plotly)
library(shiny)

#Initialising data frames for creating the graphs
LMSData <- read.csv("LMSData.csv",header = TRUE)
maleData <- subset(LMSData,sex==1)
femaleData <- subset(LMSData,sex==2)

#Z score values with corresponding centile lines
zValues <- c(-2.652, -2.054, -1.341, -0.674, 0, 0.674, 1.341, 2.054, 2.652)
pValues <- c("0.4th", "2nd", "9th", "25th", "50th", "75th", "91st", "98th", "99.6th")

#LMS to measurement function
lmsFunction <- function(l,m,s,z){
  m*((1+l*s*z)^(1/l))
}

server<- function(input, output) {
  output$plotMaleWeight <- renderPlotly({
    Agemos <- maleData$Months 
    L<-maleData$L.wt
    M<-maleData$M.wt
    S<-maleData$S.wt
    maleDF<-data.frame(Age=numeric(),Weight=numeric(),Centile=integer())
    
    for(i in 1:9)
    {
      weight<-lmsFunction(L,M,S,zValues[i])
      maleDF<-rbind(maleDF,data.frame(Age=Agemos,Weight=weight,Centile=pValues[i])) #binding the measurements to data frame
    }
    
    maleDF$Centile <- factor(maleDF$Centile, levels = rev(levels(maleDF$Centile)))
    malePlot<-ggplot(maleDF,aes(x=Age,y=Weight))+geom_smooth(aes(colour=Centile),linetype='dotdash',se=FALSE)
    
    
    malePlot<- malePlot+labs(x="Age (Months)",y="Weight (kg)")+ scale_x_continuous(breaks=seq(0,60,2), limits = c(0,60))+scale_y_continuous(breaks = seq(0,30,1), limits = c(0,max(maleDF$Weight)))
    malePlot <- malePlot + ggtitle("Boys")
    malePlot <- ggplotly(malePlot)
  })
  
  output$plotFemaleWeight <- renderPlotly({
    Agemos <- femaleData$Months 
    L<-femaleData$L.wt
    M<-femaleData$M.wt
    S<-femaleData$S.wt
    
    femaleDF<-data.frame(Age=numeric(),Weight=numeric(),Centile=character())
    
    for(i in 1:9)
    {
      weight<-lmsFunction(L,M,S,zValues[i])
      femaleDF<-rbind(femaleDF,data.frame(Age=Agemos,Weight=weight,Centile=pValues[i]))  #binding the measurements to data frame
    }
    
    femaleDF$Centile=factor(femaleDF$Centile, levels = rev(levels(femaleDF$Centile)))
    femalePlot<-ggplot(femaleDF,aes(x=Age,y=Weight))+geom_smooth(aes(colour=Centile),linetype='dotdash',se=FALSE)
    
    femalePlot<- femalePlot+labs(x="Age (Months)",y="Weight (kg)")+ scale_x_continuous(breaks=seq(0,60,2))+scale_y_continuous(breaks = seq(0,30,1), limits = c(0,max(femaleDF$Weight)))
    femalePlot <- femalePlot + ggtitle("Girls")
    femalePlot <- ggplotly(femalePlot)
    
  })
  
  output$plotMaleHeight <- renderPlotly({
    Agemos <- maleData$Months
    L<-maleData$L.ht
    M<-maleData$M.ht
    S<-maleData$S.ht
    maleDF<-data.frame(Age=numeric(),Height=numeric(),Centile=integer())
    
    for(i in 1:9)
    {
      height<-lmsFunction(L,M,S,zValues[i])
      maleDF<-rbind(maleDF,data.frame(Age=Agemos,Height=height,Centile=pValues[i])) #binding the measurements to data frame
    }
    
    maleDF$Centile <- factor(maleDF$Centile, levels=rev(levels(maleDF$Centile)))
    malePlot<-ggplot(maleDF,aes(x=Age,y=Height))+geom_smooth(aes(colour=Centile),linetype='dotdash',se=FALSE)
    
    
    malePlot<- malePlot+labs(x="Age (Months)",y="Height (cm)")+ scale_x_continuous(breaks=seq(0,60,2))+scale_y_continuous(breaks=seq(40,130,by=5), limits=c(40,max(maleDF$Height)))
    malePlot <- malePlot + ggtitle("Boys")
    malePlot <- ggplotly(malePlot)
  })
  
  output$plotFemaleHeight <- renderPlotly({
    Agemos <- femaleData$Months 
    L<-femaleData$L.ht
    M<-femaleData$M.ht
    S<-femaleData$S.ht
    
    femaleDF<-data.frame(Age=numeric(),Height=numeric(),Centile=character())
    
    for(i in 1:9)
    {
      height<-lmsFunction(L,M,S,zValues[i])
      femaleDF<-rbind(femaleDF,data.frame(Age=Agemos,Height=height,Centile=pValues[i])) #binding the measurements to data frame
    }
    
    femaleDF$Centile=factor(femaleDF$Centile, levels = rev(levels(femaleDF$Centile)))
    femalePlot<-ggplot(femaleDF,aes(x=Age,y=Height))+geom_smooth(aes(colour=Centile),linetype='dotdash',se=FALSE)
    
    femalePlot<- femalePlot+labs(x="Age (Months)",y="Height (cm)")+ scale_x_continuous(breaks=seq(0,60,2))+scale_y_continuous(breaks=seq(40,130,by=5), limits=c(40,max(femaleDF$Height))) 
    femalePlot <- femalePlot + ggtitle("Girls")
    femalePlot <- ggplotly(femalePlot)
    
  })
  
  
  
  
  
  
}