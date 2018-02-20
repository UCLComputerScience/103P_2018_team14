library(plotly)
library(shiny)

#Reading in the data files for creating the graphs
LMSData <- read.csv("LMSData.csv",header = TRUE)
maleData <- subset(LMSData,sex==1)
femaleData <- subset(LMSData,sex==2)
childrenData <- read.csv("ChildrenData.csv",header = TRUE)

#Z score values with corresponding centile lines
zValues <- c(-2.652, -2.054, -1.341, -0.674, 0, 0.674, 1.341, 2.054, 2.652)
pValues <- c("0.4th", "2nd", "9th", "25th", "50th", "75th", "91st", "98th", "99.6th")

#LMS to measurement function
lmsFunctionToM <- function(l,m,s,z){
  m*((1+l*s*z)^(1/l))
}

#LMS to Z function
lmsFunctionToZ <- function(l,m,s,data){
  (((data/m)*l)-1)/(l*s)
}

plotZ <- function(x, type) {
  switch(type,
         A = hist(x),
         B = barplot(x),
         C = pie(x))
}

plotGraph <- function(type, gender){
  if (type=="wt")
  {
    if(gender=="Boys")
    {
      Agemos <- maleData$Months 
      L<-maleData$L.wt
      M<-maleData$M.wt
      S<-maleData$S.wt
      DF<-data.frame(Age=numeric(),Values=numeric(),Centile=integer())
    }
    else
    {
      Agemos <- femaleData$Months 
      L<-femaleData$L.wt
      M<-femaleData$M.wt
      S<-femaleData$S.wt
      DF<-data.frame(Age=numeric(),Values=numeric(),Centile=character())
      
    }
  }
  else
  {
    if(gender=="Boys")
    {
      Agemos <- maleData$Months 
      L<-maleData$L.ht
      M<-maleData$M.ht
      S<-maleData$S.ht
      DF<-data.frame(Age=numeric(),Values=numeric(),Centile=integer())
    }
    else
    {
      Agemos <- femaleData$Months 
      L<-femaleData$L.ht
      M<-femaleData$M.ht
      S<-femaleData$S.ht
      DF<-data.frame(Age=numeric(),Values=numeric(),Centile=character())
      
    }
  }
  for(i in 1:9)
  {
    values<-lmsFunctionToM(L,M,S,zValues[i])
    DF<-rbind(DF,data.frame(Age=Agemos,Values=values,Centile=pValues[i])) #binding the measurements to data frame
  }
  
  DF$Centile <- factor(DF$Centile, levels = rev(levels(DF$Centile)))
  plot<-ggplot(DF,aes(x=Age,y=Values))+geom_smooth(aes(colour=Centile),linetype='dotdash',se=FALSE)
  
  if(type=="wt")
  {
    plot<- plot+labs(x="Age (Months)",y="Weight (kg)") + scale_x_continuous(breaks=seq(0,60,5), limits = c(0,60))+scale_y_continuous(breaks = seq(0,30,1), limits = c(0,max(DF$Values)))
  }
  else
  {
    plot<- plot+labs(x="Age (Months)",y="Height (cm)")+ scale_x_continuous(breaks=seq(0,60,2))+scale_y_continuous(breaks=seq(40,130,by=5), limits=c(40,max(DF$Values)))
  }
  
  plot <- plot + ggtitle(gender)
  plot <- ggplotly(plot)
  
  
}
server<- function(input, output) {
  output$plotMaleWeight <- renderPlotly({
    plotGraph("wt","Boys")
  })
  
  output$plotFemaleWeight <- renderPlotly({
    plotGraph("wt","Girls")
  })
  
  output$plotMaleHeight <- renderPlotly({
    plotGraph("ht","Boys")
  })
  
  output$plotFemaleHeight <- renderPlotly({
    plotGraph("ht","Girls")
  })
  
  
  
  
  
  
}