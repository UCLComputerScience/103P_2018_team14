library(plotly)
library(shiny)

#Reading in the data files for creating the graphs
LMSData <- read.csv("LMSData.csv",header = TRUE)
maleData <- subset(LMSData,sex==1)
femaleData <- subset(LMSData,sex==2)
childrenData <- read.csv("ChildrenData.csv",header = TRUE)
childrenData <- subset(childrenData,Months<=60)
maleChildrenData <- subset(childrenData,sex==1)
femaleChildrenData <- subset(childrenData,sex==2)

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
getL<-function(type,gender,age){
  if (gender=="Boys")
  {
    if (type=="wt")
    {
      return(maleData$L.wt[maleData$Months==age])
    }
    else
    {
      return(maleData$L.ht[maleData$Months==age])
    }
  }
  else
  {
    if (type=="wt")
    {
      return(femaleData$L.wt[femaleData$Months==age])
    }
    else
    {
      return(femaleData$L.ht[femaleData$Months==age])
    }
    
  }
}

getM<-function(type,gender,age){
  if (gender=="Boys")
  {
    if (type=="wt")
    {
      return(maleData$M.wt[maleData$Months==age])
    }
    else
    {
      return(maleData$M.ht[maleData$Months==age])
    }
  }
  else
  {
    if (type=="wt")
    {
      return(femaleData$M.wt[femaleData$Months==age])
    }
    else
    {
      return(femaleData$M.ht[femaleData$Months==age])
    }
    
  }
}

getS<-function(type,gender,age){
  if (gender=="Boys")
  {
    if (type=="wt")
    {
      return(maleData$S.wt[maleData$Months==age])
    }
    else
    {
      return(maleData$S.ht[maleData$Months==age])
    }
  }
  else
  {
    if (type=="wt")
    {
      return(femaleData$S.wt[femaleData$Months==age])
    }
    else
    {
      return(femaleData$S.ht[femaleData$Months==age])
    }
    
  }
}

plotZ <- function(type,gender, childID) {
  Agemos <- maleData$Months
  DF<-data.frame(Age=numeric(),Values=numeric(),Centile=character())
  for (i in 1:9)
  {
    DF<-rbind(DF,data.frame(Age=Agemos,Values=zValues[i],Centile=pValues[i]))
  }
  # if (type=="wt")
  # {
  #   if(gender=="Boys")
  #   {
  #     childDF <- subset(maleChildrenData,id==childID)
  #   }
  #   else
  #   {
  #     childDF <- data.frame(Age=femaleChildrenData$Months[femaleChildrenData$id==childID], Data=femaleChildrenData$weight[femaleChildrenData$id==childID],Values=numeric())
  # 
  #   }
  # }
  # else
  # {
  #   if(gender=="Boys")
  #   {
  #     childDF <- data.frame(Age=maleChildrenData$Months[maleChildrenData$id==childID], Data=maleChildrenData$height[maleChildrenData$id==childID],Values=numeric())
  #   }
  #   else
  #   {
  #     childDF <- data.frame(Age=femaleChildrenData$Months[femaleChildrenData$id==childID], Data=femaleChildrenData$height[femaleChildrenData$id==childID],Values=numeric())
  #   }
  # }
  # 
  # for (i in 1:nrow(childDF))
  # {
  #   row<- childDF[i,]
  #   L<-getL(type,gender,row$Months)
  #   M<-getM(type,gender,row$Months)
  #   S<-getS(type,gender,row$Months)
  # 
  #   data<-row$Data
  #   value<-lmsFunctionToZ(L,M,S,data)
  #   values<-union(values,c(value))
  #   
  # 
  # }
  # childDF <-cbind(childDF,data.frame(Values=values))
  
  DF$Centile <- factor(DF$Centile, levels = rev(levels(DF$Centile)))
  plot<-ggplot(DF,aes(x=Age,y=Values))+geom_smooth(aes(colour=Centile),linetype='dotdash',se=FALSE)
  plot <- plot + ggtitle(gender) + labs(x="Age (Months)",y="Z-Score")+ scale_x_continuous(breaks=seq(0,60,5), limits = c(0,60))
  #plot<- plot + +geom_point(data=childDF,aes(x=Age, y=Values), show.legend = TRUE)
  plot <- ggplotly(plot)
  

}

plotGraph <- function(type, gender, childID){
  if (type=="wt")
  {
    if(gender=="Boys")
    {
      Agemos <- maleData$Months 
      L<-maleData$L.wt
      M<-maleData$M.wt
      S<-maleData$S.wt
      DF<-data.frame(Age=numeric(),Values=numeric(),Centile=integer())
      childDF <- data.frame(Age=maleChildrenData$Months[maleChildrenData$id==childID], Values=maleChildrenData$weight[maleChildrenData$id==childID])
    }
    else
    {
      Agemos <- femaleData$Months 
      L<-femaleData$L.wt
      M<-femaleData$M.wt
      S<-femaleData$S.wt
      DF<-data.frame(Age=numeric(),Values=numeric(),Centile=character())
      childDF <- data.frame(Age=femaleChildrenData$Months[femaleChildrenData$id==childID], Values=femaleChildrenData$weight[femaleChildrenData$id==childID])
      
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
      childDF <- data.frame(Age=maleChildrenData$Months[maleChildrenData$id==childID], Values=maleChildrenData$height[maleChildrenData$id==childID])
    }
    else
    {
      Agemos <- femaleData$Months 
      L<-femaleData$L.ht
      M<-femaleData$M.ht
      S<-femaleData$S.ht
      DF<-data.frame(Age=numeric(),Values=numeric(),Centile=character())
      childDF <- data.frame(Age=femaleChildrenData$Months[femaleChildrenData$id==childID], Values=femaleChildrenData$height[femaleChildrenData$id==childID])
      
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
    plot<- plot+labs(x="Age (Months)",y="Height (cm)")+ scale_x_continuous(breaks=seq(0,60,5))+scale_y_continuous(breaks=seq(40,130,by=5), limits=c(40,max(DF$Values)))
  }
  
  plot <- plot +geom_line(data=childDF, show.legend = TRUE) +geom_point(data=childDF) + ggtitle(gender)
  plot <- ggplotly(plot)
  
  
}
server<- function(input, output) {
  output$plotMaleWeight <- renderPlotly({
    if (input$plotType1 == 1)
    {
      plotGraph("wt","Boys",input$boysWT)
    }
    else
    {
      plotZ("wt","Boys",input$boysWT)
    }
    
  })
  
  output$plotFemaleWeight <- renderPlotly({
    if (input$plotType2 == 1)
    {
      plotGraph("wt","Girls",input$girlsWT)
    }
    else
    {
      plotZ("wt","Girls",input$boysWT)
    }
  })
  
  output$plotMaleHeight <- renderPlotly({
    if (input$plotType3 == 1)
    {
      plotGraph("ht","Boys",input$boysHT)
    }
    else
    {
      plotZ("ht","Boys",input$boysHT)
    }
  })
  
  output$plotFemaleHeight <- renderPlotly({
    if (input$plotType4 == 1)
    {
      plotGraph("ht","Girls",input$girlsHT)
    }
    else
    {
      plotZ("ht","Girls",input$boysHT)
    }
  })
  
  
  
  
  
  
}