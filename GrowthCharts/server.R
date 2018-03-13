library(plotly)
library(shiny)
library(BH)

#Reading in the data files for creating the graphs
LMSData <- read.csv("LMSData.csv",header = TRUE)
maleLMSData <- subset(LMSData,sex==1)
femaleLMSData <- subset(LMSData,sex==2)
childrenData <- read.csv("ChildrenData.csv",header = TRUE)
childrenData <- subset(childrenData,Months<=60)
maleChildrenData <- subset(childrenData,sex==1)
femaleChildrenData <- subset(childrenData,sex==2)

#Z score values with corresponding centile lines
zValues <- c(-2.652, -2.054, -1.341, -0.674, 0, 0.674, 1.341, 2.054, 2.652)
pValues <- c("0.4th", "2nd", "9th", "25th", "50th", "75th", "91st", "98th", "99.6th")

#Converts any z score value to a centile
z2cent <- function(z) {
  np <- abs(z) > qnorm(0.99)
  ct <- round(pnorm(z) * 100, np)
  mod10 <- ifelse(np, 0, floor(ct %% 10))
  th <- ifelse(mod10 == 0 | mod10 > 4 | (ct > 10 & ct < 14), 4, mod10)
  th <- paste0(ct, c('st', 'nd', 'rd', 'th')[th])
  th[th == '0th'] <- paste0('SDS', round(z[th == '0th'], 1))
  th[th == '100th'] <- paste('SDS', round(z[th == '100th'], 1), sep='+')
  th
}

#LMS to measurement function
lmsFunctionToM <- function(l,m,s,z){
  m*((1+l*s*z)^(1/l))
}

#LMS to Z function
lmsFunctionToZ <- function(l,m,s,data){
  (((data/m)^l)-1)/(l*s)
}
getL<-function(type,gender,age){
  if (gender=="Boys")
  {
    if (type=="wt")
    {
      return(maleLMSData$L.wt[maleLMSData$Months==age])
    }
    else
    {
      return(maleLMSData$L.ht[maleLMSData$Months==age])
    }
  }
  else
  {
    if (type=="wt")
    {
      return(femaleLMSData$L.wt[femaleLMSData$Months==age])
    }
    else
    {
      return(femaleLMSData$L.ht[femaleLMSData$Months==age])
    }
    
  }
}

getM<-function(type,gender,age){
  if (gender=="Boys")
  {
    if (type=="wt")
    {
      return(maleLMSData$M.wt[maleLMSData$Months==age])
    }
    else
    {
      return(maleLMSData$M.ht[maleLMSData$Months==age])
    }
  }
  else
  {
    if (type=="wt")
    {
      return(femaleLMSData$M.wt[femaleLMSData$Months==age])
    }
    else
    {
      return(femaleLMSData$M.ht[femaleLMSData$Months==age])
    }
    
  }
}

getS<-function(type,gender,age){
  if (gender=="Boys")
  {
    if (type=="wt")
    {
      return(maleLMSData$S.wt[maleLMSData$Months==age])
    }
    else
    {
      return(maleLMSData$S.ht[maleLMSData$Months==age])
    }
  }
  else
  {
    if (type=="wt")
    {
      return(femaleLMSData$S.wt[femaleLMSData$Months==age])
    }
    else
    {
      return(femaleLMSData$S.ht[femaleLMSData$Months==age])
    }
    
  }
}

plotZ <- function(type,gender, childID) {
  Agemos <- maleLMSData$Months
  DF<-data.frame(Age=numeric(),Values=numeric(),Centile=character())
  for (i in 1:9)
  {
    DF<-rbind(DF,data.frame(Age=Agemos,Values=zValues[i],Centile=pValues[i]))
  }
  if (type=="wt")
  {
    if(gender=="Boys")
    {
      childDF <- subset(maleChildrenData,id==childID & weight>0, select = c(Months,weight))
    }
    else
    {
      childDF <- subset(femaleChildrenData,id==childID & weight >0, select = c(Months,weight))
    }
  }
  else
  {
    if(gender=="Boys")
    {
      childDF <- subset(maleChildrenData,id==childID & height>0, select = c(Months,height))
    }
    else
    {
      childDF <- subset(femaleChildrenData,id==childID & height >0, select = c(Months,height))
    }
  }
  Centile <- data.frame(Centile = character())
  
  Values<- data.frame(Values=numeric())

  for (i in 1:nrow(childDF))
  {

    L<-getL(type,gender,childDF[i,1])
    M<-getM(type,gender,childDF[i,1])
    S<-getS(type,gender,childDF[i,1])

    data<-childDF[i,2]
    value<-lmsFunctionToZ(L,M,S,data)
    Values<-rbind(Values,value)
    Centile <- rbind(Centile, z2cent(value))


  }
  childDF <-cbind(childDF,Values,Centile)
  names(childDF)[names(childDF)=="Months"] <- "Age" #Renaming columns
  names(childDF)[3] <- "Values" #Renaming columns
  
  childDF$my_text=paste("Centile: " ,Centile, sep="")
  DF$my_text=paste("Centile: " ,Centile, sep="")
  DF$Centile <- factor(DF$Centile, levels = rev(levels(DF$Centile)))
  plot<-ggplot(DF,aes(x=Age,y=Values)) +geom_smooth(aes(colour=Centile),linetype='dotdash',se=FALSE)
  plot <- plot + ggtitle(gender) + labs(x="Age (Months)",y="Z-Score")+ scale_x_continuous(breaks=seq(0,60,5), limits = c(0,60))
  plot<- plot +geom_point(data=childDF, show.legend = TRUE) + geom_line(data=childDF)
  plot <- ggplotly(plot)
  

}

plotGraph <- function(type, gender, childID){
  if (type=="wt")
  {
    if(gender=="Boys")
    {
      Agemos <- maleLMSData$Months 
      L<-maleLMSData$L.wt
      M<-maleLMSData$M.wt
      S<-maleLMSData$S.wt
      DF<-data.frame(Age=numeric(),Values=numeric(),Centile=integer())
      childDF <- data.frame(Age=maleChildrenData$Months[maleChildrenData$id==childID], Values=maleChildrenData$weight[maleChildrenData$id==childID])
    }
    else
    {
      Agemos <- femaleLMSData$Months 
      L<-femaleLMSData$L.wt
      M<-femaleLMSData$M.wt
      S<-femaleLMSData$S.wt
      DF<-data.frame(Age=numeric(),Values=numeric(),Centile=character())
      childDF <- data.frame(Age=femaleChildrenData$Months[femaleChildrenData$id==childID], Values=femaleChildrenData$weight[femaleChildrenData$id==childID])
      
    }
  }
  else
  {
    if(gender=="Boys")
    {
      Agemos <- maleLMSData$Months 
      L<-maleLMSData$L.ht
      M<-maleLMSData$M.ht
      S<-maleLMSData$S.ht
      DF<-data.frame(Age=numeric(),Values=numeric(),Centile=integer())
      childDF <- data.frame(Age=maleChildrenData$Months[maleChildrenData$id==childID], Values=maleChildrenData$height[maleChildrenData$id==childID])
    }
    else
    {
      Agemos <- femaleLMSData$Months 
      L<-femaleLMSData$L.ht
      M<-femaleLMSData$M.ht
      S<-femaleLMSData$S.ht
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
  
  plot <- plot + geom_line(data=childDF, show.legend = TRUE) +geom_point(data=childDF, show.legend = TRUE) + ggtitle(gender) 
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
      plotZ("wt","Girls",input$girlsWT)
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
      plotZ("ht","Girls",input$girlsHT)
    }
  })
  
  
  
  
  
  
}