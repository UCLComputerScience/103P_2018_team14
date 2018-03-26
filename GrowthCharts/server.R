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

#LMS to measurement function
lmsFunctionToM <- function(l,m,s,z){
  m * (1 + l * s * z) ^ (1/l)
}

#LMS to Z function
lmsFunctionToZ <- function(l,m,s,data){
  (((data/m)^l)-1)/(l*s)
}

maleHeightCentileDF <- data.frame(Age=numeric(),Values=numeric(),Centile=character())
femaleHeightCentileDF <- data.frame(Age=numeric(),Values=numeric(),Centile=character())
maleWeightCentileDF <- data.frame(Age=numeric(),Values=numeric(),Centile=character())
femaleWeightCentileDF <- data.frame(Age=numeric(),Values=numeric(),Centile=character())


#Calculating centile values for each type of measurement graph
for(i in 1:9)
{
  maleHeightCentileDF<-rbind(maleHeightCentileDF,data.frame(Age=maleLMSData$Months,Values= lmsFunctionToM(maleLMSData$L.ht,maleLMSData$M.ht,maleLMSData$S.ht,zValues[i]),Centile=pValues[i]))
  femaleHeightCentileDF<-rbind(femaleHeightCentileDF,data.frame(Age=femaleLMSData$Months,Values= lmsFunctionToM(femaleLMSData$L.ht,femaleLMSData$M.ht,femaleLMSData$S.ht,zValues[i]),Centile=pValues[i]))
  maleWeightCentileDF<-rbind(maleWeightCentileDF,data.frame(Age=maleLMSData$Months,Values= lmsFunctionToM(maleLMSData$L.wt,maleLMSData$M.wt,maleLMSData$S.wt,zValues[i]),Centile=pValues[i]))
  femaleWeightCentileDF<-rbind(femaleWeightCentileDF,data.frame(Age=femaleLMSData$Months,Values= lmsFunctionToM(femaleLMSData$L.wt,femaleLMSData$M.wt,femaleLMSData$S.wt,zValues[i]),Centile=pValues[i]))
}

#Sorting each data frame according to the centile
maleWeightCentileDF$Centile <- factor(maleWeightCentileDF$Centile, levels = rev(levels(maleWeightCentileDF$Centile)))
femaleWeightCentileDF$Centile <- factor(femaleWeightCentileDF$Centile, levels = rev(levels(femaleWeightCentileDF$Centile)))
maleHeightCentileDF$Centile <- factor(maleHeightCentileDF$Centile, levels = rev(levels(maleHeightCentileDF$Centile)))
femaleHeightCentileDF$Centile <- factor(femaleHeightCentileDF$Centile, levels = rev(levels(femaleHeightCentileDF$Centile)))





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
} #Get appropriate L values from LMS table

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
} #Get appropriate M values from LMS table

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
} #Get appropriate S values from LMS table

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
  Centile <- vector()
  Values<- data.frame(Values=numeric())
  
  for (i in 1:nrow(childDF))
  {
    
    L<-getL(type,gender,childDF[i,1])
    M<-getM(type,gender,childDF[i,1])
    S<-getS(type,gender,childDF[i,1])
    
    data<-childDF[i,2]
    value<-lmsFunctionToZ(L,M,S,data)
    Values<-rbind(Values,value)
    
    
  }
  
  for (i in 1:nrow(Values))
  {
    Centile <- c(Centile,z2cent(Values[i,1]))

  }
  Centile <- as.data.frame(Centile)
  childDF<- cbind(childDF, data.frame(Values=Values,Centile=Centile))
  names(childDF)[names(childDF)=="Months"] <- "Age" #Renaming columns
  DF$Centile <- factor(DF$Centile, levels = rev(levels(DF$Centile)))
  plot<-ggplot(DF,aes(x=Age,y=Values,label=Centile)) +geom_smooth(aes(colour=Centile),linetype='dotdash',se=FALSE)
  plot <- plot + ggtitle(gender) + labs(x="Age (Months)",y="Z-Score")+ scale_x_continuous(breaks=seq(0,60,5), limits = c(0,60))
  plot<- plot +geom_point(data=childDF, show.legend = TRUE) + geom_line(data=childDF)
  plot <- ggplotly(plot)
  

}

plotGraph <- function(type, gender, childID){
  
  if(type=="wt")
  {
    if (gender=="Boys")
    {
      
      childDF <- data.frame(Age=maleChildrenData$Months[maleChildrenData$id==childID], Values=maleChildrenData$weight[maleChildrenData$id==childID])
      plot<-ggplot(maleWeightCentileDF,aes(x=Age,y=Values, label = Centile))+geom_path(aes(colour=Centile),linetype='dotdash')
      plot<- plot+labs(x="Age (Months)",y="Weight (kg)") + scale_x_continuous(breaks=seq(0,60,5), limits = c(0,60))+scale_y_continuous(breaks = seq(0,30,1), limits = c(0,max(maleWeightCentileDF$Values)))
    }
    else
    {
      childDF <- data.frame(Age=femaleChildrenData$Months[femaleChildrenData$id==childID], Values=femaleChildrenData$weight[femaleChildrenData$id==childID])
      plot<-ggplot(femaleWeightCentileDF,aes(x=Age,y=Values, label = Centile))+geom_path(aes(colour=Centile),linetype='dotdash')
      plot<- plot+labs(x="Age (Months)",y="Weight (kg)") + scale_x_continuous(breaks=seq(0,60,5), limits = c(0,60))+scale_y_continuous(breaks = seq(0,30,1), limits = c(0,max(femaleWeightCentileDF$Values)))
      
    }
    
  }
  else
  {
    if (gender=="Boys")
    {
      childDF <- data.frame(Age=maleChildrenData$Months[maleChildrenData$id==childID], Values=maleChildrenData$height[maleChildrenData$id==childID])
      plot<-ggplot(maleHeightCentileDF,aes(x=Age,y=Values, label = Centile))+geom_path(aes(colour=Centile),linetype='dotdash')
      plot<- plot+labs(x="Age (Months)",y="Height (cm)")+ scale_x_continuous(breaks=seq(0,60,5))+scale_y_continuous(breaks=seq(40,130,by=5), limits=c(40,max(maleHeightCentileDF$Values)))
    }
    else
    {
      childDF <- data.frame(Age=femaleChildrenData$Months[femaleChildrenData$id==childID], Values=femaleChildrenData$height[femaleChildrenData$id==childID])
      plot<-ggplot(femaleHeightCentileDF,aes(x=Age,y=Values, label = Centile))+geom_path(aes(colour=Centile),linetype='dotdash')
      plot<- plot+labs(x="Age (Months)",y="Height (cm)")+ scale_x_continuous(breaks=seq(0,60,5))+scale_y_continuous(breaks=seq(40,130,by=5), limits=c(40,max(femaleHeightCentileDF$Values)))
      
    }
    
  }
  
  Centile <- vector()
  zValues<- data.frame(Values=numeric())
  
  for (i in 1:nrow(childDF))
  {
    
    L<-getL(type,gender,childDF[i,1])
    M<-getM(type,gender,childDF[i,1])
    S<-getS(type,gender,childDF[i,1])
    
    data<-childDF[i,2]
    value<-lmsFunctionToZ(L,M,S,data)
    zValues<-rbind(zValues,value)
    
    
  }
  
  for (i in 1:nrow(zValues))
  {
    Centile <- c(Centile,z2cent(zValues[i,1]))
    
  }
  
  Centile <- as.data.frame(Centile)
  childDF <- cbind(childDF,data.frame(Centile=Centile))
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