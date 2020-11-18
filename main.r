#--------------------------------------
#Author: Lei Chen
#Date: November 17,2020
#Comp321 final
#import package:dplyr, readxl, tidyselect, readr, magrittr, tidyverse
library(dplyr)
library(readxl)
library(tidyr)
library(tidyselect)
library(tidyverse)
library(magrittr)
library(readr)
#--------------------------------------
#Import table
myData <- read_excel("/Users/leianna/Documents/321F20/ProgLanProj-R/catsvdogs.xlsx")

#--------------------------------------
#print list of headers
printHeader <-function(dataEnter){
  list_header<-names(dataEnter)
  for(val in 1:length(list_header)){
    print(list_header[val])
  }
}

#call the function
printHeader(myData)

#--------------------------------------
#Create a function to find columns
#Used in Min, Max, Find functions
findColumn <-function(columnInput){
  if(toupper(columnInput)==toupper(list_header[1])){
    result<-myData$Location
  }else if(toupper(columnInput)==toupper(list_header[2])){
    result<-myData$`Number of Households (in 1000)`
  }else if(toupper(columnInput)==toupper(list_header[3])){
    result<-myData$`Percentage of households with pets`
  }else if(toupper(columnInput)==toupper(list_header[4])){
    result<-myData$`Number of Pet Households (in 1000)`
  }else if(toupper(columnInput)==toupper(list_header[5])){
    result<-myData$`Percentage of Dog Owners`
  }else if(toupper(columnInput)==toupper(list_header[6])){
    result<-myData$`Dog Owning Households (1000s)`
  }else if(toupper(columnInput)==toupper(list_header[7])){
    result<-myData$`Mean Number of Dogs per household`
  }else if(toupper(columnInput)==toupper(list_header[8])){
    result<-myData$`Dog Population (in 1000)`
  }else if(toupper(columnInput)==toupper(list_header[9])){
    result<-myData$`Percentage of Cat Owners`
  }else if(toupper(columnInput)==toupper(list_header[10])){
    result<-myData$`Cat Owning Households`
  }else if(toupper(columnInput)==toupper(list_header[11])){
    result<-myData$`Mean Number of Cats`
  }else if(toupper(columnInput)==toupper(list_header[12])){
    result<-myData$`Cat Population`
  }
  return(result)
}

#--------------------------------------
#Create function to find the min except Location
findMin <-function(myData){
  #Import data headers
  list_header<-names(myData)
  #creating list to store individual mins for each column
  minList = list()
  for(val in 2:length(list_header)){
    #store mins to the list 
    minList[[val]] <- myData[findColumn(list_header[val])==min(findColumn(list_header[val])),]
  }
  #merge all mins from the list into one table
  minTable = do.call(rbind,minList)
  #filter out duplicate rows
  minTable %>% distinct()
}

#call the function
minTable <- findMin(myData)

#--------------------------------------
#Create function to find the max except Location
findMax <-function(myData){
  #Import data headers
  list_header<-names(myData)
  #creating list to store individual maxs for each column
  maxList = list()
  for(val in 2:length(list_header)){
    #save maxs to the list
    maxList[[val]] <- myData[findColumn(list_header[val])==max(findColumn(list_header[val])),]
  }
  #merge all maxs from the table into one table
  maxTable = do.call(rbind,maxList)
  #filter out duplicate rows
  maxTable %>% distinct()
}

#call the function
maxTable <- findMax(myData)

#--------------------------------------
#Find rows that user input
findInfo <- function(){
  userInputColumn <- readline(prompt="Enter column name: ")
  if(toupper(userInputColumn)=="LOCATION"){
    #user input value as string
    userInputValue <- readline(prompt="Enter value to Find: ")
    #Find the row
    resultTable<-myData[findColumn(userInputColumn)==userInputValue,]
  }else{
    #user input value as mathmatical number
    userInputValue <- as.numeric(readline(prompt="Enter value to Find: "))
    
    resultList = list()
    for(val in 2:length(list_header)){
      #save results to the list
      resultList[[val]] <- myData[findColumn(userInputColumn)==userInputValue,]
    }
    #merge all results from the table into one table
    resultTable = do.call(rbind,resultList)
    #filter out duplicate rows
    resultTable %>% distinct()
  }
}

#Call function
resultTable<-findInfo()


