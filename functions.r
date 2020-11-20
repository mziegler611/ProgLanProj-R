#--------------------------------------
#Author: Lei Chen, Morgan Ziegler
#Date: November 17,2020
#Comp321 final
#--------------------------------------
#install.packages("")
library(readxl)
library(tidyselect)
library(dplyr)
library(readxl)
library(tidyr)
library(tidyselect)
library(magrittr)
library(readr)

#Import table
myData <- read_excel("/Users/leianna/Documents/321F20/ProgLanProj-R/catsvdogs.xlsx")
#myData <- read_excel("/Users/morganziegler/Desktop/ProgLanProj-R-main/catsvdogs.xlsx")

options(tibble.print_max = Inf)
options(tibble.width = Inf)

#print(myData)
list_header<-names(myData)

#--------------------------------------
#print list of headers
printHeader <-function(dataEnter){
  list_header<-names(dataEnter)
  for(val in 1:length(list_header)){
    print(list_header[val])
  }
}

#call the function
#printHeader(myData)

#--------------------------------------
#Create a function to find columns
#Used in Min, Max, Find functions
findColumn <-function(columnInput){
  #input string or number
  if(toupper(columnInput)==toupper(list_header[1]) || columnInput=="1"){
    result<-myData$Location
  }else if(toupper(columnInput)==toupper(list_header[2]) || columnInput=="2"){
    result<-myData$`Number of Households (in 1000)`
  }else if(toupper(columnInput)==toupper(list_header[3]) || columnInput=="3"){
    result<-myData$`Percentage of households with pets`
  }else if(toupper(columnInput)==toupper(list_header[4]) || columnInput=="4"){
    result<-myData$`Number of Pet Households (in 1000)`
  }else if(toupper(columnInput)==toupper(list_header[5]) || columnInput=="5"){
    result<-myData$`Percentage of Dog Owners`
  }else if(toupper(columnInput)==toupper(list_header[6]) || columnInput=="6"){
    result<-myData$`Dog Owning Households (1000s)`
  }else if(toupper(columnInput)==toupper(list_header[7]) || columnInput=="7"){
    result<-myData$`Mean Number of Dogs per household`
  }else if(toupper(columnInput)==toupper(list_header[8]) || columnInput=="8"){
    result<-myData$`Dog Population (in 1000)`
  }else if(toupper(columnInput)==toupper(list_header[9]) || columnInput=="9"){
    result<-myData$`Percentage of Cat Owners`
  }else if(toupper(columnInput)==toupper(list_header[10]) || columnInput=="10"){
    result<-myData$`Cat Owning Households`
  }else if(toupper(columnInput)==toupper(list_header[11]) || columnInput=="11"){
    result<-myData$`Mean Number of Cats`
  }else if(toupper(columnInput)==toupper(list_header[12]) || columnInput=="12"){
    result<-myData$`Cat Population`
  }
  return(result)
}


#--------------------------------------
#Create function to find the min except Location
findMin <-function(myData,userInputColumn){
  #find a min for a column, or find all column's min
  if(toupper(userInputColumn)=="ALL"){
    #creating list to store individual mins for each column
    minList = list()
    for(val in 2:length(names(myData))){
      minList[[val]] <- myData[findColumn(val)==min(findColumn(val)),]
    }
    #merge all mins from the list into one table
    minTable = do.call(rbind,minList)
    #filter out duplicate rows
    minTable %>% distinct()
  }else{
    minTable<-myData[findColumn(userInputColumn)==min(findColumn(userInputColumn)),]
  }
}

#call the function
#minTable <- findMin(myData,"Percentage of households with pets")
#print("Min Table")
#print(minTable)


#--------------------------------------
#Create function to find the max except Location
findMax <-function(myData,userInputColumn){
  
  #find a max for a column, or find all column's max
  if(toupper(userInputColumn)=="ALL"){
    #creating list to store individual maxs for each column
    maxList = list()
    for(val in 2:length(names(myData))){
      maxList[[val]] <- myData[findColumn(val)==max(findColumn(val)),]
    }
    #merge all maxs from the table into one table
    maxTable = do.call(rbind,maxList)
    #filter out duplicate rows
    maxTable %>% distinct()
  }else{
    maxTable<-myData[findColumn(userInputColumn)==max(findColumn(userInputColumn)),]
  }
}

#call the function
#maxTable <- findMax(myData,"all")
#print("Max Table")
#print(maxTable)

#--------------------------------------
#Find rows that user input
findInfo <- function(userInputColumn,userInputValue){
  if(toupper(userInputColumn)=="LOCATION"||userInputColumn==1){
    #user input value as string
    resultTable<-myData[findColumn(userInputColumn)==userInputValue,]
  }else{
    #userInputValue <- as.numeric(userInputValue)
    resultList = list()
    for(val in 2:length(list_header)){
      resultList[[val]] <- myData[findColumn(userInputColumn)==userInputValue,]
    }
    #merge all results from the table into one table
    resultTable = do.call(rbind,resultList)
    #filter out duplicate rows
    resultTable %>% distinct()
  }
}

#Call function
#resultTable<-findInfo("Dog Population (in 1000)", 507)
# print("Result Table")
# print(resultTable)

#--------------------------------------
#Create function to find the average of a column except Location
findAvg <-function(myData, columnInput){
  sum <- 0
  result <- findColumn(columnInput)
  
  if ((columnInput != 1) && (toupper(columnInput) != 'LOCATION')){
    for (val in result){
      sum <- sum + val
      
    }
    return (sum/length(result))
  }
  return ('Average of Location Not possible')
  
}

#call the function
# avg <- findAvg(myData, "Percentage of households with pets")
# print("Average of Percentage of households with pets: ")
# print (avg)

#--------------------------------------
#Create function to find the frequency of a result in a column 
findFrequency <-function(myData, columnInput, numToFind){
  frequency <- 0
  result <- findColumn(columnInput)
  
  for (val in result){
    if (val == numToFind){
      frequency <- frequency + 1
    }    
  }
  return (frequency)
}

#call the function
#print("Frequency of 59.3 in Percentage of households with pets: ")
#frq <- findFrequency(myData, 3, 59.5)
#print (frq)