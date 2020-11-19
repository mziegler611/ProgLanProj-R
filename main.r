#--------------------------------------
#Author: Lei Chen, Morgan Ziegler
#Date: November 17,2020
#Comp321 final
#--------------------------------------
source("/Users/leianna/Documents/321F20/ProgLanProj-R/functions.R", chdir = TRUE)
#source("/Users/morganziegler/Desktop/ProgLanProj-R-main/functions.R", chdir = TRUE)

commands <- function(){
  print("Functions Available:", quote = FALSE)
  print("1. Print Data", quote = FALSE)
  print("2. Print Headers", quote = FALSE)
  print("3. Find Minimum", quote = FALSE)
  print("4. Find Maximum", quote = FALSE)
  print("5. Find Info", quote = FALSE)
  print("6. Find Average of Column", quote = FALSE)
  print("7. Find Frequency of A Value in Column", quote = FALSE)
  print("8. Reprint Commands", quote = FALSE)
}

main <- function(){
  #myData <- read_excel("/Users/morganziegler/Desktop/ProgLanProj-R-main/catsvdogs.xlsx")
  myData <- read_excel("/Users/leianna/Documents/321F20/ProgLanProj-R/catsvdogs.xlsx")
  options(tibble.print_max = Inf)
  options(tibble.width = Inf)
  list_header<-names(myData)
  input <- 'y'
  commands()
  userInputValue <- readline(prompt="What would you like to do: ")
  while (userInputValue != 'n'){
    
    if (userInputValue==1){
      print(myData)
    }
    else if(userInputValue == 2){
      printHeader(myData)
    }
    else if(userInputValue ==3){
      userInputColumn <- readline(prompt="Enter a column name/key or all: ")
      print(findMin(myData,userInputColumn))
    }
    else if(userInputValue == 4){
      userInputColumn <- readline(prompt="Enter a column name/key or all: ")
      print(findMax(myData,userInputColumn))
    }
    else if(userInputValue == 5){
      userInputColumn <- readline(prompt="Enter column name/key: ")
      userInputValue  <- readline(prompt="Enter value to Find: ")
      print(findInfo(userInputColumn, userInputValue))
    }
    else if(userInputValue == 6){
      columnInput <- readline(prompt="What Column do you want to average: ")
      print(findAvg(myData, columnInput))
    }
    else if(userInputValue == 7){
      columnInput <- readline(prompt="What Column do you want to look in: ")
      valueInput <- readline(prompt="What Value are you looking for: ")
      print(findFrequency(myData, columnInput, valueInput))
      
    }
    else if (userInputValue ==8) {
      commands()
    }
    userInputValue <- readline(prompt="What would you like to do (enter 'n' to quit): ")
  }
}

main()

