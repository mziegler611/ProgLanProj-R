#import test library
library(testthat)

#import file
source("/Users/leianna/Documents/321F20/ProgLanProj-R/functions.R", chdir = TRUE)
#source("/Users/morganziegler/Desktop/ProgLanProj-R-main/functions.R", chdir = TRUE)

#Import table
myData <- read_excel("/Users/leianna/Documents/321F20/ProgLanProj-R/catsvdogs.xlsx")
#myData <- read_excel("/Users/morganziegler/Desktop/ProgLanProj-R-main/catsvdogs.xlsx")


print("Find Min Tests" , quote=FALSE)
test_that("table", {
  expect_equal(findMin(myData,1),filter(myData, myData$Location== min(myData$Location)))
  expect_equal(findMin(myData,7),filter(myData, myData$`Mean Number of Dogs per household`==min(myData$`Mean Number of Dogs per household`)))
  expect_equal(findMin(myData,"Percentage of Dog Owners"),filter(myData, myData$`Percentage of Dog Owners`== min(myData$`Percentage of Dog Owners`)))
})

print("Find Max Tests" , quote=FALSE)
test_that("table", {
  expect_equal(findMax(myData,1),filter(myData, myData$Location== max(myData$Location)))
  expect_equal(findMax(myData,3),filter(myData, myData$`Percentage of households with pets`==max(myData$`Percentage of households with pets`)))
  expect_equal(findMax(myData,"Dog Population (in 1000)"),filter(myData, myData$`Dog Population (in 1000)`== max(myData$`Dog Population (in 1000)`)))
})

print("Find Information Tests" , quote=FALSE)
test_that("table", {
  expect_equal(findInfo("Location","Florida"),filter(myData, myData$Location == "Florida"))
  expect_equal(findInfo("Mean Number of Cats",2.1),filter(myData, myData$`Mean Number of Cats`==2.1))
  expect_equal(findInfo("9",2079),filter(myData, myData$`Percentage of Cat Owners`==2079))
})

print("Find Frequency Tests" , quote=FALSE)
test_that("single number", {
  expect_equal(findFrequency(myData, 3, 59.5), 2)
  expect_equal(findFrequency(myData, 1, 'Nebraska'), 1)
  expect_equal(findFrequency(myData, 5, -1), 0)
})

print("Find Average Tests", quote=FALSE)
test_that("single number", {
  expect_equal(findAvg(myData, 3), 56.859183)
  expect_equal(findAvg(myData, 4), 1342.591836)
  expect_equal(findAvg(myData, 5), 36.973469)
})
