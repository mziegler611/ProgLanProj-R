#import test library
library(testthat)
#import file
source("/Users/leianna/Documents/321F20/ProgLanProj-R/main.R", chdir = TRUE)
#source("/Users/morganziegler/Desktop/ProgLanProj-R-main/main.R")
#Import table
myData <- read_excel("/Users/leianna/Documents/321F20/final/catsvdogs.xlsx")
#myData <- read_excel("/Users/morganziegler/Desktop/ProgLanProj-R-main/catsvdogs.xlsx")
list_header<-names(myData)


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






