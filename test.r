#import test library
library(testthat)
#import file
source("/Users/leianna/Documents/321F20/ProgLanProj-R/main.R", chdir = TRUE)
#Import table
myData <- read_excel("/Users/leianna/Documents/321F20/final/catsvdogs.xlsx")

test_that("Headers", {
  expect_equal(printHeader(myData), list("Location", "Number of Households (in 1000)", "Percentage of households with pets", "Number of Pet Households (in 1000)", "Percentage of Dog Owners", "Dog Owning Households (1000s)", "Mean Number of Dogs per household", "Dog Population (in 1000)", "Percentage of Cat Owners", "Cat Owning Households", "Mean Number of Cats", "Cat Population"))
})
