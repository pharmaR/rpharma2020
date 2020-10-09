context("Req 01: Can subset by specified columns")
library(dplyr)

# Data frame with 3 records
air3 <- airquality[1:3,]

test_that("Select based on names", {

  wind_temp <- select(air3, Wind, Temp)
  expect_equal(names(wind_temp), c("Wind", "Temp"))
  
})

test_that("Select based on number", {
  
  wind_temp <- select(air3, 3:4)
  expect_equal(names(wind_temp), c("Wind", "Temp"))
  
})
