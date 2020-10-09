context("Req 02: Can subset based on logical condition")
library(dplyr)

# Data frame with 3 records
air3 <- airquality[1:3,]

# A conditional filter
high_ozone <- filter(air3, Ozone >30)  

test_that("Returns the right number of rows", {

  expect_equal(nrow(high_ozone), 2)
  
})

test_that("Returns correct records", {
  
  expect_equal(high_ozone$Ozone, c(41, 36))
  
})
