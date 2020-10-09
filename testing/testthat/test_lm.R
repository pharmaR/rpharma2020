context("Req 99: Implements a linear model")
library(stats)

test_that("Coefficients generated as expected", {
  # Data and results are taken from:
  # Dobson AJ, Barnett AG.
  # An Introduction to Generalized Linear Models. 3rd ed
  # CRC Press. 2008.

  # table 6.3 page 96
  d <- data.frame(
    carbohydrate = c(33, 40, 37, 27, 30, 43, 34, 48, 30, 38, 50, 51, 30, 36, 41, 42, 46, 24, 35, 37),
    age = c(33, 47, 49, 35, 46, 52, 62, 23, 32, 42, 31, 61, 63, 40, 50, 64, 56, 61, 48, 28),
    weight = c(100, 92, 135, 144, 140, 101, 95, 101, 98, 105, 108, 85, 130, 127, 109, 107, 117, 100, 118, 102),
    protein = c(14, 15, 18, 12, 15, 15, 14, 17, 15, 14, 17, 19, 19, 20, 15, 16, 18, 13, 18, 14)
  )

  fit <- lm(carbohydrate ~ age + weight + protein, data = d)

  # table 6.4 page 97
  expect_equivalent(
    round(coefficients(fit), 3),
    c(36.960, -0.114, -0.228, 1.958)
  )
  expect_equivalent(
    round(summary(fit)$coefficients[,"Std. Error"], 3),
    c(13.071, 0.109, 0.083, 0.635)
  )
  expect_equal(
    round(summary(fit)$coefficients[,"Std. Error"], 3),
    c(13.071, 0.109, 0.083, 0.635)
  )


})
