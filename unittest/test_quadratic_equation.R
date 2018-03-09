# Load libraries
library(testthat)

# Source Files
source("quadratic_function.R")

# Define Context
context("Functionality of quadratic_root function")

# Define Tests
test_that("roots are distinct values", { 
  calculated_root <- quadratic_equation(1, 7, 10)
  
  expect_is(calculated_root, "numeric")
  expect_length(calculated_root, 2)
  expect_lt( calculated_root[1], calculated_root[2])
  }
)

test_that("functions throws correct error", { 
  expect_warning(quadratic_equation(0, 7, 10))
  }
)

test_that("function returns only one value for repeated roots", { 
  calculated_root <- quadratic_equation(1, 6000, 9000000)
  expect_length(calculated_root, 1)
  expect_equal(calculated_root, -3000)
  }
)
