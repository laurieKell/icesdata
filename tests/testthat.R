# This file is part of the standard testthat setup
# It ensures that the package is loaded before running tests

library(testthat)
library(icesdata)

test_check("icesdata")
