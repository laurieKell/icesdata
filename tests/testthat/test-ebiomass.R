# Test file for ebiomass function

test_that("ebiomass function exists and is exported", {
  expect_true(exists("ebiomass"))
  expect_true(isGeneric("ebiomass"))
})

# Note: Additional tests would require FLStock objects
# which are typically loaded from FLCore or created from data
