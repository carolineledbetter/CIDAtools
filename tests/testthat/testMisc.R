context('First Tests')
library(CIDAtools)

testthat::test_that('check n levels P', {
  expect_equal(nLevelsP(1:3), "0")
})

testthat::test_that('check n levels P', {
  expect_equal(nLevelsP(factor(LETTERS)), "26")
})

