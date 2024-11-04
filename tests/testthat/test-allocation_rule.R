skip_on_cran()

file_name_for_test <- "for_test"
dirpath_for_test <- system.file("extdata", package = "RLoptimal")
filepath_for_test <- system.file("extdata/for_test", package = "RLoptimal")

setup_python()

test_that("generate object", {
  obj <- AllocationRule$new(dir = filepath_for_test)
  expect_equal(class(obj), c("AllocationRule", "R6"))
})

test_that("base_dir", {
  obj <- AllocationRule$new(dir = file_name_for_test, base_dir = dirpath_for_test)
  expect_equal(class(obj), c("AllocationRule", "R6"))
})

obj <- AllocationRule$new(dir = filepath_for_test)

test_that("opt_allocation_probs", {
  doses <- c( 0,  0,  0,  0,  2,  2,  4,  4,  4,  6,  6,   8,  8,   8)
  resps <- c(.2, .1, .0, .3, .2, .4, .1, .6, .8, .5, .8, 1.1, .9, 1.6)

  expected <- c(`0` = 0.2036620, `2` = 0.1981888, `4` = 0.1986541,
                `6` = 0.1962135, `8` = 0.2032815)
  act <- obj$opt_allocation_probs(doses, resps)

  expect_equal(expected, act, tolerance = 1e-6)
})

test_that("opt_allocation_probs - lengths of doses and resps do not match", {
  doses <- c( 0,  0,  0,  0,  2,  2,  4,  4,  4,  6,  6,   8,  8,   8)
  resps <- c(.2, .1, .0, .3, .2, .4, .1, .6, .8, .5, .8, 1.1, .9)

  expect_error(obj$opt_allocation_probs(doses, resps))
})

test_that("opt_allocation_probs - dose that was not present in training", {
  doses <- c( 0,  0,  0,  1,  2,  2,  4,  4,  4,  6,  6,   8,  8,   8)
  resps <- c(.2, .1, .0, .3, .2, .4, .1, .6, .8, .5, .8, 1.1, .9, 1.6)

  expect_error(obj$opt_allocation_probs(doses, resps))
})

test_that("opt_allocation_probs - count per action is less than 2", {
  doses <- c( 0)
  resps <- c(.2)

  expect_error(obj$opt_allocation_probs(doses, resps))
})
