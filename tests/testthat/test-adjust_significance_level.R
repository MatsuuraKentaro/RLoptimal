allocation_rule <- structure(list(), class = "AllocationRule")
doses <- c(0, 2, 4, 6, 8)
models <- DoseFinding::Mods(
  doses = doses, maxEff = 1.65,
  linear = NULL, emax = 0.79, sigEmax = c(4, 5)
)
N_total <- 150L
N_ini <- rep(10L, 5L)
N_block <- 10L

local_mocked_bindings(
  simulate_one_trial = function(...) list(min_p_value = runif(1L))
)

test_that("when not adjusting", {
  act <- adjust_significance_level(allocation_rule, models, 
                                   N_total, N_ini, N_block, sd_normal = 1.0,
                                   alpha = 0.025, seed = 1)
  expect_equal(act, expected = 0.025)
})

test_that("when adjusting", {
  act <- adjust_significance_level(allocation_rule, models, 
                                   N_total, N_ini, N_block, sd_normal = 1.0,
                                   alpha = 0.025, seed = 3)
  expect_equal(act, expected = 0.02337515, tolerance = 1e-6)
})

test_that("result has no name", {
  act <- adjust_significance_level(allocation_rule, models, 
                                   N_total, N_ini, N_block, sd_normal = 1.0)
  expect_null(names(act))
})
