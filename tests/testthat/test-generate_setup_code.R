# Base --------------------------------------------------------------------
doses <- c(0, 2, 4, 6, 8)
models <- DoseFinding::Mods(
  doses = doses, maxEff = 1.65, linear = NULL, emax = 0.79, sigEmax = c(4, 5)
)
Delta <- 1.3

obj <- new.env(parent = baseenv())
code <- generate_setup_code(
  doses, models, Delta, outcome_type = "continuous",
  optimization_metric = "MAE", rl_models = models,seed = 123, alpha = 0.025,
  selModel = "AIC", Delta_range = c(0.9, 1.1) * Delta)
eval(code, obj)

test_that("compute_reward_TD estimated_target_dose = NA", {
  act <- obj$compute_reward_TD(NA, "linear")
  expect_equal(act, expected = 0)
})

test_that("compute_reward_TD", {
  act <- obj$compute_reward_TD(5, "linear")
  expect_equal(act, expected = 0)
  act <- obj$compute_reward_TD(6, "linear")
  expect_equal(act, expected = 1)
  act <- obj$compute_reward_TD(7, "linear")
  expect_equal(act, expected = 0)
  act <- obj$compute_reward_TD(1, "emax")
  expect_equal(act, expected = 0)
  act <- obj$compute_reward_TD(2, "emax")
  expect_equal(act, expected = 1)
  act <- obj$compute_reward_TD(3, "emax")
  expect_equal(act, expected = 0)
  act <- obj$compute_reward_TD(4, "sigEmax")
  expect_equal(act, expected = 0)
  act <- obj$compute_reward_TD(5, "sigEmax")
  expect_equal(act, expected = 1)
  act <- obj$compute_reward_TD(6, "sigEmax")
  expect_equal(act, expected = 0)
})

test_that("compute_MAE", {
  set.seed(123)
  x <- rnorm(10)
  y <- rnorm(10)
  act <- obj$compute_MAE(x, y)
  expect_equal(act, expected = 1.8339568)
})

test_that("compute_reward_MAE", {
  act <- obj$compute_reward_MAE(0.1)
  expect_equal(act, expected = 0.87878788)
})


# Large Delta -------------------------------------------------------------
doses <- c(0, 2, 4, 6, 8)
models <- DoseFinding::Mods(
  doses = doses, maxEff = 1.65, linear = NULL, emax = 0.79, sigEmax = c(4, 5)
)
Delta <- 1.9

obj <- new.env(parent = baseenv())
code <- generate_setup_code(
  doses, models, Delta, outcome_type = "continuous",
  optimization_metric = "MAE", rl_models = models,seed = 123, alpha = 0.025,
  selModel = "AIC", Delta_range = c(0.9, 1.1) * Delta)
eval(code, obj)

test_that("compute_reward_TD large Delta", {
  act <- obj$compute_reward_TD(8, "linear")
  expect_equal(act, expected = 0)
  act <- obj$compute_reward_TD(9, "linear")
  expect_equal(act, expected = 1)
  act <- obj$compute_reward_TD(10, "linear")
  expect_equal(act, expected = 1)
  act <- obj$compute_reward_TD(11, "linear")
  expect_equal(act, expected = 0)
  act <- obj$compute_reward_TD(13, "emax")
  expect_equal(act, expected = 0)
  act <- obj$compute_reward_TD(14, "emax")
  expect_equal(act, expected = 1)
  act <- obj$compute_reward_TD(1000, "emax")
  expect_equal(act, expected = 1)
  act <- obj$compute_reward_TD(1000, "sigEmax")
  expect_equal(act, expected = 0)
})


# direction = decreasing --------------------------------------------------
doses <- c(0, 2, 4, 6, 8)
models <- DoseFinding::Mods(
  doses = doses, maxEff = -1.65, linear = NULL, emax = 0.79, sigEmax = c(4, 5),
  direction = "decreasing"
)
Delta <- 1.3

obj <- new.env(parent = baseenv())
code <- generate_setup_code(
  doses, models, Delta, outcome_type = "continuous",
  optimization_metric = "MAE", rl_models = models,seed = 123, alpha = 0.025,
  selModel = "AIC", Delta_range = c(0.9, 1.1) * Delta)
eval(code, obj)

test_that("compute_reward_TD direction = decreasing", {
  act <- obj$compute_reward_TD(5, "linear")
  expect_equal(act, expected = 0)
  act <- obj$compute_reward_TD(6, "linear")
  expect_equal(act, expected = 1)
  act <- obj$compute_reward_TD(7, "linear")
  expect_equal(act, expected = 0)
  act <- obj$compute_reward_TD(1, "emax")
  expect_equal(act, expected = 0)
  act <- obj$compute_reward_TD(2, "emax")
  expect_equal(act, expected = 1)
  act <- obj$compute_reward_TD(3, "emax")
  expect_equal(act, expected = 0)
  act <- obj$compute_reward_TD(4, "sigEmax")
  expect_equal(act, expected = 0)
  act <- obj$compute_reward_TD(5, "sigEmax")
  expect_equal(act, expected = 1)
  act <- obj$compute_reward_TD(6, "sigEmax")
  expect_equal(act, expected = 0)
})
