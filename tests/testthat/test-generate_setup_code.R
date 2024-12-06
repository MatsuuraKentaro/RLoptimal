# Base --------------------------------------------------------------------
doses <- c(0, 2, 4, 6, 8)
models <- DoseFinding::Mods(
  doses = doses, maxEff = 1.65, linear = NULL, emax = 0.79, sigEmax = c(4, 5)
)
Delta <- 1.3

obj <- new.env()
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

test_that("compute_reward fit well", {
  model_names <- c("linear", "emax", "sigEmax")
  optimization_metrics <- c("MAE", "power", "TD", "power and MAE")
  
  df_expected <- data.frame(
    linear = c(0.98473088, 1, 1, 0.98473088),
    emax = c(0.97718847, 1, 1, 0.97718847),
    sigEmax = c(0.9839088, 1, 1, 0.9839088),  # MacOS: 0.983908858
    row.names = optimization_metrics
  )
  
  for (model_name in model_names) {
    set.seed(123)
    sim_doses <- sample(doses, 10, replace = TRUE)
    actions <- seq_along(obj$doses)
    names(actions) <- obj$doses
    sim_actions <- actions[as.character(sim_doses)]
    true_resps <- obj$true_response_list[[model_name]][sim_actions]
    sim_resps <- rnorm(10, mean = true_resps, sd = 0.01)
    
    for (optimization_metric in optimization_metrics) {
      obj$optimization_metric <- optimization_metric
      act <- obj$compute_reward(model_name, sim_doses, sim_resps)
      expected <- df_expected[optimization_metric, model_name]
      expect_equal(act, expected, tolerance = 1e-7)
    }
  }
  
  obj$optimization_metric <- "MAE"
})

test_that("compute_reward don't fit well", {
  model_names <- c("linear", "emax", "sigEmax")
  optimization_metrics <- c("MAE", "power", "TD", "power and MAE")
  
  df_expected <- data.frame(
    linear = c(0.16667721, 0, 0, 0),
    emax = c(-0.64942815, 0, 0, 0),
    sigEmax = c(0.36050607, 0, 0, 0),
    row.names = optimization_metrics
  )
  
  for (model_name in model_names) {
    set.seed(123)
    sim_doses <- sample(doses, 10, replace = TRUE)
    actions <- seq_along(obj$doses)
    names(actions) <- obj$doses
    sim_actions <- actions[as.character(sim_doses)]
    true_resps <- obj$true_response_list[[model_name]][sim_actions]
    sim_resps <- rnorm(10, mean = true_resps, sd = 0.5)
    
    for (optimization_metric in optimization_metrics) {
      obj$optimization_metric <- optimization_metric
      act <- obj$compute_reward(model_name, sim_doses, sim_resps)
      expected <- df_expected[optimization_metric, model_name]
      expect_equal(act, expected)
    }
  }
  
  obj$optimization_metric <- "MAE"  
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


# outcome_type = binary ---------------------------------------------------
doses <- c(0, 0.5, 1.5, 2.5, 4)
models <- DoseFinding::Mods(
  doses = doses, placEff = qlogis(0.1), maxEff = qlogis(0.35) - qlogis(0.1),
  emax = c(0.25, 1), sigEmax = rbind(c(1, 3), c(2.5, 4)), betaMod = c(1.1, 1.1)
)
Delta <- 1.4

obj <- new.env()
code <- generate_setup_code(
  doses, models, Delta, outcome_type = "binary",
  optimization_metric = "MAE", rl_models = models, seed = 123, alpha = 0.05,
  selModel = "AIC", Delta_range = c(0.9, 1.1) * Delta)
eval(code, obj)

test_that("compute_reward outcome_type = binary fit well", {
  model_names <- c("emax1", "emax2", "sigEmax1", "sigEmax2", "betaMod")
  optimization_metrics <- c("MAE", "power", "TD", "power and MAE")
  
  df_expected <- data.frame(
    emax1 = c(0.95762338, 1, 1, 0.95762338),
    emax2 = c(0.97962054, 1, 1, 0.97962054),
    sigEmax1 = c(0.92066444, 1, 1, 0.92066444),
    sigEmax2 = c(0.89181074, 1, 1, 0.89181074),
    betaMod = c(0.96190979, 1, 1, 0.96190979),
    row.names = optimization_metrics
  )
  
  for (model_name in model_names) {
    set.seed(123)
    sim_doses <- sample(doses, 1400, replace = TRUE)
    actions <- seq_along(obj$doses)
    names(actions) <- obj$doses
    sim_actions <- actions[as.character(sim_doses)]
    true_resps <- obj$true_response_list[[model_name]][sim_actions]
    sim_resps <- rbinom(1400, 1, plogis(true_resps))
    
    for (optimization_metric in optimization_metrics) {
      obj$optimization_metric <- optimization_metric
      act <- obj$compute_reward(model_name, sim_doses, sim_resps)
      expected <- df_expected[optimization_metric, model_name]
      expect_equal(act, expected)
    }
  }
  
  obj$optimization_metric <- "MAE"
})

test_that("compute_reward outcome_type = binary don't fit well", {
  model_names <- c("emax1", "emax2", "sigEmax1", "sigEmax2", "betaMod")
  optimization_metrics <- c("MAE", "power", "TD", "power and MAE")
  
  df_expected <- data.frame(
    emax1 = c(-0.21123768, 0, 0, 0),
    emax2 = c(-0.32752064, 0, 0, 0),
    sigEmax1 = c(0.043401334, 0, 0, 0),
    sigEmax2 = c(-0.89546023, 0, 0, 0),
    betaMod = c(-0.52750171, 0, 0, 0),
    row.names = optimization_metrics
  )
  
  for (model_name in model_names) {
    set.seed(123)
    sim_doses <- sample(doses, 100, replace = TRUE)
    actions <- seq_along(obj$doses)
    names(actions) <- obj$doses
    sim_actions <- actions[as.character(sim_doses)]
    true_resps <- obj$true_response_list[[model_name]][sim_actions]
    sim_resps <- rbinom(100, 1, plogis(true_resps))
    
    for (optimization_metric in optimization_metrics) {
      obj$optimization_metric <- optimization_metric
      act <- obj$compute_reward(model_name, sim_doses, sim_resps)
      expected <- df_expected[optimization_metric, model_name]
      expect_equal(act, expected)
    }
  }
  
  obj$optimization_metric <- "MAE"
})
