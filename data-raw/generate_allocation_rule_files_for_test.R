library(RLoptimal)

doses <- c(0, 2, 4, 6, 8)

models <- DoseFinding::Mods(
  doses = doses, maxEff = 1.65,
  linear = NULL, emax = 0.79, sigEmax = c(4, 5)
)

allocation_rule <- learn_allocation_rule(
  models, N_total = 150, N_ini = rep(10, 5), N_block = 10, Delta = 1.3,
  outcome_type = "continuous", sd_normal = sqrt(4.5), 
  seed = 123, 
  rl_config = rl_config_set(iter = 1, model = rl_dnn_config(fcnet_hiddens = c(1, 1))),
  output_dir = "allocation_rule_for_test", output_base_dir = "inst/extdata",
  alpha = 0.025
)
