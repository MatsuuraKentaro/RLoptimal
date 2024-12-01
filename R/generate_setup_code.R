generate_setup_code <- function(
    doses, models, Delta, outcome_type, optimization_metric, rl_models, seed, alpha, selModel, Delta_range) {

  substitute({
    restore_R_object <- function(deparsed_object) {
      eval(parse(text = deparsed_object))
    }

    set.seed(SEED)
    
    doses <- DOSES
    models <- restore_R_object(MODELS)
    Delta <- DELTA
    outcome_type <- OUTCOME_TYPE
    optimization_metric <- OPTIMIZATION_METRIC
    rl_models <- restore_R_object(RL_MODELS)
    alpha <- ALPHA
    Delta_lower <- DELTA_LOWER
    Delta_upper <- DELTA_UPPER
    model_selection_criterion <- SEL_MODEL

    max_effect <- attr(models, "maxEff")
    direction <- attr(models, "direction")
    true_response_matrix <- DoseFinding::getResp(rl_models, doses = doses)
    true_response_list <- as.list(data.frame(true_response_matrix, check.names = FALSE))

    # For use in MCPModEnv.py
    true_responses <- unname(unlist(true_response_list))

    # Utility functions
    compute_reward_TD <- function(estimated_target_dose, true_model_name) {
      # estimated_target_dose is possibly NA
      if (is.na(estimated_target_dose)) return(0)

      # Note: Calculating TD for all DR models is costly, but extracting
      # a single model from 'rl_models' is troublesome.
      # The overhead is about 100ms per execution.
      lower <- DoseFinding::TD(rl_models, Delta = Delta_lower, direction = direction)
      lower <- unname(lower[true_model_name])
      upper <- DoseFinding::TD(rl_models, Delta = Delta_upper, direction = direction)
      upper <- unname(upper[true_model_name])
      # TD returns NA for large Delta, so if NA, return Inf.
      lower <- ifelse(is.na(lower), Inf, lower)
      upper <- ifelse(is.na(upper), Inf, upper)
      
      reward_TD <- ifelse(lower < estimated_target_dose && estimated_target_dose < upper, 1, 0)
      return(reward_TD)
    }

    compute_MAE <- function(estimated_response, true_response) {
      shifted_estimates <- estimated_response - estimated_response[1L]
      shifted_true <- true_response - true_response[1L]
      errors <- shifted_estimates[-1L] - shifted_true[-1L]
      mean(abs(errors))
    }

    # This has been modified from the formula as described in the original paper.
    compute_reward_MAE <- function(MAE) {
      scaling_factor <- 0.5 * abs(max_effect)
      reward_MAE <- 1 - MAE / scaling_factor
      reward_MAE
    }

    # For use in MCPModEnv.py
    compute_reward <- function(true_model_name, sim_doses, sim_resps) {
      
      if (outcome_type == "binary") {
        sim_Ns <- unname(tapply(sim_resps, sim_doses, length))
        sim_resp_rates <- unname(tapply(sim_resps, sim_doses, sum)) / sim_Ns
        # fit logistic regression (without intercept)
        logfit <- glm(sim_resp_rates ~ factor(doses) + 0, family = binomial, weights = sim_Ns)
        mu_hat <- coef(logfit)
        S_hat <- vcov(logfit)
      }
      
      # Execute MCP-Mod
      if (outcome_type == "continuous") {
        result_mcpmod <- DoseFinding::MCPMod(
          dose = sim_doses, resp = sim_resps, models = models,
          selModel = model_selection_criterion, alpha = alpha, Delta = Delta,
          pVal = TRUE, alternative = "one.sided")
      } else if (outcome_type == "binary") {
        result_mcpmod <- DoseFinding::MCPMod(
          dose = doses, resp = mu_hat, S = S_hat, type = "general", models = models,
          selModel = model_selection_criterion, alpha = alpha, Delta = Delta,
          pVal = TRUE, alternative = "one.sided")
      }
      
      # Power (detecting dose-response)
      if (optimization_metric == "power") {
        # Minimum p-value for power
        p_values <- attr(result_mcpmod$MCTtest$tStat, "pVal")
        min_p_value <- min(p_values)
        reward_power <- ifelse(min_p_value < alpha, 1, 0)
        return(reward_power)
      }
       
      # When no model is selected, fit all models by setting alpha = 1
      if (is.null(result_mcpmod$selMod)) {
        if (optimization_metric == "power and MAE") {
          return(0)
        }
        if (outcome_type == "continuous") {
          result_mcpmod <- DoseFinding::MCPMod(
            dose = sim_doses, resp = sim_resps, models = models,
            selModel = model_selection_criterion, alpha = 1, Delta = Delta,
            pVal = TRUE, alternative = "one.sided")
        } else if (outcome_type == "binary") {
          result_mcpmod <- DoseFinding::MCPMod(
            dose = doses, resp = mu_hat, S = S_hat, type = "general", models = models,
            selModel = model_selection_criterion, alpha = 1, Delta = Delta,
            pVal = TRUE, alternative = "one.sided")
        }
      }
      
      selected_model_name <- result_mcpmod$selMod
      
      # TD (accuracy of estimated target dose)
      if (optimization_metric == "TD") {
        # Estimated target dose for TD (continuous value or NA, not necessarily one of the doses)
        estimated_target_dose <- result_mcpmod$doseEst[[selected_model_name]]
        reward_TD <- compute_reward_TD(estimated_target_dose, true_model_name)
        return(reward_TD)
      }
      
      # MAE (mean absolute error) (default)
      selected_model <- result_mcpmod$mods[[selected_model_name]]
      estimated_response <- predict(selected_model, predType = "ls-means", doseSeq = doses)
      true_response <- true_response_list[[true_model_name]]
      MAE <- compute_MAE(estimated_response, true_response)
      reward_MAE <- compute_reward_MAE(MAE)
      reward_MAE <- ifelse(optimization_metric == "power and MAE", max(reward_MAE, 0), reward_MAE)
      return(reward_MAE)
    }
  }, list(DOSES = doses, MODELS = deparse(models), DELTA = Delta, 
          OUTCOME_TYPE = outcome_type,
          OPTIMIZATION_METRIC = optimization_metric,
          RL_MODELS = deparse(rl_models), SEED = seed, 
          ALPHA = alpha, SEL_MODEL = selModel,
          DELTA_LOWER = Delta_range[1], DELTA_UPPER = Delta_range[2]))
}
