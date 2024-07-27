generate_setup_code <- function(
    doses, models, Delta, rl_models, rl_seed, alpha, selModel, Delta_range) {

  substitute({
    restore_R_object <- function(deparsed_object) {
      eval(parse(text = deparsed_object))
    }

    doses <- DOSES
    dr_models <- restore_R_object(MODELS)
    Delta <- DELTA
    dr_models_for_RL <- restore_R_object(RL_MODELS)
    seed <- RL_SEED
    alpha <- ALPHA
    Delta_lower <- DELTA_LOWER
    Delta_upper <- DELTA_UPPER
    model_selection_criterion <- SEL_MODEL

    max_dose <- max(doses)
    placebo_effect <- attr(dr_models, "placEff")
    max_effect <- attr(dr_models, "maxEff")
    true_response_matrix <- DoseFinding::getResp(dr_models_for_RL, doses = doses)
    true_response_list <- as.list(data.frame(true_response_matrix, check.names = FALSE))

    # Setting a random seed
    set.seed(seed)

    # For use in MCPModEnv.py
    true_responses <- unname(unlist(true_response_list))

    # Utility functions
    compute_in_range <- function(estimated_target_dose, true_dr_model_name) {
      # estimated_target_dose is possibly NA
      if (is.na(estimated_target_dose)) return(FALSE)

      # Note: Calculating TD for all DR models is costly, but extracting
      # a single model from 'dr_models_for_RL' is troublesome.
      # The overhead is about 100ms per execution.
      lower <- DoseFinding::TD(dr_models_for_RL, Delta = Delta_lower)
      lower <- unname(lower[true_dr_model_name])
      upper <- DoseFinding::TD(dr_models_for_RL, Delta = Delta_upper)
      upper <- unname(upper[true_dr_model_name])
      # Clip at max_dose. If NA, return max_dose.
      upper <- min(upper, max_dose, na.rm = TRUE)

      lower < estimated_target_dose && estimated_target_dose < upper
    }

    compute_MAE <- function(estimated_response, true_response) {
      shifted_estimates <- estimated_response - estimated_response[1L]
      shifted_true <- true_response - true_response[1L]
      errors <- shifted_estimates[-1L] - shifted_true[-1L]
      mean(abs(errors))
    }

    # This has been modified from the formula as described in the original paper.
    compute_score_MAE <- function(MAE) {
      scaling_factor <- 2 * abs(max_effect - placebo_effect)
      score_MAE <- 1 - MAE / scaling_factor
      score_MAE <- max(score_MAE, 0)  # Clip at 0
      score_MAE
    }

    compute_metric_base <- function(true_dr_model_name, true_response = NULL,
                                    simulated_dose, simulated_response) {

      # Execute MCP-Mod (multiple comparison procedure - modeling)
      result_mcpmod <- DoseFinding::MCPMod(
        dose = simulated_dose, resp = simulated_response, models = dr_models,
        selModel = model_selection_criterion , alpha = alpha, Delta = Delta,
        pVal = TRUE, alternative = "one.sided")

      # Minimum p-value for power (not the selMod's p-value)
      p_values <- attr(result_mcpmod$MCTtest$tStat, "pVal")
      min_p_value <- min(p_values)

      # No model is selected
      if (is.null(result_mcpmod$selMod)) {
        return(list(min_p_value = min_p_value, selected_dr_model_name = NA,
                    estimated_target_dose = NA, MAE = NA))
      }

      # Selected model for MS and MAE
      selected_dr_model_name <- result_mcpmod$selMod
      selected_dr_model <- result_mcpmod$mods[[selected_dr_model_name]]

      # Estimated target dose for TD (continuous value or NA, not necessarily one of the doses)
      estimated_target_dose <- result_mcpmod$doseEst[[selected_dr_model_name]]

      estimated_response <- predict(selected_dr_model, predType = "ls-means", doseSeq = doses)
      if (is.null(true_response)) {
        true_response <- true_response_list[[true_dr_model_name]]
      }
      MAE <- compute_MAE(estimated_response, true_response)

      list(min_p_value = min_p_value, selected_dr_model_name = selected_dr_model_name,
           estimated_target_dose = estimated_target_dose, MAE = MAE)
    }

    # For use in MCPModEnv.py
    compute_scores <- function(
        true_dr_model_name, simulated_dose, simulated_response) {

      metric_base <- compute_metric_base(
          true_dr_model_name, NULL, simulated_dose, simulated_response)

      min_p_value <- metric_base$min_p_value
      selected_dr_model_name <- metric_base$selected_dr_model_name
      estimated_target_dose <- metric_base$estimated_target_dose
      MAE <- metric_base$MAE

      # No model is selected
      if (is.na(selected_dr_model_name)) {
        return(c(min_p_value, NA, NA, 0, 0, 0, 0))
      }

      # Power (detecting dose-response)
      is_detected_some_model <- min_p_value < alpha
      score_power <- ifelse(is_detected_some_model, 1, 0)

      # MS (accuracy of model selection)
      score_MS <- ifelse(selected_dr_model_name == true_dr_model_name, 1, 0)

      # TD (accuracy of a target dose)
      in_range <- compute_in_range(estimated_target_dose, true_dr_model_name)
      score_TD <- ifelse(in_range, 1, 0)

      # MAE (mean absolute error)
      score_MAE <- compute_score_MAE(MAE)

      c(min_p_value, selected_dr_model_name, estimated_target_dose,
        score_power, score_MS, score_TD, score_MAE)
    }
  }, list(DOSES = doses, MODELS = deparse(models), DELTA = Delta,
          RL_MODELS = deparse(rl_models), RL_SEED = rl_seed, ALPHA = alpha,
          SEL_MODEL = selModel, DELTA_LOWER = Delta_range[1], DELTA_UPPER = Delta_range[2]))
}
