#' Simulate One Study Using an Obtained Optimal Adaptive Allocation Rule
#'
#' @param allocation_rule An object of class \link[RLoptimal]{AllocationRule}
#'        specifying an obtained optimal adaptive allocation rule.
#' @param models An object of class \link[DoseFinding]{Mods} specifying assumed
#'        dose-response models. This is used in the MCPMod method at the end of 
#'        this study.
#' @param true_model_name A character value specifying the true model name.
#' @param true_response A numeric vector specifying the true response values of 
#'        the true model.
#' @param N_total A positive integer value. The total number of subjects.
#' @param N_ini A positive integer vector in which each element is greater than 
#'        or equal to 2. The number of subjects initially assigned to each dose. 
#' @param N_block A positive integer value. The number of subjects allocated
#'        adaptively in each round.
#' @param Delta A positive numeric value. The clinically relevant target effect.
#'        See \link[DoseFinding]{TD} for details.
#' @param sd_normal A positive numeric value. The standard deviation of the
#'        observation noise.
#' @param alpha A positive numeric value. The significance level. Default is 0.025.
#' @param selModel A character value specifying the model selection criterion
#'        for dose estimation. Possible values are "AIC" (default), "maxT", or
#'        "aveAIC". See \link[DoseFinding]{MCPMod} for details.
#' @param seed An integer value. Random seed for data generation in this study.
#' @param eval_type A character value specifying the evaluation type. Possible 
#'        values are "all" (default) and "pVal". "all" returns all metrics,
#'        which contain the minimum p value, the selected model name, 
#'        the estimated target dose, and the MAE. "pVal" returns only the 
#'        minimum p value without fitting models.
#'        
#' @returns A list which contains the minimum p value, the selected model name, 
#'        the estimated target dose, and the MAE.
#'
#' @export
simulate_one_study <- function(
    allocation_rule, models, true_model_name, true_response, 
    N_total, N_ini, N_block, Delta, sd_normal,
    # type = c("normal", "general"),
    alpha = 0.025, selModel = c("AIC", "maxT", "aveAIC"), 
    seed = NULL, eval_type = c("all", "pVal")) {
  
  if (!is.null(seed)) {
    set.seed(seed)
  }
  
  doses <- attr(models, "doses")
  K <- length(doses)
  actions <- seq_len(K)
  
  sim_actions <- rep(actions, times = N_ini)
  sim_doses <- doses[sim_actions]
  sim_resps <- rnorm(sum(N_ini), mean = true_response[sim_actions], sd = sd_normal)
  
  while (length(sim_resps) < N_total) {
    probs <- allocation_rule$get_next_action_probs(sim_doses, sim_resps)
    ns <- DoseFinding::rndDesign(probs, N_block)
    new_actions <- rep(actions, times = ns)
    new_doses <- doses[new_actions]
    new_resps <- rnorm(N_block, mean = true_response[new_actions], sd = sd_normal)
    sim_actions <- c(sim_actions, new_actions)
    sim_doses <- c(sim_doses, new_doses)
    sim_resps <- c(sim_resps, new_resps)
  }
  
  if (eval_type == "pVal") {
    result_mct <- DoseFinding::MCTtest(
      dose = sim_doses, resp = sim_resps, models = models,
      alpha = alpha, pVal = TRUE, alternative = "one.sided")
    p_values <- attr(result_mct$tStat, "pVal")
    min_p_value <- min(p_values)
    return(list(min_p_value = min_p_value))
  }
  
  # Execute MCP-Mod
  result_mcpmod <- DoseFinding::MCPMod(
    dose = sim_doses, resp = sim_resps, models = models,
    selModel = selModel, alpha = alpha, Delta = Delta,
    pVal = TRUE, alternative = "one.sided")
  
  p_values <- attr(result_mcpmod$MCTtest$tStat, "pVal")
  min_p_value <- min(p_values)
  
  # No model is selected
  if (is.null(result_mcpmod$selMod)) {
    return(list(min_p_value = min_p_value, selected_model_name = NA,
                estimated_target_dose = NA, MAE = NA))
  }
  
  selected_model_name <- result_mcpmod$selMod
  selected_model <- result_mcpmod$mods[[selected_model_name]]
  
  estimated_target_dose <- result_mcpmod$doseEst[[selected_model_name]]
  
  estimated_response <- predict(selected_model, predType = "ls-means", doseSeq = doses)
  MAE <- compute_MAE(estimated_response, true_response)
  
  list(min_p_value = min_p_value, selected_model_name = selected_model_name,
       estimated_target_dose = estimated_target_dose, MAE = MAE)
}
