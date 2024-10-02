#' Simulate One Trial Using an Obtained Optimal Adaptive Allocation Rule
#'
#' @param allocation_rule An object of class \link[RLoptimal]{AllocationRule}
#'        specifying an obtained optimal adaptive allocation rule.
#' @param models An object of class \link[DoseFinding]{Mods} specifying assumed
#'        dose-response models. When `outcome_type` is "binary", `models` should 
#'        be specified on the logit scale. This is used in the MCPMod method 
#'        at the end of this trial.
#' @param true_response A numeric vector specifying the true response values of 
#'        the true model. When `outcome_type` is "binary", `true_response` should 
#'        be specified on the logit scale.
#' @param N_total A positive integer value. The total number of subjects.
#' @param N_ini A positive integer vector in which each element is greater than 
#'        or equal to 2. The number of subjects initially assigned to each dose. 
#' @param N_block A positive integer value. The number of subjects allocated
#'        adaptively in each round.
#' @param Delta A positive numeric value. The clinically relevant target effect.
#'        When `outcome_type` is "binary", `Delta` should be specified 
#'        on the logit scale. See \link[DoseFinding]{TD} for details.
#' @param outcome_type A character value specifying the outcome type. 
#'        Possible values are "continuous" (default), and "binary".
#' @param sd_normal A positive numeric value. The standard deviation of the
#'        observation noise. When `outcome_type` is "continuous", 
#'        `sd_normal` must be specified.
#' @param alpha A positive numeric value. The significance level. Default is 0.025.
#' @param selModel A character value specifying the model selection criterion
#'        for dose estimation. Possible values are "AIC" (default), "maxT", or
#'        "aveAIC". See \link[DoseFinding]{MCPMod} for details.
#' @param seed An integer value. Random seed for data generation in this trial.
#' @param eval_type A character value specifying the evaluation type. Possible 
#'        values are "all" (default) and "pVal". "all" returns all metrics,
#'        which contain the minimum p value, the selected model name, 
#'        the estimated target dose, and the MAE. "pVal" returns only the 
#'        minimum p value without fitting models.
#'        
#' @returns A list which contains the minimum p value, the selected model name, 
#'        the estimated target dose, and the MAE.
#' 
#' @examples
#' library(RLoptimal)
#' 
#' doses <- c(0, 2, 4, 6, 8)
#' 
#' models <- DoseFinding::Mods(
#'   doses = doses, maxEff = 1.65,
#'   linear = NULL, emax = 0.79, sigEmax = c(4, 5)
#' )
#' 
#' \dontrun{
#' allocation_rule <- learn_allocation_rule(
#'   models,
#'   N_total = 150, N_ini = rep(10, 5), N_block = 10, Delta = 1.3,
#'   outcome_type = "continuous", sd_normal = sqrt(4.5), 
#'   seed = 123, rl_config = rl_config_set(iter = 1000),
#'   alpha = 0.025
#' )
#'
#' # Simulation-based adjustment of the significance level using `allocation_rule`
#' adjusted_alpha <- adjust_significance_level(
#'   allocation_rule, models,
#'   N_total = 150, N_ini = rep(10, 5), N_block = 10,
#'   outcome_type = "continuous", sd_normal = sqrt(4.5),
#'   alpha = 0.025, n_sim = 10000, seed = 123
#' )}
#' 
#' eval_models <- DoseFinding::Mods(
#'   doses = doses, maxEff = 1.65,
#'   linear = NULL, emax = 0.79, sigEmax = c(4, 5), exponential = 1, quadratic = - 1/12
#' )
#' true_response_matrix <- DoseFinding::getResp(eval_models, doses = doses)
#' true_response_list <- as.list(data.frame(true_response_matrix, check.names = FALSE))
#' 
#' true_model_name <- "emax"
#' 
#' # Simulate one trial using the obtained `allocation_rule` When the true model is "emax"
#' \dontrun{
#' res_one <- simulate_one_trial(
#'   allocation_rule, models, 
#'   true_response = true_response_list[[true_model_name]],
#'   N_total = 150, N_ini = rep(10, 5), N_block = 10, 
#'   Delta = 1.3, outcome_type = "continuous", sd_normal = sqrt(4.5),
#'   alpha = adjusted_alpha, seed = simID, eval_type = "all"
#' )}
#' 
#' @importFrom stats coef binomial glm plogis predict rbinom rnorm vcov
#' 
#' @export
simulate_one_trial <- function(
    allocation_rule, models, true_response, 
    N_total, N_ini, N_block, Delta, 
    outcome_type = c("continuous", "binary"), sd_normal = NULL,
    alpha = 0.025, selModel = c("AIC", "maxT", "aveAIC"), 
    seed = NULL, eval_type = c("all", "pVal")) {
  
  outcome_type <- match.arg(outcome_type)
  if (outcome_type == "continuous") {
    stopifnot("sd_normal must be specified when outcome_type = 'continuous'" = !is.null(sd_normal))
    sd_normal <- as.double(sd_normal)
    stopifnot(length(sd_normal) == 1L, sd_normal > 0)
  }
  
  doses <- attr(models, "doses")
  doses <- as.double(doses)
  stopifnot("'doses' must have >=2 distinct values" = length(unique(doses)) >= 2L)
  K <- length(doses)
  true_response <- as.double(true_response)
  stopifnot("'true_response' must have the same length of doses" = length(true_response) == length(doses))
  
  N_total <- as.integer(N_total)
  N_ini <- as.integer(N_ini)
  N_block <- as.integer(N_block)
  stopifnot(length(N_total) == 1L, N_total >= 1L)
  stopifnot(length(N_ini) == K, N_ini >= 2L)
  stopifnot(length(N_block) == 1L, N_block >= 1L)
  stopifnot((N_total - sum(N_ini)) %% N_block == 0.)
  selModel <- match.arg(selModel)
  eval_type <- match.arg(eval_type)
  
  set.seed(seed)
  actions <- seq_len(K)
  
  sim_actions <- rep(actions, times = N_ini)
  sim_doses <- doses[sim_actions]
  if (outcome_type == "continuous") {
    sim_resps <- rnorm(sum(N_ini), mean = true_response[sim_actions], sd = sd_normal)
  } else if (outcome_type == "binary") {
    sim_resps <- rbinom(sum(N_ini), size = 1, prob = plogis(true_response[sim_actions]))
  }
  
  while (length(sim_resps) < N_total) {
    probs <- allocation_rule$opt_allocation_probs(sim_doses, sim_resps)
    ns <- DoseFinding::rndDesign(probs, N_block)
    new_actions <- rep(actions, times = ns)
    new_doses <- doses[new_actions]
    if (outcome_type == "continuous") {
      new_resps <- rnorm(N_block, mean = true_response[new_actions], sd = sd_normal)
    } else if (outcome_type == "binary") {
      new_resps <- rbinom(N_block, size = 1, prob = plogis(true_response[new_actions]))
    }
    sim_actions <- c(sim_actions, new_actions)
    sim_doses <- c(sim_doses, new_doses)
    sim_resps <- c(sim_resps, new_resps)
  }

  if (outcome_type == "binary") {
    sim_Ns <- unname(tapply(sim_resps, sim_doses, length))
    sim_resp_rates <- unname(tapply(sim_resps, sim_doses, sum)) / sim_Ns
    # fit logistic regression (without intercept)
    logfit <- glm(sim_resp_rates ~ factor(doses) + 0, family = binomial, weights = sim_Ns)
    mu_hat <- coef(logfit)
    S_hat <- vcov(logfit)
  }
    
  if (eval_type == "pVal") {
    if (outcome_type == "continuous") {
      result_mct <- DoseFinding::MCTtest(
        dose = sim_doses, resp = sim_resps, models = models,
        alpha = alpha, pVal = TRUE, alternative = "one.sided")
    } else if (outcome_type == "binary") {
      result_mct <- DoseFinding::MCTtest(
        dose = doses, resp = mu_hat, S = S_hat, type = "general", models = models,
        alpha = alpha, pVal = TRUE, alternative = "one.sided")
    }
    p_values <- attr(result_mct$tStat, "pVal")
    min_p_value <- min(p_values)
    return(list(min_p_value = min_p_value))
  }
  
  # Execute MCP-Mod
  if (outcome_type == "continuous") {
    result_mcpmod <- DoseFinding::MCPMod(
      dose = sim_doses, resp = sim_resps, models = models,
      selModel = selModel, alpha = alpha, Delta = Delta,
      pVal = TRUE, alternative = "one.sided")
  } else if (outcome_type == "binary") {
    result_mcpmod <- DoseFinding::MCPMod(
      dose = doses, resp = mu_hat, S = S_hat, type = "general", models = models,
      selModel = selModel, alpha = 1, Delta = Delta,
      pVal = TRUE, alternative = "one.sided")
  }
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
