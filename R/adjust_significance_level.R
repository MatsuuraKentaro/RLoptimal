#' Adjust Significance Level on a Simulation Basis 
#'
#' @param allocation_rule An object of class \link[RLoptimal]{AllocationRule}
#'        specifying an obtained optimal adaptive allocation rule.
#' @param models An object of class \link[DoseFinding]{Mods} specifying assumed
#'        dose-response models. This is used in the MCPMod method at the end of 
#'        this study.
#' @param N_total A positive integer value. The total number of subjects.
#' @param N_ini A positive integer vector in which each element is greater than 
#'        or equal to 2. The number of subjects initially assigned to each dose. 
#' @param N_block A positive integer value. The number of subjects allocated
#'        adaptively in each round.
#' @param outcome_type A character value specifying the outcome type. 
#'        Possible values are "continuous" (default), and "binary".
#' @param sd_normal A positive numeric value. The standard deviation of the
#'        observation noise. When `outcome_type` is "continuous", 
#'        `sd_normal` must be specified.
#' @param alpha A positive numeric value. The original significance level.
#'        Default is 0.025.
#' @param n_sim A positive integer value. The number of simulation studies
#'        to calculate the adjusted significance level. Default is 10000.
#' @param seed An integer value. Random seed for data generation in the simulation
#'        studies.
#'        
#' @returns A positive numeric value specifying adjusted significance level.
#' 
#' @examples
#' # Simulation-based adjustment of the significance level using `allocation_rule`
#' \dontrun{
#' adjusted_alpha <- adjust_significance_level(
#'   allocation_rule, models,
#'   N_total = 150, N_ini = rep(10, 5), N_block = 10,
#'   outcome_type = "continuous", sd_normal = sqrt(4.5),
#'   alpha = 0.025, n_sim = 10000, seed = 123
#' )
#' }
#'
#' @importFrom stats quantile
#'
#' @export
adjust_significance_level <- function(
    allocation_rule, models,
    N_total, N_ini, N_block, 
    outcome_type = c("continuous", "binary"), sd_normal = NULL,
    alpha = 0.025, n_sim = 10000L, seed = NULL) {
  
  outcome_type <- match.arg(outcome_type)
  
  if (is.null(seed)) {
    seed <- 0
  }
  
  doses <- attr(models, "doses")
  K <- length(doses)
  true_response <- rep(attr(models, "placEff"), K)
  
  p_values <- sapply(seq_len(n_sim), function(simID) {
    res <- simulate_one_trial(
      allocation_rule, models, 
      true_response = true_response,
      N_total = N_total, N_ini = N_ini, N_block = N_block, 
      Delta = NULL, outcome_type = outcome_type, sd_normal = sd_normal,
      alpha = alpha, selModel = NULL, seed = simID + seed, eval_type = "pVal")
    res$min_p_value
  })
  adjusted <- unname(quantile(p_values, prob = alpha))
  adjusted <- min(adjusted, alpha)
  adjusted
}
