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
#' @param sd_normal A positive numeric value. The standard deviation of the
#'        observation noise.
#' @param alpha A positive numeric value. The original significance level.
#'        Default is 0.025.
#' @param n_sim A positive integer value. The number of simulation studies
#'        to calculate the adjusted significance level. Default is 10000.
#' @param seed An integer value. Random seed for data generation in the simulation
#'        studies.
#'        
#' @returns A positive numeric value specifying adjusted significance level.
#'
#' @export
adjust_significance_level <- function(
    allocation_rule, models,
    N_total, N_ini, N_block, sd_normal, 
    # type = c("normal", "general"),
    alpha = 0.025, n_sim = 10000L, seed = NULL) {
  
  if (is.null(seed)) {
    seed <- 0
  }
  
  doses <- attr(models, "doses")
  K <- length(doses)
  true_response <- rep(attr(models, "placEff"), K)
  
  p_values <- sapply(seq_len(n_sim), function(simID) {
    res <- simulate_one_study(
      allocation_rule, models, "flat", true_response,
      N_total, N_ini, N_block, NULL, sd_normal, alpha, NULL, 
      simID + seed, "pVal")
    res$min_p_value
  })
  adjusted <- unname(quantile(p_values, prob = alpha))
  adjusted
}
