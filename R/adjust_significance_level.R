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
#' @importFrom stats quantile
#'
#' @export
adjust_significance_level <- function(
    allocation_rule, models,
    N_total, N_ini, N_block, 
    outcome_type = c("continuous", "binary"), sd_normal = NULL,
    alpha = 0.025, n_sim = 10000L, seed = NULL) {
  
  # Check arguments ---------------------------------------------------------
  stopifnot("'allocation_rule' needs to be of class AllocationRule" =
              class(allocation_rule) == "AllocationRule")
  stopifnot("'models' needs to be of class Mods" = class(models) == "Mods")
  
  doses <- attr(models, "doses")
  K <- length(doses)
  
  N_total <- as.integer(N_total)
  N_ini <- as.integer(N_ini)
  N_block <- as.integer(N_block)
  stopifnot(length(N_total) == 1L, N_total >= 1L)
  stopifnot(length(N_ini) == K, N_ini >= 2L)
  stopifnot(length(N_block) == 1L, N_block >= 1L)
  stopifnot((N_total - sum(N_ini)) %% N_block == 0.)
  
  outcome_type <- match.arg(outcome_type)
  if (outcome_type == "continuous") {
    stopifnot("sd_normal must be specified when outcome_type = 'continuous'" =
                !is.null(sd_normal))
    sd_normal <- as.double(sd_normal)
    stopifnot(length(sd_normal) == 1L, sd_normal > 0)
  }
  
  alpha <- as.double(alpha)
  stopifnot(length(alpha) == 1L, alpha > 0, alpha < 1)
  n_sim <- as.integer(n_sim)
  stopifnot(length(n_sim) == 1L, n_sim > 0L)
  
  if (is.null(seed)) {
    seed <- 0
  }
  
  # Compute adjusted significance level -------------------------------------
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
