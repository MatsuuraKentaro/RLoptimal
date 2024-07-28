#' Build an Optimal Adaptive Allocation Rule using Reinforcement Learning
#'
#' @param models An object of class \link[DoseFinding]{Mods} specifying assumed
#'        dose-response models.
#' @param N_total A positive integer value. The total number of subjects.
#' @param N_ini A positive integer value or vector. The number of subjects
#'        initially allocated equally among dose groups or the number of subjects
#'        initially assigned to each dose.
#' @param N_block A positive integer value. The number of subjects allocated
#'        adaptively in each round. This may differ from the actual clinical trials.
#' @param Delta A positive numeric value. The clinically relevant target effect.
#'        See \link[DoseFinding]{TD} for details.
#' @param sd_normal A positive numeric value. The standard deviation of the
#'        observation noise.
#' @param optimization_metric A character value specifying the metric to
#'        optimize. Possible values are "MAE" (default), "TD", "power", or "MS".
#'        See Section 2.2 of the original paper for details.
#' @param rl_models An object of class \link[DoseFinding]{Mods}. True dose-response
#'        models in simulations for reinforcement learning. The default is the
#'        same as the 'models' argument. Empirically, employing a wide variety of
#'        models tends to improve performance.
#' @param rl_models_prior A positive numeric vector. The probability or weight
#'        with which each model in rl_models is selected as the true model in
#'        the simulation. The default is NULL, which specifies equal probability
#'        for each model.
#' @param rl_seed An integer value. Random seed for reinforcement learning.
#' @param rl_config A list. Other settings for reinforcement learning. See
#'        \link{rl_config} for details.
#' @param alpha A positive numeric value. The significance level. Default is 0.025.
#' @param selModel A character value specifying the model selection criterion
#'        for dose estimation. Possible values are "AIC" (default), "maxT", or
#'        "aveAIC". See \link[DoseFinding]{MCPMod} for details.
#' @param Delta_range A numeric vector of length 2. The lower and upper bounds
#'        of Delta where the estimated target dose is correct. Default is
#'        `c(0.9, 1.1) * Delta`.
#' @param output_dir A character value. Directory name or path to store the
#'        built allocation rule. Default is the current datetime.
#' @param output_base_dir A character value. Parent directory path where the
#'        built allocation rule will be stored. Valid only if 'output_dir' does
#'        not contain '/'. Default is "allocation_rules".
#' @param checkpoint_dir A character value. Parent directory path to save
#'        checkpoints. It enables you to resume learning from that point onwards.
#'        Default is "checkpoints".
#' @param save_start_iter,save_every_iter An integer value. Save checkpoints every
#'        'save_every_iter' iterations starting from 'save_start_iter' or later.
#'
#' @returns An \link{AllocationRule} object.
#'
#' @importFrom glue glue
#'
#' @export
learn_allocation_rule <- function(
    models, N_total, N_ini, N_block, Delta, sd_normal,
    # type = c("normal", "general"),
    optimization_metric = c("MAE", "TD", "power", "MS"),
    rl_models = models, rl_models_prior = NULL, rl_seed = NULL,
    rl_config = rl_config(), alpha = 0.025,
    selModel  = c("AIC", "maxT", "aveAIC"), Delta_range = c(0.9, 1.1) * Delta,
    output_dir = format(Sys.time(), "%Y%m%d_%H%M%S"),
    output_base_dir = "allocation_rules", checkpoint_dir = "checkpoints",
    save_start_iter = 300L, save_every_iter = 100L) {

  # -------------------------------------------------------------------------
  # Check arguments ---------------------------------------------------------
  # -------------------------------------------------------------------------
  stopifnot("'models' needs to be of class Mods" = class(models) == "Mods")

  doses <- attr(models, "doses")
  doses <- as.double(doses)
  # Note: 'doses[1] == 0' is guaranteed by DoseFinding::Mods()
  stopifnot("'doses' must have >=2 distinct values" = length(unique(doses)) >= 2L)
  K <- length(doses)

  N_total <- as.integer(N_total)
  N_ini <- as.integer(N_ini)
  N_block <- as.integer(N_block)
  stopifnot(length(N_total) == 1L, N_total > 0)
  stopifnot(length(N_ini) == K, N_ini > 1)
  stopifnot(length(N_block) == 1L, N_block > 0)
  stopifnot((N_total - sum(N_ini)) %% N_block == 0.)

  Delta <- as.double(Delta)
  sd_normal <- as.double(sd_normal)
  stopifnot(length(Delta) == 1L, Delta > 0)
  stopifnot(length(sd_normal) == 1L, sd_normal > 0)

  optimization_metric <- match.arg(optimization_metric)

  # All attributes must match except for 'names', which is in the first position.
  stopifnot(identical(attributes(rl_models)[-1L], attributes(models)[-1L]))
  dr_models_names <- dimnames(DoseFinding::getResp(rl_models))[[2]]

  n_models <- length(dr_models_names)
  if (!is.null(rl_models_prior)) {
    rl_models_prior <- as.double(rl_models_prior)
    rl_models_prior <- rl_models_prior / sum(rl_models_prior)
    stopifnot(length(rl_models_prior) == n_models, rl_models_prior > 0)
  }
  stopifnot(is.null(rl_seed) || length(rl_seed) == 1L)
  if (!is.null(rl_seed)) {
    rl_seed <- as.integer(rl_seed)
  }

  alpha <- as.double(alpha)
  selModel <- match.arg(selModel)
  Delta_range <- as.double(Delta_range)
  stopifnot(length(alpha) == 1L, alpha > 0)
  stopifnot(length(Delta_range) == 2L, Delta_range > 0)
  stopifnot(Delta_range[1] < Delta, Delta < Delta_range[2])

  # TODO
  if (!grepl("/", output_dir)) {
    # If 'output_dir' does not contain '/', it is a directory name.
    output_path <- file.path(output_base_dir, output_dir)
  }
  output_checkpoint_path <- file.path(checkpoint_dir, basename(output_path))

  # -------------------------------------------------------------------------
  # R environment setup code used in MCPModEnv.py ---------------------------
  # -------------------------------------------------------------------------
  setup_code <- generate_setup_code(
    doses = doses, models = models, Delta = Delta, rl_models = rl_models,
    rl_seed = rl_seed, alpha = alpha, selModel = selModel, Delta_range = Delta_range)

  # -------------------------------------------------------------------------
  # Execute reinforcement learning ------------------------------------------
  # -------------------------------------------------------------------------
  reticulate::source_python(system.file("python/RProcess.py", package = "RLoptimal"))
  reticulate::source_python(system.file("python/MCPModEnv.py", package = "RLoptimal"))

  env_config <- list(
    doses = doses, K = K,
    N_total = N_total, N_ini = N_ini, N_block = N_block,
    std_dev = sd_normal,
    dr_models_names = dr_models_names,
    dr_models_weights = rl_models_prior,
    optimization_metric = optimization_metric,
    r_home = file.path(R.home("bin"), "R"),
    r_code_to_setup = deparse(setup_code)
  )

  ppo <- reticulate::import("ray.rllib.algorithms.ppo")

  N_update <- rl_config$iter
  digits <- floor(log10(N_update)) + 1
  num_env_runners <- rl_config$cores
  rl_config[c("iter", "cores")] <- NULL

  config <- ppo$PPOConfig()$
    environment(env = MCPModEnv, env_config = env_config)$
    env_runners(num_env_runners = num_env_runners, num_envs_per_env_runner = 1L)$
    framework(framework = "torch")$
    debugging(seed = rl_seed)  # Note: NULL is converted to None in Python.
  config <- do.call(config$training, rl_config)
  algo <- config$build()

  result <- train_algo(algo, n_start = 1L, N_update,
                       output_path, output_checkpoint_path,
                       save_start_iter, save_every_iter)

  # Create AllocationRule object
  allocation_rule <- AllocationRule$new(dir = output_path)
  info <- list(call = match.call(), iterations = N_update, setup_code = setup_code)
  default_arguments <- formals()
  input <- Map(eval, as.list(info$call)[-1L])
  default_arguments[names(input)] <- input
  input <- default_arguments
  allocation_rule$set_info(info, input, result$episode_data, result$checkpoints)

  allocation_rule
}
